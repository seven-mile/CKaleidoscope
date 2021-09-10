#pragma once

#include "anyex.hpp"
#include "ast.hpp"
#include "errdef.hpp"
#include "global.hpp"
#include "ioagent.hpp"
#include "jit.hpp"
#include "lexer.hpp"
#include "optimizer.hpp"
#include <algorithm>
#include <any>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <stdexcept>

namespace zMile {

inline std::map<std::string, int> binop_prec
{
  {",", 1},
  {"=", 2},
  {"+=", 2}, {"-=", 2}, // += -=
  {"*=", 2}, {"/=", 2}, {"%=", 2}, // *= /= %=
  {"<<=", 2}, {">>=", 2}, // <<= >>=
  {"^=", 2}, {"&=", 2}, // ^= &=

  {"||", 3}, // ||
  {"&&", 4}, // &&
  {"|", 5},
  {"^", 6},
  {"&", 7},
  {"==", 8}, {"!=", 8}, // == and !=
  {"<", 9}, {">", 9},
  {"<=", 9}, {">=", 9}, // <= and >=
  {"<<", 11}, {">>", 11}, // 9 = '(' = '<<', 0 = ')' = '>>'
  {"+", 12}, {"-", 12},
  {"*", 13}, {"/", 13}, {"%", 13},
};

inline std::map<std::string, int> unary_op
{
  {"+", 0}, {"-", 0},
  {"~", 0}, {"!", 0},
  {"*", 0}, {"&", 0}
};

const int unary_precedence = 16;

class Parser {
  Lexer& lex;
  Token cur_tok = { tok_invalid, nullptr, {0,0} };

  template<class err_t = syntax_error>
  inline auto log_err(std::string errinfo) {
    return log_err_with_loc<err_t>(errinfo, cur_tok.loc);
  }

  inline void adv() { cur_tok = lex.get_token(); }
  inline int get_cur_prior() {
    if (cur_tok.type != tok_other || cur_tok.val.type() != typeid(std::string))
      return -1; // always pass

    int res = binop_prec[std::any_cast<std::string>(cur_tok.val)];
    if (res <= 0) return -1;
    return res;
  }

  void expand_comma_expr(expr_t expr, const std::function<void(expr_t)>& func) {
    BinExprNode *ptr = nullptr;
    if (instof<BinExprNode>(*expr) &&
        (ptr = dynamic_cast<BinExprNode*>(expr.get()))->op == ",") {
      
      expand_comma_expr(std::move(ptr->left), func);
      func(std::move(ptr->right));
    }
    else func(std::move(expr));
  }

  // */&... {id} [A][B]...
  std::string parse_id_type(expr_t expr, llvm::Type *&ty) {
    if (auto ptr = dynamic_cast<VarExprNode*>(expr.get()))
      return ptr->get_name();
    
    if (auto ptr = dynamic_cast<UnaryExprNode*>(expr.get())) {
      auto & op = ptr->op;
      if (op == "*" || op == "&") ty = ty->getPointerTo();
      else throw syntax_error("invalid type unary operator definition.");
      return parse_id_type(std::move(ptr->operand), ty);
    }

    if (auto ptr = dynamic_cast<SubScriptExprNode*>(expr.get())) {
      llvm::Constant* idxv = nullptr;

      // todo: deal with some special var situation such as (1, 2, var_a)
      if (auto ptrvar = dynamic_cast<VarExprNode*>(ptr->idx.get())) {
        if (ptrvar->is_global()) {
          idxv = 
            llvm::dyn_cast<llvm::GlobalVariable>(
              g_named_values[ptrvar->get_name()].top())
              ->getInitializer();
        }
      }

      if (!idxv) {
        auto idx_temp = ptr->idx->codegen();
        if (!idx_temp) return nullptr;

        idxv = llvm::dyn_cast<llvm::Constant>(idx_temp);
      }
      
      if (!idxv)
        return log_err("type definition index must be a constant!");
      
      uint64_t idx = 0;
      if (auto idxfp = llvm::dyn_cast<llvm::ConstantFP>(idxv)) {
        idx = idxfp->getValueAPF().convertToDouble();
      } else if (auto idxint = llvm::dyn_cast<llvm::ConstantInt>(idxv)) {
        idx = idxint->getZExtValue();
      } else throw object_invalid("unsupported array index type.");

      if (!idx) return log_err("array index must be constant.");

      ty = llvm::ArrayType::get(ty, idx);
      return parse_id_type(std::move(ptr->arr), ty);
    }

    if (auto ptr = dynamic_cast<ParenExprNode*>(expr.get())) {
      return parse_id_type(std::move(ptr->val), ty);
    }

    throw syntax_error("invalid var id expr type!");
  }

public:
  Parser(Lexer& lex) : lex(lex) {  }

  node_t parse_num() {
    node_t ptr = std::make_unique<NumExprNode>(cur_tok.val, cur_tok.loc);
    return adv(), std::move(ptr);
  }

  node_t parse_char() {
    node_t ptr = std::make_unique<CharExprNode>(
      std::any_cast<char>(cur_tok.val), cur_tok.loc);
    return adv(), std::move(ptr);
  }

  node_t parse_str() {
    node_t ptr = std::make_unique<StringExprNode>(
      std::any_cast<const std::string&>(cur_tok.val),
      cur_tok.loc);
    return adv(), std::move(ptr);
  }

  node_t parse_paren() {
    SourceLoc loc_start = cur_tok.loc;
    adv(); // (
    node_t expr = parse_expr();
    if (!expr) return nullptr;

    if (!cur_tok.is_tool(")"))
      return log_err("unexpected token, expected ')'.");

    adv(); // )
    
    return std::make_unique<ParenExprNode>(std::move(expr), loc_start);
  }

  node_t try_parse_postfix(node_t core) {
    if (!core)
      return log_err<object_invalid>("the core argument is invalid!");
    
    SourceLoc loc_start = cur_tok.loc;

    if (cur_tok.is_tool("["))
    {
      adv(); // '['
      auto idx = parse_expr();
      if (!cur_tok.is_tool("]"))
        return log_err("unexpected token, expected ']'.");
      adv(); // ']'
      core =
          std::make_unique<SubScriptExprNode>(std::move(core), std::move(idx), loc_start);
    }
    else if (cur_tok.is_tool("("))
    {
      adv();
      std::vector<node_t> args;
      auto ptrVarNode = dynamic_cast<VarExprNode*>(core.get());
      if (!ptrVarNode)
        throw syntax_error("function reference calling not supported until now!");

      if (cur_tok.is_tool(")"))
        return (adv(), std::make_unique<CallExprNode>(ptrVarNode->get_name(), std::move(args), loc_start));

      // (((A,B),C),D)
      auto args_expr = parse_expr();

      if (!cur_tok.is_tool(")"))
        log_err("unexpected token, expected ')'.");
      adv();

      expand_comma_expr(std::move(args_expr), [&](expr_t expr){
        args.push_back(std::move(expr));
      });


      return std::make_unique<CallExprNode>(ptrVarNode->get_name(), std::move(args), loc_start);
    }
    // self in/de-crement afterward
    else if (cur_tok.type == tok_addadd || cur_tok.type == tok_subsub) {
      bool is_add = cur_tok.type == tok_addadd;
      adv();
      return std::make_unique<SelfBackCreExprNode>(
        static_unique_ptr_cast<ExprNode>(std::move(core)), is_add, loc_start);
    }

    // todo: member access "." and "->"

    if (cur_tok.is_tool("[") || cur_tok.is_tool("(")
     || cur_tok.type == tok_addadd || cur_tok.type == tok_subsub)
      return try_parse_postfix(std::move(core));

    return std::move(core);
  }

  node_t parse_id() {
    auto cur_id = std::any_cast<const std::string&>(cur_tok.val);
    node_t L = std::make_unique<VarExprNode>(cur_id, cur_tok.loc);
    adv();

    return L;
  }

  expr_t parse_addr() {
    adv(); // '&'
    SourceLoc loc_start = cur_tok.loc;
    expr_t operand = parse_expr(unary_precedence);
    
    return std::make_unique<UnaryExprNode>("&", std::move(operand), loc_start);
  }

  node_t parse_core() {
    if (cur_tok.is_tool(";"))
      return std::make_unique<NullStmt>(cur_tok.loc);
    if (cur_tok.type == tok_id)
      return parse_id();
    if (cur_tok.type == tok_lit_num)
      return parse_num();
    if (cur_tok.type == tok_lit_char)
      return parse_char();
    if (cur_tok.type == tok_lit_string)
      return parse_str();
    if (cur_tok.type == tok_kw_true) {
      SourceLoc loc_start = cur_tok.loc;
      return adv(), std::make_unique<BoolExprNode>(true, loc_start);
    } if (cur_tok.type == tok_kw_false) {
      SourceLoc loc_start = cur_tok.loc;
      return adv(), std::make_unique<BoolExprNode>(false, loc_start);
    } if (cur_tok.type == tok_kw_if)
      return parse_if();
    if (cur_tok.type == tok_kw_for)
      return parse_for();
    if (cur_tok.type == tok_kw_while)
      return parse_while();
    if (cur_tok.type == tok_kw_return)
      return parse_return();
    if (cur_tok.type == tok_kw_break) {
      SourceLoc loc_start = cur_tok.loc;
      return adv(), std::make_unique<LoopJumpStmtNode>(true, loc_start);
    } if (cur_tok.type == tok_kw_continue) {
      SourceLoc loc_start = cur_tok.loc;
      return adv(), std::make_unique<LoopJumpStmtNode>(false, loc_start);
    } if (cur_tok.type == tok_addadd || cur_tok.type == tok_subsub) {
      bool is_add = cur_tok.type == tok_addadd;
      SourceLoc loc_start = cur_tok.loc;
      adv();
      auto left = parse_expr(unary_precedence);
      return std::make_unique<SelfFrontCreExprNode>(std::move(left), is_add, loc_start);
    }
    if (cur_tok.is_type() || cur_tok.is_spec())
      return parse_dvar();
    if (cur_tok.is_tool("("))
      return parse_paren();
    if (cur_tok.is_tool("{"))
      return parse_block();
    
    // unary
    if (cur_tok.is_tool("-")) {
      SourceLoc loc_start = cur_tok.loc;
      return adv(), std::make_unique<BinExprNode>("*", std::make_unique<NumExprNode>(-1, loc_start), parse_expr(unary_precedence), loc_start);
    }
    if (cur_tok.type == tok_other
        && cur_tok.val.type() == typeid(std::string)
        && unary_op.find(std::any_cast<const std::string&>(cur_tok.val))
            != unary_op.end()) {
      auto un_op = std::any_cast<const std::string&>(cur_tok.val);
      SourceLoc loc_start = cur_tok.loc;
      return adv(), std::make_unique<UnaryExprNode>(un_op, parse_expr(unary_precedence), loc_start);
    }
    // if (cur_tok.is_tool("&"))
    //   return parse_addr();
    // if (cur_tok.is_tool("*"))
    //   return adv(), std::make_unique<UnaryExprNode>("*", parse_expr(unary_precedence));
    // if (cur_tok.is_tool("!"))
    //   return adv(), std::make_unique<UnaryExprNode>("!", parse_expr(unary_precedence));
    // if (cur_tok.is_tool("~"))
    //   return adv(), std::make_unique<UnaryExprNode>("~", parse_expr(unary_precedence));
    // if (cur_tok.is_tool("+"))
    //   return adv(), parse_expr(unary_precedence);
    
    return log_err("identify expr token failed.");
  }

  node_t parse_prim() {
    auto core = parse_core();
    return try_parse_postfix(std::move(core));
  }

  // parse code blocks, which contains several stmt
  // and owns name scope.
  // r_b = 0 ==> normal (if no brace, only parse one stmt)
  // r_b = 1 ==> (for function) brace is necessary
  block_t parse_block(bool require_brace = false) {
    std::vector<stmt_t> v;
    SourceLoc loc_start = cur_tok.loc;
    if (cur_tok.is_tool("{")) {
      adv();
      while (1) {
        if (cur_tok.type == tok_other
            && cur_tok.is_tool("}"))
          { adv(); break; }
        v.push_back(parse_stmt());
      }
    } else {
      if (require_brace) return log_err(
            "unexpected token, a left brace is expected for the start of the block.");
      v.push_back(parse_stmt());
    }
    return std::make_unique<BlockNode>(std::move(v), loc_start);
  }

  // lhs binop(pr) rest_section
  //     ^ cur_pos
  node_t parse_binrhs(int oppr, node_t lhs) {
    std::vector<std::pair<std::string, expr_t>> layer;

    while (1) {
      SourceLoc loc_start = cur_tok.loc;
      int pr = get_cur_prior();

      if (pr < oppr)
        return lhs;

      bool is_right_asso = pr == 2 || pr == 8 || pr  == 9;

      // it must be a binop
      auto op = std::any_cast<const std::string &>(cur_tok.val);
      adv();
      node_t rhs = parse_prim();
      if (!rhs) return nullptr;

      // get a binop again(next)
      int nxpr = get_cur_prior();

      // posmap: lhs op(pr) rhs nxop(nxpr) [unparsed]
      //                        ^ cur_pos

      if (is_right_asso ? pr <= nxpr : pr < nxpr) {
        // next layer, e.g. a + b + c*d*e + f*g
        //                         ( ^   )
        rhs = parse_binrhs(pr + !is_right_asso, std::move(rhs));
        if (!rhs) return nullptr;
      }
      lhs = std::make_unique<BinExprNode>(op, std::move(lhs), std::move(rhs), loc_start);
    }
  }

  // def ret_type func_name(func_arg1, func_arg2, ...)
  proto_t parse_proto() {
    if (!cur_tok.is_type())
      return log_err("unexpected token, expected a type name.");
    auto rtn_ty = cur_tok.type;

    SourceLoc loc_start = cur_tok.loc;
    adv();
    if (cur_tok.type != tok_id)
      return log_err("unexpected token, expected an identifier.");
    auto id = std::any_cast<const std::string&>(cur_tok.val);
    
    adv();
    if (!cur_tok.is_tool("("))
      return log_err("unexpected token, expected '('.");
    
    adv();
    std::vector<Argu> args;
    bool varargs = false;

    if (!cur_tok.is_tool(")"))
      while(1) {
        if (cur_tok.type == tok_varargs) {
          varargs = true;
          adv();
          if (!cur_tok.is_tool(")"))
            return log_err("unexpected token, the prototype should end after var args.");
          break;
        }

        if (!cur_tok.is_type())
          return log_err("unexpected token, expected a type name.");
        auto arg_ty = get_type_of_tok(cur_tok.type);
        adv();

        auto prm = parse_prim();
        prm->output(std::cerr);
        
        auto name = this->parse_id_type(std::move(prm), arg_ty);
        args.emplace_back(name, arg_ty);
        
        if (cur_tok.is_tool(")")) break;
        if (!cur_tok.is_tool(","))
          return log_err("unexpected token, expected ',' or ')'.");
        adv();
      }
    adv();
    return std::make_unique<ProtoNode>(id, rtn_ty, args, varargs, loc_start);
  }

  // extern func_prototype
  proto_t parse_extern() {
    if (cur_tok.type != tok_kw_extern)
      return log_err("unexpected token, expected the keyword extern.");
    return adv(), parse_proto();
  }

  // def func_prototype: func_body_expr
  // do not forget the ':' tool
  func_t parse_def() {
    if (cur_tok.type != tok_kw_def)
      return log_err("unexpected token, expected the keyword def.");

    SourceLoc loc_start = cur_tok.loc;
    adv(); // func_prototype
    proto_t proto = parse_proto();
    if (!proto) return nullptr;

    auto body = parse_block(true);
    if (!body) return nullptr;

    return std::make_unique<FuncNode>(std::move(proto), std::move(body), loc_start);
  }

  node_t parse_if() {
    if (cur_tok.type != tok_kw_if)
      return log_err("unexpected token, expected the keyword if.");

    SourceLoc loc_start = cur_tok.loc;
    adv(); // cond

    if (cur_tok.type != tok_other || !cur_tok.is_tool("("))
      return log_err("unexpected token, expected the token '('.");
    adv(); // '('

    node_t cond; block_t then, els;
    cond = parse_expr();
    if (!cond) return nullptr;

    if (cur_tok.type != tok_other || !cur_tok.is_tool(")"))
      return log_err("unexpected token, expected the token ')'.");
    adv(); // ')'
    
    then = parse_block();
    if (!then) return nullptr;

    if (cur_tok.type == tok_kw_else) {
      adv(); // else
      els  = parse_block();
      if (!els ) return nullptr;
    } else els = nullptr;

    return std::make_unique<IfStmtNode>(std::move(cond), std::move(then), std::move(els), loc_start);
  }

  node_t parse_for() {
    SourceLoc loc_start = cur_tok.loc;
    adv(); // for
    if (!cur_tok.is_tool("("))
      log_err("unexpected token, expected the token '('.");
    
    adv(); // (
    auto start = parse_stmt();
    if (!start) return nullptr;

    auto cond = parse_expr();
    if (!cond) return nullptr;

    // for (...; ; ...) -> for (...; true; ...)
    if (instof<NullStmt*>(cond.get()))
      cond = std::make_unique<BoolExprNode>(true, cur_tok.loc);

    if (!cur_tok.is_tool(";"))
      log_err("unexpected token, expected the token ';'.");
    
    adv(); // ;

    stmt_t every = cur_tok.is_tool(")") ? std::make_unique<NullStmt>(cur_tok.loc) : parse_stmt();
    if (!every) return nullptr;

    if (!cur_tok.is_tool(")"))
      log_err("unexpected token, expected the token ')'.");    
    
    adv();
    auto body = parse_block();
    if (!body) return nullptr;

    return std::make_unique<ForStmtNode>(std::move(start),
           std::move(cond), std::move(every), std::move(body), loc_start);
  }

  node_t parse_while() {
    SourceLoc loc_start = cur_tok.loc;
    adv(); // while
    if (!cur_tok.is_tool("("))
      log_err("unexpected token, expected the token '('.");
    
    adv(); // (

    auto cond = parse_expr();
    if (!cond) return nullptr;

    // while (;)
    if (instof<NullStmt*>(cond.get())) {
      return log_err("while condition expression can't be empty");
    }

    if (!cur_tok.is_tool(")"))
      log_err("unexpected token, expected the token ')'.");    
    
    adv(); // )
    auto body = parse_block();
    if (!body) return nullptr;

    return std::make_unique<ForStmtNode>(std::make_unique<NullStmt>(SourceLoc{-1,-1}),
           std::move(cond), std::make_unique<NullStmt>(SourceLoc{-1,-1}), std::move(body), loc_start);
  }

  stmt_t parse_return() {
    SourceLoc loc_start = cur_tok.loc;
    adv(); // return

    auto V = parse_expr();
    if (cur_tok.type == tok_other
      && cur_tok.is_tool(";"))
      adv();
    
    return std::make_unique<RetStmtNode>(std::move(V), loc_start);
  }

  stmt_t parse_dvar() {
    SourceLoc loc_start = cur_tok.loc;
    bool is_const = false;
    if (cur_tok.type == tok_kw_const) is_const = true, adv();

    auto ty = get_type_of_tok(cur_tok.type);
    adv();

    std::vector<std::pair<Var, expr_t>> lst;

    auto decls = parse_expr();

    if (!cur_tok.is_tool(";"))
      return log_err("unexpected token, expected a ';'.");
    
    expand_comma_expr(std::move(decls), [this, ty, &lst](expr_t expr) {
      std::pair<Var, expr_t> ele{{"", nullptr}, nullptr};
      if (auto ptr = dynamic_cast<BinExprNode*>(expr.get())) {
        if (ptr->op != "=")
          throw syntax_error("invalid variable declaration syntax! expected '='.");
        
        ele.second = std::move(ptr->right);
        expr = std::move(ptr->left);
      }
      ele.first.type = ty;
      ele.first.name = this->parse_id_type(std::move(expr), ele.first.type);
      lst.push_back(std::move(ele));
    });

    return std::make_unique<VarDeclNode>(lst, is_const, loc_start);
  }

  // global scope (naked) codes, considered as a function
  func_t parse_toplevel() {
    std::vector<stmt_t> v;
    v.push_back(parse_stmt());
    
    // autoly assign a prototype
    proto_t proto = std::make_unique<ProtoNode>(
      "__top_level__", tok_kw_number,
      std::vector<Argu>(), false,
      SourceLoc{-1,-1}
    );
    return std::make_unique<FuncNode>(
      std::move(proto),
      std::make_unique<BlockNode>(std::move(v), SourceLoc{-1,-1}),
      SourceLoc{-1,-1}
    );
  }

  // a calculatable expression
  node_t parse_expr(int oppr = 0) {
    node_t lhs = parse_prim();
    if (!lhs) return nullptr;

    // equivalent model, root binary node_t is 0
    return parse_binrhs(oppr, std::move(lhs));
  }

  stmt_t parse_stmt() {
    stmt_t s = parse_expr();
    // stmt must end with token ';'
    if (cur_tok.is_tool(";")) adv();

    return s;
  }
  
  // stream power up!
  void read_stream(bool cmd = true, std::string prom = "Test> ") {
    while (1) {
      if (cur_tok.type == tok_invalid) {
        if (cmd) std::cerr << prom;
        adv();
      }
      if (cur_tok.is_type() || cur_tok.is_spec()) {
        auto gvar = parse_dvar();
        if (gvar) gvar->codegen();
        continue;
      }
      switch (cur_tok.type)
      {
      case tok_eof: return void(std::cerr << "EOF read, exiting." << std::endl);
      case tok_invalid: continue;
      case tok_kw_def:
      {
        auto def = parse_def();
        if (!def) { adv(); break; }

        if (def->get_name() == "main") {
          // return value default zero
          if (def->get_proto()->get_return_type() != tok_kw_int)
            throw syntax_error("the return type of the main function should be int32.");

          def->get_body()->get_list().push_back(
            std::make_unique<RetStmtNode>(std::make_unique<NumExprNode>(0, SourceLoc{-1,-1}), SourceLoc{-1,-1}));
        }
        
        if (!def->codegen()) return;

        if (cmd) {
          auto RT = zMile::g_jit->getMainJITDylib().createResourceTracker();
          auto TSM = llvm::orc::ThreadSafeModule(
            std::move(zMile::g_module), std::move(zMile::g_context));
          g_exit_err(g_jit->addModule(std::move(TSM), RT));
          init_module_and_pass_mgr();
        }
      }
      break;
      case tok_kw_extern:
      {
        auto ext = parse_extern();
        if (!ext) { adv(); break; }

        if (ext->codegen())
          g_protos[ext->get_name()] = std::move(ext);
      }
      break;
      case tok_kw_if:
      {
        auto ife = parse_if();
        if (!ife) { adv(); break; }

        ife->codegen();
      }
      break;
      case tok_other:
        if (cur_tok.is_tool(";")) { adv(); break; }
      default:
      {
        auto top = parse_toplevel();
        if (!top) { adv(); break; }
        std::cerr << "Toplevel Function: ";
        top->output();
        std::cerr << std::endl;

        // if (auto tcode = top->codegen()) {
        //   auto hmodule = g_jit->addModule(std::move(g_module));
        //   init_module_and_pass_mgr();

        //   auto expr_sym = g_jit->findSymbol("__top_level__");
        //   if (!expr_sym) log_err("jit error: function not found.");

        //   auto res = ((double(*)())(intptr_t)cantFail(expr_sym.getAddress()))();
        //   std::cerr << "\nEvaluated to " << res << std::endl;

        //   g_jit->removeModule(hmodule);
        // }
      }
      break;
      }
    }
  }

};

}