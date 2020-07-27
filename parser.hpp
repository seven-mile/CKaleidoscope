#pragma once

#include "anyex.hpp"
#include "ast.hpp"
#include "global.hpp"
#include "jit.hpp"
#include "lexer.hpp"
#include "optimizer.hpp"
#include <any>
#include <cassert>
#include <cstdint>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace zMile {

// non-const
std::map<char, int> map_op_prior
{
  {'=', 2},
  {'<', 5}, {'>', 5},
  {-'<', 5}, {-'>', 5}, // <= and >=
  {'+', 10}, {'-', 10},
  {'*', 20}, {'/', 20}, {'%', 20},
};

class Parser {
  Lexer& lex;
  Token cur_tok = { tok_invalid, nullptr };

  inline void adv() { cur_tok = lex.get_token(); }
  inline int get_cur_prior() {
    if (cur_tok.type != tok_other || cur_tok.val.type() != typeid(char))
      return -1; // always pass
    
    int res = map_op_prior[std::any_cast<char>(cur_tok.val)];
    if (res <= 0) return -1;
    return res;
  }

public:
  Parser(Lexer& lex) : lex(lex) {  }

  node_t parse_num() {
    node_t ptr = std::make_unique<NumExprNode>(
      std::any_cast<double>(cur_tok.val));
    return adv(), std::move(ptr);
  }

  node_t parse_char() {
    node_t ptr = std::make_unique<CharExprNode>(
      std::any_cast<char>(cur_tok.val));
    return adv(), std::move(ptr);
  }

  node_t parse_str() {
    node_t ptr = std::make_unique<StringExprNode>(
      EXT_STR_ANY(cur_tok.val));
    return adv(), std::move(ptr);
  }

  node_t parse_paren() {
    adv();
    node_t expr = parse_expr();
    if (!expr) return nullptr;

    if (cur_tok.val.type() == typeid(char) && 
      std::any_cast<char>(cur_tok.val) != ')')
      return log_err("unexpected token, expected ')'.");

    adv();
    
    return expr;
  }

  node_t parse_id() {
    std::string id = EXT_STR_ANY(cur_tok.val);
    adv();

    if (!any_eq_char(cur_tok.val, '('))
      return std::make_unique<VarExprNode>(id);
    
    // go on to parse call expr
    adv();
    std::vector<node_t> args;
    if (!any_eq_char(cur_tok.val, ')'))
      while (1) {
        if (node_t arg = parse_expr())
          args.push_back(std::move(arg));
        else return nullptr; // having thrown

        if (any_eq_char(cur_tok.val, ')'))
          break;
        if (!any_eq_char(cur_tok.val, ','))
          throw syntax_error("unexpected token, expected ',' or ')'.");
        
        adv();
      }
    adv();

    return std::make_unique<CallExprNode>(id, std::move(args));
  }

  expr_t parse_addr() {
    adv(); // '&'
    expr_t operand = parse_id();
    if (operand->get_type() != FuncNodeTy &&
        operand->get_type() != VarExprNodeTy)
      return log_err("unexpected token, expected a variable or function name.");
    
    return std::make_unique<AddressExprNode>(std::move(operand));
  }

  node_t parse_prim(bool hasSign = false) {
    if (cur_tok.type == tok_id)
      return parse_id();
    if (cur_tok.type == tok_lit_num)
      return parse_num();
    if (cur_tok.type == tok_lit_char)
      return parse_char();
    if (cur_tok.type == tok_lit_string)
      return parse_str();
    if (cur_tok.type == tok_kw_true) {
      auto ptr = std::make_unique<BoolExprNode>(true);
      return adv(), std::move(ptr);
    }
    if (cur_tok.type == tok_kw_false)  {
      auto ptr = std::make_unique<BoolExprNode>(false);
      return adv(), std::move(ptr);
    }
    if (cur_tok.type == tok_kw_if)
      return parse_if();
    if (cur_tok.type == tok_kw_for)
      return parse_for();
    if (cur_tok.type == tok_kw_return)
      return parse_return();
    if (cur_tok.is_type())
      return parse_dvar();
    if (cur_tok.is_tool('('))
      return parse_paren();
    if (cur_tok.is_tool('{'))
      return parse_block();
    if (cur_tok.is_tool('&'))
      return parse_addr();
    if (!hasSign && cur_tok.type == tok_other) {
      if (any_eq_char(cur_tok.val, '+')) {
        auto expr = (adv(), parse_prim(true));
        return expr;
      }
      if (any_eq_char(cur_tok.val, '-')) {
        auto expr = (adv(), parse_prim(true));
        return std::make_unique<BinExprNode>('*', std::make_unique<NumExprNode>(-1), std::move(expr));
      }
    }
    return log_err("identify expr token failed.");
  }

  // parse code blocks, which contains several stmt
  // and owns name scope.
  // r_b = 0 ==> normal (if no brace, only parse one stmt)
  // r_b = 1 ==> (for function) brace is necessary
  block_t parse_block(bool require_brace = false) {
    std::vector<stmt_t> v;
    if (cur_tok.is_tool('{')) {
      adv();
      while (1) {
        if (cur_tok.type == tok_other
            && any_eq_char(cur_tok.val, '}'))
          { adv(); break; }
        v.push_back(parse_stmt());
      }
    } else {
      if (require_brace) return log_err(
            "unexpected token, a left brace is expected for the start of the block.");
      v.push_back(parse_stmt());
    }
    return std::make_unique<BlockNode>(std::move(v), require_brace);
  }

  // lhs binop(pr) rest_section
  //     ^ cur_pos
  node_t parse_binrhs(int oppr, node_t lhs) {
    while (1) {
      int pr = get_cur_prior();

      if (pr < oppr) return lhs;

      // it must be a binop
      char op = std::any_cast<char>(cur_tok.val);
      adv();
      node_t rhs = parse_prim();
      if (!rhs) return nullptr;

      // get a binop again(next)
      int nxpr = get_cur_prior();

      // posmap: lhs op(pr) rhs nxop(nxpr) [unparsed]
      //                        ^ cur_pos

      if (pr < nxpr) {
        rhs = parse_binrhs(pr+1, std::move(rhs));
        if (!rhs) return nullptr;
      }

      lhs = std::make_unique<BinExprNode>(op, std::move(lhs), std::move(rhs));
    }
  }

  // all functions return a number(double), so the pattern is
  // func_name(func_arg1, func_arg2, ...)
  proto_t parse_proto() {
    if (!cur_tok.is_type())
      return log_err("unexpected token, expected a type name.");
    auto rtn_ty = cur_tok.type;

    adv();
    if (cur_tok.type != tok_id)
      return log_err("unexpected token, expected an identifier.");
    std::string id = EXT_STR_ANY(cur_tok.val);
    
    adv();
    if (std::any_cast<char>(cur_tok.val) != '(')
      return log_err("unexpected token, expected '('.");
    
    adv();
    std::vector<Argu> args;
    bool varargs = false;

    if (!any_eq_char(cur_tok.val, ')'))
      while(1) {
        if (cur_tok.type == tok_varargs) {
          varargs = true;
          adv();
          if (!any_eq_char(cur_tok.val, ')'))
            return log_err("unexpected token, the prototype should end after var args.");
          break;
        }

        if (!cur_tok.is_type())
          return log_err("unexpected token, expected a type name.");
        auto arg_ty = cur_tok.type;
        adv();
        if (cur_tok.type != tok_id)
          return log_err("unexpected token, expected an arg name.");
        args.emplace_back(EXT_STR_ANY(cur_tok.val), arg_ty);
        adv();
        if (any_eq_char(cur_tok.val, ')')) break;
        if (!any_eq_char(cur_tok.val, ','))
          return log_err("unexpected token, expected ',' or ')'.");
        adv();
      }
    adv();
    return std::make_unique<ProtoNode>(id, rtn_ty, args, varargs);
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
    
    adv(); // func_prototype
    proto_t proto = parse_proto();
    if (!proto) return nullptr;

    auto body = parse_block(true);
    if (!body) return nullptr;

    return std::make_unique<FuncNode>(std::move(proto), std::move(body));
  }

  node_t parse_if() {
    if (cur_tok.type != tok_kw_if)
      return log_err("unexpected token, expected the keyword if.");
    adv(); // cond

    if (cur_tok.type != tok_other || !any_eq_char(cur_tok.val, '('))
      return log_err("unexpected token, expected the token '('.");
    adv(); // '('

    node_t cond; block_t then, els;
    cond = parse_expr();
    if (!cond) return nullptr;

    if (cur_tok.type != tok_other || !any_eq_char(cur_tok.val, ')'))
      return log_err("unexpected token, expected the token ')'.");
    adv(); // ')'
    
    then = parse_block();
    if (!then) return nullptr;

    if (cur_tok.type == tok_kw_else) {
      adv(); // else
      els  = parse_block();
      if (!els ) return nullptr;
    } else els = nullptr;

    return std::make_unique<IfStmtNode>(std::move(cond), std::move(then), std::move(els));
  }

  node_t parse_for() {
    adv(); // for
    if (!cur_tok.is_tool('('))
      log_err("unexpected token, expected the token '('");
    
    adv(); // (
    auto start = parse_stmt();
    if (!start) return nullptr;

    auto cond = parse_expr();
    if (!cond) return nullptr;

    if (!cur_tok.is_tool(';'))
      log_err("unexpected token, expected the token ';'");
    
    adv(); // ;
    stmt_t every = parse_stmt();
    if (!every) return nullptr;

    if (!cur_tok.is_tool(')'))
      log_err("unexpected token, expected the token ')'");    
    
    adv();
    auto body = parse_block();
    if (!body) return nullptr;

    return std::make_unique<ForStmtNode>(std::move(start),
           std::move(cond), std::move(every), std::move(body));
  }

  stmt_t parse_return() {
    adv();

    auto V = parse_expr();
    if (cur_tok.type == tok_other
      && any_eq_char(cur_tok.val, ';'))
      adv();
    
    return std::make_unique<RetStmtNode>(std::move(V));
  }

  stmt_t parse_dvar() {
    tag_tok ty = cur_tok.type;
    adv();

    std::vector<std::pair<Var, expr_t>> lst;

    while(1) {
      if (cur_tok.type != tok_id)
        log_err("unexpected token, expected an identifier.");
      Var v = { EXT_STR_ANY(cur_tok.val), ty };

      adv();
      node_t init;
      if (any_eq_char(cur_tok.val, '=')) {
        adv();
        init = parse_expr();
        if (!init) return nullptr;
      } else init = nullptr;
      
      lst.emplace_back(v, std::move(init));
      if (any_eq_char(cur_tok.val, ';'))
        break;
      if (!any_eq_char(cur_tok.val, ','))
        return log_err("unexpected token, expected a ','");
      adv();
    }

    return std::make_unique<VarDeclNode>(lst);
  }

  // global scope (naked) codes, considered as a function
  func_t parse_toplevel() {
    std::vector<stmt_t> v;
    v.push_back(parse_stmt());
    
    // autoly assign a prototype
    proto_t proto = std::make_unique<ProtoNode>(
      "__top_level__", tok_kw_number,
      std::vector<Argu>(), false
    );
    return std::make_unique<FuncNode>(
      std::move(proto),
      std::make_unique<BlockNode>(std::move(v))
    );
  }

  // a calculatable expression
  node_t parse_expr() {
    node_t lhs = parse_prim();
    if (!lhs) return nullptr;

    // equivalent model, root binary node_t is 0
    return parse_binrhs(0, std::move(lhs));
  }

  stmt_t parse_stmt() {
    stmt_t s = parse_expr();
    // stmt must end with token ';'
    if (cur_tok.type == tok_other
      && any_eq_char(cur_tok.val, ';'))
      adv();
    return s;
  }
  
  // stream power up!
  void read_stream(bool cmd = true, std::string prom = "Test> ") {
    while (1) {
      if (cur_tok.type == tok_invalid) {
        if (cmd) std::cerr << prom;
        adv();
      }
      switch (cur_tok.type)
      {
      case tok_eof: return void(std::cerr << "EOF read, exiting." << std::endl);
      case tok_invalid: continue;
      case tok_kw_def:
      {
        auto def = parse_def();
        if (!def) { adv(); break; }

        // def->output();
        
        if (auto tcode = def->codegen())
        if (cmd) {
          g_jit->addModule(std::move(g_module));
          init_module_and_pass_mgr();
        }
      }
      break;
      case tok_kw_extern:
      {
        auto ext = parse_extern();
        if (!ext) { adv(); break; }

        if (auto tcode = ext->codegen())
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
        if (any_eq_char(cur_tok.val, ';')) { adv(); break; }
      default:
      {
        auto top = parse_toplevel();
        if (!top) return adv();
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