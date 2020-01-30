#pragma once

#include "ast.hpp"

namespace zMile {

// non-const
std::map<char, int> map_op_prior
{
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

  expr_t parse_num() {
    expr_t ptr = std::make_unique<NumExprNode>(
      std::any_cast<double>(cur_tok.val));
    return adv(), std::move(ptr);
  }

  expr_t parse_char() {
    expr_t ptr = std::make_unique<CharExprNode>(
      std::any_cast<char>(cur_tok.val));
    return adv(), std::move(ptr);
  }

  expr_t parse_str() {
    expr_t ptr = std::make_unique<StringExprNode>(
      EXT_STR_ANY(cur_tok.val));
    return adv(), std::move(ptr);
  }

  expr_t parse_paren() {
    adv();
    expr_t expr = parse_expr();
    if (!expr) return nullptr;

    if (cur_tok.val.type() == typeid(char) && 
      std::any_cast<char>(cur_tok.val) != ')')
      return log_err("unexpected token, expected ')'.");

    adv();
    
    return expr;
  }

  expr_t parse_id() {
    std::string id = EXT_STR_ANY(cur_tok.val);
    adv();

    if (!any_eq_char(cur_tok.val, '('))
      return std::make_unique<VarExprNode>(id);
    
    adv();
    std::vector<expr_t> args;
    if (!any_eq_char(cur_tok.val, ')'))
      while (1) {
        if (expr_t arg = parse_expr())
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

  expr_t parse_prim(bool hasSign = false) {
    if (cur_tok.type == tok_id)
      return parse_id();
    if (cur_tok.type == tok_lit_num)
      return parse_num();
    if (cur_tok.type == tok_lit_char)
      return parse_char();
    if (cur_tok.type == tok_lit_string)
      return parse_str();
    if (any_eq_char(cur_tok.val, '('))
      return parse_paren();
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

  // lhs binop(pr) rest_section
  //     ^ cur_pos
  expr_t parse_binrhs(int oppr, expr_t lhs) {
    while (1) {
      int pr = get_cur_prior();

      if (pr < oppr) return lhs;

      // it must be a binop
      char op = std::any_cast<char>(cur_tok.val);
      adv();
      expr_t rhs = parse_prim();
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
    if (cur_tok.type != tok_id)
      return log_err("unexpected token, expected an identifier.");
    std::string id = EXT_STR_ANY(cur_tok.val);
    
    adv();
    if (std::any_cast<char>(cur_tok.val) != '(')
      return log_err("unexpected token, expected '('.");
    
    adv();
    std::vector<std::string> args;

    if (!any_eq_char(cur_tok.val, ')'))
      while(1) {
        if (cur_tok.type != tok_id)
          return log_err("unexpected token, expected an arg name.");
        args.push_back(EXT_STR_ANY(cur_tok.val));
        adv();
        if (any_eq_char(cur_tok.val, ')')) break;
        if (!any_eq_char(cur_tok.val, ','))
          return log_err("unexpected token, expected ',' or ')'.");
        adv();
      }
    adv();
    return std::make_unique<ProtoNode>(id, args);
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

    // ':' tool
    if (cur_tok.type != tok_other || !any_eq_char(cur_tok.val, ':'))
      return log_err("unexpected token, expected ':'.");

    adv(); // func_body_expr
    expr_t body = parse_expr();
    if (!body) return nullptr;

    return std::make_unique<FuncNode>(std::move(proto), std::move(body));
  }

  // global scope (naked) codes, considered as a function
  func_t parse_toplevel() {
    expr_t body = parse_expr();
    if (!body) return nullptr;
    
    // autoly assign a prototype
    proto_t proto = std::make_unique<ProtoNode>("__top_level__", std::vector<std::string>());
    return std::make_unique<FuncNode>(std::move(proto), std::move(body));
  }

  // a calculatable expression
  expr_t parse_expr() {
    expr_t lhs = parse_prim();
    if (!lhs) return nullptr;

    // equivalent model, root binary expr_t is 0
    return parse_binrhs(0, std::move(lhs));
  }
  
  // stream power up!
  void read_stream(std::string prom = "Test> ", bool cmd = true) {
    while (1) {
      if (cur_tok.type == tok_invalid) {
        if (cmd) std::cerr << prom;
        adv();
      }
      switch (cur_tok.type)
      {
      case tok_eof: return void(std::cerr << std::endl);
      case tok_invalid: continue;
      case tok_kw_def:
      {
        auto def = parse_def();
        if (!def) { adv(); break; }
        std::cerr << "Function Definition: ";
        def->output();
        std::cerr << std::endl;
      }
      break;
      case tok_kw_extern:
      {
        auto ext = parse_extern();
        if (!ext) { adv(); break; }
        std::cerr << "Extern Function Prototype: ";
        ext->output();
        std::cerr << std::endl;
      }
      break;
      case tok_other:
        if (any_eq_char(cur_tok.val, ';')) { adv(); break; }
      default:
      {
        auto top = parse_toplevel();
        if (!top) { adv(); break; }
        std::cerr << "Toplevel Function: ";
        top->output();
        std::cerr << std::endl;
      }
      break;
      }
    }
  }

};

}