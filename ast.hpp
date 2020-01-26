#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <vector>

#include "lexer.hpp"

namespace zMile {

inline auto log_err(const char* errinfo) {
  throw syntax_error(errinfo);
  return nullptr;
}

class Outputable {
public:
  virtual void output(std::ostream & os = std::cerr) = 0;
};

class ExprNode : public Outputable {
public:
  virtual ~ExprNode() {  }
};

using expr_t = std::unique_ptr<ExprNode>;

class NumExprNode : public ExprNode {
  double val;

public:
  NumExprNode(double val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ NumExpr, \"val\": " << val << " }";
  }
};

class CharExprNode : public ExprNode {
  char val;

public:
  CharExprNode(char val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ CharExpr, \"val\": '" << val << "' }";
  }
};

class StringExprNode : public ExprNode {
  std::string val;

public:
  StringExprNode(const std::string& val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ StringExpr, \"val\": \"" << val << "\" }";
  }
};

class VarExprNode : public ExprNode {
  std::string id;

public:
  VarExprNode(const std::string &id) : id(id) {  }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ VarExpr, \"id\": \"" << id << "\" }";
  }
};

class BinExprNode : public ExprNode {
  char op;
  expr_t left, right;
public:
  BinExprNode(char op, expr_t left,
    expr_t right) : op(op),
    left(std::move(left)), right(std::move(right)) {  }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ BinExpr, \"operator\": '" << op << "', \"left\": ";
    left->output();
    os << ", \"right\": ";
    right->output();
    os << " }";
  }
};

class CallExprNode : public ExprNode {
  std::string func;
  std::vector<expr_t> args;

public:
  CallExprNode(const std::string &func, std::vector<expr_t> args)
    : func(func), args(std::move(args)) {  }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ CallExpr, \"func_name\": \"" << func << "\", \"args\":[ ";
    for (size_t idx = 0; idx < args.size()-1; idx++)
      args[idx]->output(), os << ", ";
    args.back()->output();
    os << " ] }";
  }
};

// classes for functions

class ProtoNode : public Outputable {
  std::string id;
  std::vector<std::string> args;
public:
  ProtoNode(std::string id, std::vector<std::string> args)
    : id(id), args(std::move(args)) {  }
  
  // { Prototype, "id": "cos", "args": ["theta"] }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ Prototype, \"id\": \"" << id << "\"";
    if (args.size()) {
      os << ", \"args\": [ ";

      for (size_t idx = 0; idx < args.size()-1; idx++)
        os << '"' << args[idx] << "\", ";
      os << '"' << args.back() << "\" ]";
    }
    os << " }";
  }
};

using proto_t = std::unique_ptr<ProtoNode>;

class FuncNode : public Outputable {
  proto_t proto;
  expr_t body; // what it will be calced.
public:
  FuncNode(proto_t proto,
    expr_t body) : proto(std::move(proto)),
      body(std::move(body)) {  }

  // { Function, { Prototype, $...$ }, { Body, "..." } }
  virtual void output(std::ostream & os = std::cerr) {
    os << "{ Function, \"prototype\": ";
    proto->output(os);
    os << ", \"body\": ";
    body->output();
    os << " }";
  }
};

using func_t = std::unique_ptr<FuncNode>;

}
