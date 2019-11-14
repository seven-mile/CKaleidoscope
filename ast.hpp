#pragma once

#include <iostream>
#include <memory>
#include <vector>

namespace zMile {

class ExprNode {
public:
  virtual ~ExprNode() {  }
  virtual void* eval() {  }
};

class NumberExprNode : ExprNode {
  double val;

public:
  NumberExprNode(double val) : val(val) {  }
};

class VariableExprNode : ExprNode {
  std::string id;

public:
  VariableExprNode(std::string id) : id(id) {  }
};

class BinaryExprNode : ExprNode {
  char op;
  std::unique_ptr<ExprNode> left, right;
public:
  BinaryExprNode(char op, std::unique_ptr<ExprNode> left,
    std::unique_ptr<ExprNode> right) : op(op),
    left(std::move(left)), right(std::move(right)) {  }
};

class CallExprNode : ExprNode {
  std::string func;
  std::vector<std::unique_ptr<ExprNode>> args;

public:
  CallExprNode(const std::string &func, std::vector<std::unique_ptr<ExprNode>> args)
    : func(func), args(std::move(args)) {  }
};

// classes for functions

class PrototypeExprNode : ExprNode {
  std::string id;
  std::vector<std::string> args;
public:
  PrototypeExprNode(std::string id, std::vector<std::string> args)
    : id(id), args(std::move(args)) {  }
};

class FunctionExprNode : ExprNode {
  std::unique_ptr<PrototypeExprNode> proto;
  std::unique_ptr<ExprNode> body; // what it will be execused.
public:
  FunctionExprNode(std::unique_ptr<PrototypeExprNode> proto,
    std::unique_ptr<ExprNode> body) : proto(std::move(proto)),
      body(std::move(body)) {  }
};

}
