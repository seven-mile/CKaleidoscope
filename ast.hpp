#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <ostream>
#include <vector>
#include <functional>

#include <llvm/IR/Constant.h>
#include <llvm/IR/PassManager.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include "global.hpp"
#include "lexer.hpp"
#include "optimizer.hpp"

namespace zMile {

template<class err_t = syntax_error>
inline auto log_err(const char* errinfo) {
  throw err_t(errinfo);
  return nullptr;
}

template <class T>
inline void output_list(const std::vector<T>& v,
                        std::function<void(const T&, std::ostream&)> output,
                        std::ostream & os = std::cerr) {
  if (v.empty()) return;
  os << "[ ";
  for (int idx = 0; idx < v.size() - 1; idx++)
    output(v[idx], os), os << ", ";
  output(v.back(), os);
  os << " ]";
}

inline llvm::Function* get_func(std::string name);

// =========================
// Global Variants
// =========================

class Outputable {
public:
  virtual void output(std::ostream & os = std::cerr) = 0;
};

class ValueGenable {
public:
  virtual llvm::Value* codegen() = 0;
};

class FuncGenable {
public:
  virtual llvm::Function* codegen() = 0;
};

class ExprNode : public Outputable, ValueGenable {
public:
  virtual ~ExprNode() = default;
  virtual llvm::Value* codegen() = 0;
};

using expr_t = std::unique_ptr<ExprNode>;

class NumExprNode : public ExprNode {
  double val;

public:
  NumExprNode(double val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ NumExpr, \"val\": " << val << " }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantFP::get(g_context, llvm::APFloat(val));
  }
};

class CharExprNode : public ExprNode {
  char val;

public:
  CharExprNode(char val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ CharExpr, \"val\": '" << val << "' }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantInt::get(g_context, llvm::APInt(/* nBits: */8, val));
  }
};

class StringExprNode : public ExprNode {
  std::string val;

public:
  StringExprNode(const std::string& val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ StringExpr, \"val\": \"" << val << "\" }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantDataArray::getString(g_context, val);

    // std::vector<llvm::Constant*> vConInt;
    // for (auto ch : val)
    //   vConInt.push_back(llvm::ConstantInt::get(g_context, llvm::APInt(8, ch)));

    // return llvm::ConstantArray::get(
    //   llvm::ArrayType::get(llvm::Type::getInt16Ty(g_context), val.size()),
    //   llvm::ArrayRef(vConInt));
  }
};

class VarExprNode : public ExprNode {
  std::string id;

public:
  VarExprNode(const std::string &id) : id(id) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ VarExpr, \"id\": \"" << id << "\" }";
  }

  virtual llvm::Value* codegen() override {
    auto ret = g_named_values[id];
    if (!ret) return log_err("unknown variable name.");
    return ret;
  }
};

class BinExprNode : public ExprNode {
  char op;
  expr_t left, right;

public:
  BinExprNode(char op, expr_t left,
    expr_t right) : op(op),
    left(std::move(left)), right(std::move(right)) {  }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ BinExpr, \"operator\": '" << op << "', \"left\": ";
    left->output();
    os << ", \"right\": ";
    right->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto L = left->codegen(), R = right->codegen();
    if (!L || !R) return nullptr;
    
    switch (op)
    {
    case '+':
      return g_builder.CreateFAdd(L, R, "add");
    case '-':
      return g_builder.CreateFSub(L, R, "sub");
    case '*':
      return g_builder.CreateFMul(L, R, "mul");
    case '/':
      return g_builder.CreateFDiv(L, R, "div");
    case '<':
      L = g_builder.CreateFCmpULT(L, R, "cmp");
      return g_builder.CreateUIToFP(L, llvm::Type::getDoubleTy(g_context), "bool");
    default:
      return log_err("invalid binary operator.");
      break;
    }
  }
};

class CallExprNode : public ExprNode {
  std::string func;
  std::vector<expr_t> args;

public:
  CallExprNode(const std::string &func, std::vector<expr_t> args)
    : func(func), args(std::move(args)) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ CallExpr, \"func_name\": \"" << func << "\", \"args\": ";
    output_list<expr_t>(args, [](auto &x, auto &os){ x->output(os); });
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    llvm::Function *l_fun = get_func(func);
    if (!l_fun) return log_err("unknown function reference.");
    
    if (args.size() != l_fun->arg_size()) return log_err("incorrect # arguments passed.");

    std::vector<llvm::Value*> lv_args;
    for (size_t i = 0, e = args.size(); i != e; i++) {
      lv_args.push_back(args[i]->codegen());
      if (!lv_args.back()) return nullptr;
    }

    return g_builder.CreateCall(l_fun, lv_args, "call");
  }
};

class IfExprNode : public ExprNode {
  expr_t cond, then, els;

public:
  IfExprNode(expr_t cond, expr_t then, expr_t els)
    : cond(std::move(cond)), then(std::move(then)), els(std::move(els)) {}
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ IfExpr, \"cond\": ";
    cond->output();
    os << ", \"then\": ";
    then->output();
    os << ", \"else\": ";
    els->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto vl_cond = cond->codegen();
    vl_cond = g_builder.CreateFCmpONE(
      vl_cond,
      llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), 0),
      "ifcond");
    
    // func is the current block's parent, not absolutely really a function.
    auto func = g_builder.GetInsertBlock()->getParent();
    
    // bl_then is binded with func
    auto bl_then = llvm::BasicBlock::Create(g_context, "then", func),
         bl_else = llvm::BasicBlock::Create(g_context, "else"),
         bl_merg = llvm::BasicBlock::Create(g_context, "if");
    
    g_builder.CreateCondBr(vl_cond, bl_then, bl_else);
    // set the newly created block before bl_then
    g_builder.SetInsertPoint(bl_then);
    
    auto vl_then = then->codegen();
    if (!vl_then) return nullptr;

    g_builder.CreateBr(bl_merg);
    bl_then = g_builder.GetInsertBlock();

    return nullptr;
  }
};

// classes for functions

class ProtoNode : public Outputable, FuncGenable {
  std::string id;
  std::vector<std::string> args;
public:
  ProtoNode(std::string id, std::vector<std::string> args)
    : id(id), args(std::move(args)) {  }
  
  // { Prototype, "id": "cos", "args": ["theta"] }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ Prototype, \"id\": \"" << id << "\"";
    if (args.size()) {
      os << ", \"args\": ";
      output_list<std::string>(args, [](auto &x, auto &os){ os << '"' << x << '"'; });
    }
    os << " }";
  }

  std::string get_name() { return id; }

  virtual llvm::Function* codegen() override {
    std::vector<llvm::Type*> vArgsTy;
    for (const auto& x:args)
      vArgsTy.push_back(llvm::Type::getDoubleTy(g_context));
    
    auto l_fun_ty = llvm::FunctionType::get(llvm::Type::getDoubleTy(g_context), vArgsTy, false);
    auto l_fun = llvm::Function::Create(l_fun_ty, llvm::Function::ExternalLinkage, id, g_module.get());
    
    unsigned idx = 0;
    for (auto& arg: l_fun->args())
      arg.setName(args[idx]);

    return l_fun;
  }
};

using proto_t = std::unique_ptr<ProtoNode>;

class FuncNode : public Outputable, FuncGenable {
  proto_t proto;
  expr_t body; // what it will be calced.
public:
  FuncNode(proto_t proto,
    expr_t body) : proto(std::move(proto)),
      body(std::move(body)) {  }

  // { Function, { Prototype, $...$ }, { Body, "..." } }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ Function, \"prototype\": ";
    proto->output(os);
    os << ", \"body\": ";
    body->output();
    os << " }";
  }

  virtual llvm::Function* codegen() override {
    // ownership transfered, we need a reference
    auto &rpr = *proto;
    g_protos[proto->get_name()] = std::move(proto);
    // second
    auto fun = get_func(rpr.get_name());

    // first
    if (!fun) return nullptr;

    // error handling
    if (!fun) return nullptr;

    if (!fun->empty())
      return log_err("function cannot be redefined.");

    auto bas_blo = llvm::BasicBlock::Create(g_context, "entry", fun);
    g_builder.SetInsertPoint(bas_blo);

    g_named_values.clear();
    for (auto& arg: fun->args())
      g_named_values[arg.getName()] = &arg;

    if (auto ret_val = body->codegen()) {
      g_builder.CreateRet(ret_val);
      llvm::verifyFunction(*fun);

      // optimize it
      g_fpm->run(*fun);

      return fun;
    }

    fun->eraseFromParent();
    return nullptr;
  }
};

using func_t = std::unique_ptr<FuncNode>;

inline llvm::Function* get_func(std::string name) {
  if (auto *res = g_module->getFunction(name))
    return res;
  
  auto res = g_protos.find(name);
  if (res != g_protos.end())
    return res->second->codegen();
  
  return nullptr;
}

}
