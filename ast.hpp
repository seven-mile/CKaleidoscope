#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <ostream>
#include <vector>
#include <functional>

#include <llvm/IR/Instructions.h>
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

// print "\n" as "\\n"
inline std::string get_raw_string(std::string str) {
  for (auto it = str.begin(); it < str.end(); it++)
    switch (*it) {
      case '\n': str.replace(it, it+1, "\\n"); break;
      case '\r': str.replace(it, it+1, "\\r"); break;
      case '\t': str.replace(it, it+1, "\\t"); break;
      default: break;
    }
  return str;
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
    // we need to display raw string
    os << "{ StringExpr, \"val\": \"" << get_raw_string(val) << "\" }";
  }
  virtual llvm::Value* codegen() override {
    return g_builder.CreateGlobalStringPtr(val);
  }
};

class BoolExprNode : public ExprNode {
  bool val;

public:
  BoolExprNode(bool val) : val(val) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ BoolExpr, \"val\": " << (val ? "true" : "false") << " }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantInt::get(g_context, llvm::APInt(1, val));
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
    case '%':
      return g_builder.CreateFRem(L, R, "mod");
    case '<':
      return g_builder.CreateFCmpULT(L, R, "lt");
    case '>':
      return g_builder.CreateFCmpUGT(L, R, "gt");
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
    : cond(std::move(cond)), then(std::move(then)), els(std::move(els)) {  }
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
    if (vl_cond->getType()->getTypeID() != llvm::Type::getInt1Ty(g_context)->getTypeID())
    vl_cond = g_builder.CreateFCmpONE(
      vl_cond,
      llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), 0),
      "ifcond");
    
    // func is the current block's parent, not absolutely really a function.
    auto func = g_builder.GetInsertBlock()->getParent();
    
    // bl_then is binded with func, so you needn't use func.gBBL().pb() for it
    auto bl_then = llvm::BasicBlock::Create(g_context, "then", func),
         bl_else = llvm::BasicBlock::Create(g_context, "else"),
         bl_merg = llvm::BasicBlock::Create(g_context, "afterif");
    
    g_builder.CreateCondBr(vl_cond, bl_then, bl_else);
    // pour the newly created instructions into bl_then
    g_builder.SetInsertPoint(bl_then);
    
    // create instructions
    auto vl_then = then->codegen();
    if (!vl_then) return nullptr;

    // skip else, br to merg
    g_builder.CreateBr(bl_merg);
    bl_then = g_builder.GetInsertBlock();

    // insert the else block
    func->getBasicBlockList().push_back(bl_else);
    g_builder.SetInsertPoint(bl_else);

    llvm::Value* vl_else = els->codegen();
    if (!vl_else) return nullptr;

    // Also
    g_builder.CreateBr(bl_merg);
    bl_else = g_builder.GetInsertBlock();

    func->getBasicBlockList().push_back(bl_merg);
    g_builder.SetInsertPoint(bl_merg);

    llvm::PHINode* phi = g_builder.CreatePHI(llvm::Type::getDoubleTy(g_context), 2, "iftmp");

    // Judging by which block you come from, return value
    phi->addIncoming(vl_then, bl_then);
    phi->addIncoming(vl_else, bl_else);

    return phi;
  }
};

class ForExprNode : public ExprNode {
  std::string var;
  expr_t start, end, incr, body;
public:
  ForExprNode(std::string var, expr_t start, expr_t end, expr_t incr, expr_t body) :
    var(var), start(std::move(start)), end(std::move(end)),
    incr(std::move(incr)), body(std::move(body)) {
      if (!incr) this->incr = std::make_unique<NumExprNode>(1);
    }
  
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ ForExpr, \"var\": \"" << var << "\"";
    os << ", \"start\": ";
    start->output();
    os << ", \"end\": ";
    end->output();
    os << ", \"incr\": ";
    incr->output();
    os << ", \"body\": ";
    body->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    // g_builder.CreateIntToPtr()
    auto vl_start = start->codegen();
    if (!vl_start) return nullptr;
    
    auto func = g_builder.GetInsertBlock()->getParent();

    // bl_now is just for PHINode's judgement
    auto bl_now   = g_builder.GetInsertBlock(),
         bl_loop  = llvm::BasicBlock::Create(g_context, "loop", func);

    g_builder.CreateBr(bl_loop);
    g_builder.SetInsertPoint(bl_loop);

    llvm::PHINode* phi = g_builder.CreatePHI(llvm::Type::getDoubleTy(g_context), 2, var);
    phi->addIncoming(vl_start, bl_now);

    auto vl_old = g_named_values[var];
    g_named_values[var] = phi;

    auto vl_body = body->codegen();
    if (!vl_body) return nullptr;

    auto vl_incr = incr->codegen();
    if (!vl_incr) return nullptr;

    auto vl_incred = g_builder.CreateFAdd(phi, vl_incr, "incr");

    auto vl_end = end->codegen();
    if (!vl_end) return nullptr;

    if (!vl_end->getType()->isIntegerTy(1))
      vl_end = g_builder.CreateFCmpONE(
        vl_end, llvm::ConstantFP::get(g_context, llvm::APFloat(0.)), "loopcmp");
    
    auto bl_loopend = g_builder.GetInsertBlock(),
         bl_after = llvm::BasicBlock::Create(g_context, "after", func);
    
    g_builder.CreateCondBr(vl_end, bl_loop, bl_after);

    g_builder.SetInsertPoint(bl_after);
    phi->addIncoming(vl_incred, bl_loopend);

    if (vl_old) g_named_values[var] = vl_old;
    else g_named_values.erase(var);

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(g_context));
  }
};

// classes for functions

typedef struct Var {
  std::string name;
  tag_tok type;

  Var(const std::string& name, zMile::tag_tok type) : name(name), type(type)
  {
    if (!tok_is_type(type))
      assert(log_err<std::logic_error>("type error: invalid type assigned to the variable."));
  }

  std::string to_string(bool hasType = false) const {
    if (hasType) return map_tok[type] + " " + name;
    else return name;
  }
} Argu;

class ProtoNode : public Outputable, FuncGenable {
  std::string id;
  tag_tok type;
  std::vector<Argu> args;
public:
  ProtoNode(std::string id, tag_tok type, std::vector<Argu> args)
    : id(id), type(type), args(std::move(args)) {  }
  
  // { Prototype, "id": "cos", "type": "number", "args": ["number theta"] }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ Prototype, \"id\": \"" << id << "\""
       << ", \"type\": \"" << map_tok[type] << "\"";
    if (args.size()) {
      os << ", \"args\": ";
      output_list<Argu>(args, [](auto &x, auto &os){ os << '"' << x.to_string(true) << '"'; });
    }
    os << " }";
  }

  std::string get_name() { return id; }
  // return type
  tag_tok get_type() { return type; }
  void set_type(tag_tok t) { type = t; }

  virtual llvm::Function* codegen() override {
    std::vector<llvm::Type*> vArgsTy;
    for (const auto& x:args)
      vArgsTy.push_back(get_type_of_tok(x.type));
    
    auto l_fun_ty = llvm::FunctionType::get(get_type_of_tok(type), vArgsTy, false);
    auto l_fun = llvm::Function::Create(l_fun_ty, llvm::Function::ExternalLinkage, id, g_module.get());
    
    unsigned idx = 0;
    for (auto& arg: l_fun->args())
      arg.setName(args[idx++].to_string());
    
    return l_fun;
  }
};

using proto_t = std::unique_ptr<ProtoNode>;

class FuncNode : public Outputable, FuncGenable {
  proto_t proto;
  expr_t body; // what it will be calculated.
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
    // special for top level func, set the type
    // if (proto->get_name() == "__top_level__")
    //   proto->set_type(find_value_type(vl_body->getType()));
    
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

    auto vl_body = body->codegen();
    if (vl_body) {
      g_builder.CreateRet(vl_body);
      llvm::verifyFunction(*fun);

      // optimize it
      g_fpm->run(*fun);

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
