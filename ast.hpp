#pragma once

#include <algorithm>
#include <any>
#include <bits/stdint-intn.h>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <string>

#include <map>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <vector>
#include <functional>

#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include "anyex.hpp"
#include "errdef.hpp"
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

inline llvm::Constant* get_default_value(tag_tok t) {
  static llvm::Constant
    *bl = llvm::ConstantInt::get(g_context, llvm::APInt(1, 0)),
    *ch = llvm::ConstantInt::get(g_context, llvm::APInt(8, 0)),
    *it = llvm::ConstantInt::get(g_context, llvm::APInt(32, 0)),
    *st = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(g_context)),
    *db = llvm::ConstantFP::get(g_context, llvm::APFloat(.0));

  switch (t) {
  case tok_kw_bool:
    return bl;
  case tok_kw_char:
    return ch;
  case tok_kw_string:
    return st;
  case tok_kw_number:
    return db;
  case tok_kw_int:
    return it;
  default:
    return log_err<std::logic_error>("no default value found!");
  }
}

inline llvm::Constant* get_default_value(llvm::Type* t) {
  if (t->isAggregateType()) return llvm::ConstantAggregateZero::get(t);
  if (t->isPointerTy()) return llvm::ConstantPointerNull::get((llvm::PointerType*)t);
  return get_default_value(get_tok_of_type(t));
}

inline llvm::Constant* get_const_value_as(llvm::Type* ty, llvm::Value *I) {
  if (I) {
    if (I->getType()->isIntegerTy()) {
      auto CI = llvm::dyn_cast<llvm::ConstantInt>(I);
      if (ty->isDoubleTy())
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), llvm::APFloat(CI->getValue().bitsToDouble()));
      else if (ty->isIntegerTy() && ty->getIntegerBitWidth() >= I->getType()->getIntegerBitWidth())
        return CI;
      else return log_err("no available converter for the global variable initializer.");
    }
    else if (I->getType()->isDoubleTy()) {
      auto CF = llvm::dyn_cast<llvm::ConstantFP>(I);
      if (ty->isDoubleTy())
        return CF;
      else return log_err("no available converter for the global variable initializer.");
    }
    else if (I->getType()->isPointerTy()) {
      auto CP = llvm::dyn_cast<llvm::Constant>(I);
      if (ty->isPointerTy()) {
        return CP;
        if (ty->getPointerElementType() != I->getType())
          std::cerr << "warning: unsafe pointer convertion." << std::endl;
      }
      else return log_err("no available converter for the global variable initializer.");
    } else return log_err("initializer used for unsupported type.");
  }
  return nullptr;
}

inline llvm::Function* get_func(std::string name);

inline llvm::AllocaInst* create_alloca_in_entry(
    llvm::Function *func, const std::string var);


// =========================
// AST Node
// =========================

using ty = llvm::Type;
inline uint get_type_prec(ty* t) {
  switch (t->getTypeID()) {
  case ty::DoubleTyID: return 0x3f3f3f3fu;
  case ty::IntegerTyID: return t->getIntegerBitWidth();
  case ty::PointerTyID: return 32; // hey?
  default: return (log_err<std::logic_error>("cannot determine the type precedence."), 0);
  }
}

inline void rebase_int_pair(llvm::Value *&L, llvm::Value *&R) {
  int pl = get_type_prec(L->getType()), pr = get_type_prec(R->getType());
  if (pl < std::max(pl, pr))
    L = g_builder.CreateSExt(L, ty::getIntNTy(g_context, std::max(pl, pr)));
  if (pr < std::max(pl, pr))
    R = g_builder.CreateSExt(R, ty::getIntNTy(g_context, std::max(pl, pr)));
}

class IOutputable {
public:
  virtual void output(std::ostream & os = std::cerr) = 0;
};

class IValueGenable {
public:
  virtual llvm::Value* codegen() = 0;
};

class ILeftValue {
public:
  virtual llvm::Value* codegen_left() const = 0;
};

class INameGetable {
public:
  virtual std::string get_name() const = 0;
};

class IHasType {
public:
  virtual llvm::Type* get_type() const = 0;
  virtual std::string_view get_type_str() const {
    return "";// magic_enum::enum_name(get_type());
  }
};

class Node : public IOutputable, public IValueGenable, public IHasType, public INameGetable {
public:
  virtual ~Node() = default;
  virtual llvm::Value* codegen() override = 0;
  virtual llvm::Type* get_type() const override = 0;
  virtual bool is_left() { return false; }
};

using node_t = std::unique_ptr<Node>;

using StmtNode = Node;
using stmt_t = std::unique_ptr<StmtNode>;


using ExprNode = Node;
using expr_t = std::unique_ptr<ExprNode>;

class LeftExprNode : public ExprNode, public ILeftValue {
public:
  virtual bool is_left() override { return true; }
};
using left_t = std::unique_ptr<LeftExprNode>;

class DeclNode : public StmtNode {
public:
  virtual ~DeclNode() = default;
  virtual llvm::Value* codegen() = 0;
};

// For var

typedef struct Var {
  std::string name;
  llvm::Type* type;

  Var(const std::string& name, zMile::tag_tok tok) : name(name)
  {
    if (!tok_is_type(tok))
      assert(log_err<std::logic_error>("type error: invalid type assigned to the variable."));
    type = get_type_of_tok(tok);
  }

  Var(const std::string& name, llvm::Type* type) : name(name), type(type) {  }

  std::string get_type_name() const {
    std::string str;
    llvm::raw_string_ostream os(str);
    type->print(os);
    return os.str();
  }

  std::string to_string(bool hasType = false) const {
    if (hasType) return get_type_name() + " " + name;
    else return name;
  }
} Argu;

inline llvm::AllocaInst* create_alloca_in_entry(
    llvm::Function *func, const Var var)
{
  llvm::IRBuilder<> b_(&func->getEntryBlock(), func->getEntryBlock().begin());
  return b_.CreateAlloca(var.type, nullptr, var.name);
}

class NullStmt : public StmtNode {
public:
  NullStmt() {  }
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"NullStmt\" }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::UndefValue::get(get_type());
  }
  virtual std::string get_name() const override {
    return "null";
  }
};

class NumExprNode : public ExprNode {
  std::any val;

public:
  NumExprNode(std::any val) : val(val) {  }
  virtual llvm::Type* get_type() const override {
    return val.type() == typeid(int) ? ty::getInt32Ty(g_context) : ty::getDoubleTy(g_context);
  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"NumExpr\", \"val\": ";
    print_any(val);
    os << ", \"val_type\": \"" << (val.type() == typeid(int) ? "int" : "double") << "\"";
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    if (val.type() == typeid(double))
      return llvm::ConstantFP::get(g_context, llvm::APFloat(std::any_cast<double>(val)));
    if (val.type() == typeid(int))
      return llvm::ConstantInt::get(g_context, llvm::APInt(32, std::any_cast<int>(val)));
    if (val.type() == typeid(int64_t))
      return llvm::ConstantInt::get(g_context, llvm::APInt(64, std::any_cast<int64_t>(val)));
    return log_err<std::invalid_argument>("invalid val type.");
  }
  virtual std::string get_name() const override {
    return "number";
  }
  virtual std::any get_val() const {
    return val;
  }
};

class CharExprNode : public ExprNode {
  char val;

public:
  CharExprNode(char val) : val(val) {  }
  virtual llvm::Type* get_type() const override { return ty::getInt8Ty(g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"CharExpr\", \"val\": '" << val << "' }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantInt::get(g_context, llvm::APInt(/* nBits: */8, val));
  }
  virtual std::string get_name() const override {
    return std::string("") + val;
  }
};

class StringExprNode : public ExprNode {
  std::string val;

public:
  StringExprNode(const std::string& val) : val(val) {  }
  virtual llvm::Type* get_type() const override { return llvm::ArrayType::get(ty::getInt8Ty(g_context), val.size()); }
  virtual void output(std::ostream & os = std::cerr) override {
    // we need to display raw string
    os << "{ \"node_type\": \"StringExpr\", \"val\": \"" << get_raw_string(val) << "\" }";
  }
  virtual llvm::Value* codegen() override {
    return g_builder.CreateGlobalStringPtr(val);
  }
  virtual std::string get_name() const override {
    return val;
  }
};

class BoolExprNode : public ExprNode {
  bool val;

public:
  BoolExprNode(bool val) : val(val) {  }
  virtual llvm::Type* get_type() const override { return ty::getInt1Ty(g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"BoolExpr\", \"val\": " << (val ? "true" : "false") << " }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantInt::get(g_context, llvm::APInt(1, val));
  }
  virtual std::string get_name() const override {
    return val ? "true" : "false";
  }
};

class VarExprNode : public LeftExprNode {
  std::string id;

public:
  VarExprNode(const std::string &id) : id(id) {  }
  virtual llvm::Type* get_type() const override {
    auto tt = codegen_left()->getType();
    assert(tt->isPointerTy());
    return tt->isPointerTy() ? tt->getPointerElementType() : tt;
  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"VarExpr\", \"id\": \"" << id << "\" }";
  }

  virtual llvm::Value* codegen() override {
    auto ptr = codegen_left();
    if (!ptr) return nullptr;
    return g_builder.CreateLoad(ptr, id);
  }

  virtual llvm::Value* codegen_left() const override {
    llvm::Value* ptr = g_named_values[id].top();
    if (!ptr) return log_err("unknown variable name.");
    return ptr;
  }

  virtual std::string get_name() const override { return id; }
};

class SubScriptExprNode : public LeftExprNode {
  left_t arr;
  expr_t idx;
public:
  SubScriptExprNode(left_t arr, expr_t idx) : arr(std::move(arr)), idx(std::move(idx)) {  }
  virtual llvm::Type* get_type() const override { return arr->get_type()->getArrayElementType(); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"SubScriptExprNode\", \"arr\": ";
    arr->output();
    os << ", \"index\": ";
    idx->output();
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    auto v = codegen_left();
    if (!v->getType()->getPointerElementType()->isArrayTy())
      v = g_builder.CreateLoad(v);
    return v;
  }
  virtual llvm::Value* codegen_left() const override {
    auto ptr = arr->codegen_left();
    if (!ptr) return nullptr;
    auto pos = idx->codegen();
    if (!pos) return nullptr;
    
    llvm::ArrayRef<llvm::Value*> arr_pos = {
      llvm::ConstantInt::get(g_context, llvm::APInt(32, 0)), // dereference
      g_builder.CreateFPToUI(pos, llvm::Type::getInt32Ty(g_context))
    };
    return g_builder.CreateGEP(ptr, arr_pos);
  }
  virtual std::string get_name() const override {
    return "SubScript(" + arr->get_name() + ")";
  }
};

class VarDeclNode : public DeclNode {
  using ele_t = std::pair<Var, expr_t>;
  using list_t = std::vector<ele_t>;
  list_t lst;

  bool is_const;

public:
  VarDeclNode(list_t& lst, bool is_const = false) : lst(std::move(lst)), is_const(is_const) {  }
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"VarDeclNode\", \"lst\": ";
    output_list<ele_t>(lst, [](auto& x, auto& os) {
      os << "{ \"name\": \"" << x.first.name << "\", "
         << "\"type\": \"" << x.first.get_type_name() << "\", "
         << "\"init\": ";
      if (x.second) x.second->output();
      else os << "null";
      os << " }";
    }, os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    for (auto&& [v, init] : lst) {
      // codegen: v = init
      bool bArr = v.type->isArrayTy();
      if (init && bArr)
        return log_err("invalid initial value for array.");
      auto I = init ? init->codegen() :
                 bArr ? nullptr : get_default_value(v.type);
      if (init && !I) return nullptr;
      
      if (g_bl_now.empty())
      { // global

        if (g_named_values.find(v.name) != g_named_values.end()
            || !g_named_values[v.name].empty())
          throw syntax_error("global variable name has been occupied.");
        
        auto A = new llvm::GlobalVariable(*g_module, v.type, is_const,
              llvm::Function::InternalLinkage,
              I ? get_const_value_as(v.type, I) : get_default_value(v.type), v.name);

        // maintain the symbol table
        g_named_values[v.name].push(A);
      } else { // local
        if (is_const) {
          g_named_values[v.name].push(get_const_value_as(v.type, I));
        } else {
          // for possible cast
          if (I) {
            auto p1 = get_type_prec(I->getType()), p2 = get_type_prec(v.type);
            if (p1 > p2) return log_err("the initial value type is not capable for the variable.");
            if (I->getType()->isIntegerTy() && v.type->isDoubleTy())
              I = g_builder.CreateSIToFP(I, v.type);
          }

          auto A = g_builder.CreateAlloca(v.type, nullptr, v.name);
          if (I) g_builder.CreateStore(I, A);
          // maintain the symbol table
          g_named_values[v.name].push(A);
        }

      }
    }
    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }

  // please use get_list to get all names.
  virtual std::string get_name() const override {
    return lst.front().first.name;
  }

  list_t& get_list() { return lst; }
};

class BinExprNode : public ExprNode {
  char op;
  expr_t left, right;

public:
  BinExprNode(char op, expr_t left,
    expr_t right) : op(op),
    left(std::move(left)), right(std::move(right)) {  }

  llvm::Type* get_max_type() const {
    auto lp = get_type_prec(left->get_type()), rp = get_type_prec(right->get_type());
    // auto &refa = (lp <= rp ? left : right), &refb = (lp <= rp ? right : left);
    return lp >= rp ? left->get_type() : right->get_type();
  }
  
  virtual llvm::Type* get_type() const override {
    if (abs(op) == '>' || abs(op) == '<')
      return ty::getInt1Ty(g_context);
    return get_max_type();
  }

  virtual void output(std::ostream & os = std::cerr) override {
    std::string op_display;
    if (op < 0) op_display = -op, op_display += '=';
    else op_display = op;
    os << "{ \"node_type\": \"BinExpr\", \"operator\": \"" << op_display << "\", \"left\": ";
    left->output();
    os << ", \"right\": ";
    right->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    // special for assignment
    if (op == '=') {
      auto p1 = get_type_prec(left->get_type()), p2 = get_type_prec(right->get_type());
      if (p1 < p2) return log_err("invalid assignment!");

      auto R = right->codegen();

      auto L = dynamic_cast<ILeftValue*>(left.get())->codegen_left();

      if (!L || !R) return nullptr;

      if (left->get_type()->isDoubleTy() && right->get_type()->isIntegerTy())
        R = g_builder.CreateSIToFP(R, left->get_type());

      g_builder.CreateStore(R, L);
      // the value of assignment is rhs
      return R;
    }

    auto L = left->codegen(), R = right->codegen();
    if (!L || !R) return nullptr;

    auto res_ty = ((get_type_prec(L->getType()) > get_type_prec(R->getType()) ? L->getType() : R->getType()));
    // get_type();
    
    if (res_ty->isDoubleTy()) {
      if (L->getType()->isIntegerTy())
        L = g_builder.CreateSIToFP(L, res_ty);
      if (R->getType()->isIntegerTy())
        R = g_builder.CreateSIToFP(R, res_ty);
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
      case -'<':
        return g_builder.CreateFCmpULE(L, R, "le");
      case -'>':
        return g_builder.CreateFCmpUGE(L, R, "ge");
      case '@':
        return g_builder.CreateFCmpUEQ(L, R, "ueq");
      case -'@':
        return g_builder.CreateFCmpUNE(L, R, "une");
      default: break;
      }
    } else if (res_ty->isIntegerTy()) {
      L = g_builder.CreateIntCast(L, res_ty, true);
      R = g_builder.CreateIntCast(R, res_ty, true);
      switch (op)
      {
      case '+':
        return g_builder.CreateAdd(L, R, "add");
      case '-':
        return g_builder.CreateSub(L, R, "sub");
      case '*':
        return g_builder.CreateMul(L, R, "mul");
      case '/':
        return g_builder.CreateSDiv(L, R, "div");
      case '%':
        return g_builder.CreateSRem(L, R, "mod");
      case '<':
        return g_builder.CreateICmpULT(L, R, "lt");
      case '>':
        return g_builder.CreateICmpUGT(L, R, "gt");
      case -'<':
        return g_builder.CreateICmpULE(L, R, "le");
      case -'>':
        return g_builder.CreateICmpUGE(L, R, "ge");
      case '@':
        return g_builder.CreateICmpEQ(L, R, "eq");
      case -'@':
        return g_builder.CreateICmpNE(L, R, "ne");
      default: break;
      }
    }

    return log_err("invalid binary operator.");
  }

  virtual std::string get_name() const override {
    return "binop";
  }
};

class CallExprNode : public ExprNode {
  std::string func;
  std::vector<expr_t> args;

public:
  CallExprNode(const std::string &func, std::vector<expr_t> args)
    : func(func), args(std::move(args)) {  }

  virtual llvm::Type* get_type() const override { return get_func(func)->getReturnType(); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"CallExpr\", \"func_name\": \"" << func << "\", \"args\": ";
    output_list<expr_t>(args, [](auto &x, auto &os){ x->output(os); }, os);
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    llvm::Function *l_func = get_func(func);
    if (!l_func) return log_err("unknown function reference.");
    
    if ((l_func->isVarArg() && args.size() < l_func->arg_size()) ||
         (!l_func->isVarArg() && args.size() != l_func->arg_size()))
            return log_err("incorrect # arguments passed.");

    std::vector<llvm::Value*> lv_args;
    auto it = l_func->arg_begin();
    for (size_t i = 0, e = args.size(); i != e; i++) {
      auto V = args[i]->codegen();
      if (!V) return nullptr;
      if (!l_func->isVarArg()) {
        auto p1 = get_type_prec(V->getType()), p2 = get_type_prec(it->getType());
        if (p1 > p2) return log_err("the param value type is not capable for the argument.");
        if (V->getType()->isIntegerTy() && it->getType()->isDoubleTy())
          V = g_builder.CreateSIToFP(V, it->getType());
        it++;
      }
      lv_args.push_back(V);
    }

    return l_func->getReturnType()->isVoidTy() ? 
            g_builder.CreateCall(l_func, lv_args) :
            g_builder.CreateCall(l_func, lv_args, "call");
  }

  virtual std::string get_name() const override {
    return func;
  }
};

class RetStmtNode : public StmtNode {
  expr_t val;
public:
  RetStmtNode(expr_t val) : val(std::move(val)) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"RetStmt\", \"val\": ";
    val->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto V = val->codegen();
    if (!V) return nullptr;

    auto ret_ty = g_builder.getCurrentFunctionReturnType();
    if (ret_ty->isVoidTy())
      return g_builder.CreateRetVoid();
    auto p1 = get_type_prec(val->get_type()), p2 = get_type_prec(ret_ty);
    if (p1 > p2) return log_err("return value type is not capable for the function.");
    if (ret_ty->isDoubleTy() && val->get_type()->isIntegerTy())
      V = g_builder.CreateSIToFP(V, ret_ty);

    g_builder.CreateStore(V, g_named_values["__return_value"].top());
    return g_builder.CreateBr(g_bl_ret);
  }

  virtual std::string get_name() const override {
    return val->get_name();
  }
};

class BlockNode : public StmtNode {
  using list_t = std::vector<stmt_t>;
  list_t v;
public:
  BlockNode(list_t v) : v(std::move(v)) {  }
  virtual ~BlockNode() = default;
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"BlockNode\", \"list\": ";
    output_list<stmt_t>(v, [](auto& x, auto& os){ x->output(os); }, os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    g_bl_now.push(this);

    llvm::Value* res = llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));

    for (auto &x:v) {
      res = x->codegen();
      if (instof<RetStmtNode>(*x))
        break; // ignore the following stmts.
    }

    for (auto &x:v) if (instof<VarDeclNode>(*x))
      for (auto &[var, init] : dynamic_cast<VarDeclNode*>(x.get())->get_list())
        g_named_values[var.name].pop();

    g_bl_now.pop();

    return res;
  }
  virtual std::string get_name() const override {
    return "block";
  }
};

using block_t = std::unique_ptr<class BlockNode>;

class IfStmtNode : public StmtNode {
  expr_t cond;
  block_t then, els;

public:
  IfStmtNode(expr_t cond, block_t then, block_t els)
    : cond(std::move(cond)), then(std::move(then)), els(std::move(els)) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"IfStmt\", \"cond\": ";
    cond->output();
    os << ", \"then\": ";
    then->output();
    os << ", \"else\": ";
    if(els) els->output();
    else os << "null";
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto vl_cond = cond->codegen();
    if (vl_cond->getType() != llvm::Type::getInt1Ty(g_context) || vl_cond->getType()->getIntegerBitWidth() != 1)
    {
      if (vl_cond->getType()->isIntegerTy())
        vl_cond = g_builder.CreateICmpNE(
          vl_cond,
          llvm::ConstantInt::get(vl_cond->getType(), 0),
          "ifcond");
      else if (vl_cond->getType()->isDoubleTy())
        vl_cond = g_builder.CreateFCmpONE(
          vl_cond,
          llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), 0),
          "ifcond");
      else return log_err("unsupported condition expression type.");
    }
    
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
    if (g_builder.GetInsertBlock()->getInstList().empty()
      || !g_builder.GetInsertBlock()->getInstList().back().isTerminator())
      g_builder.CreateBr(bl_merg);
    bl_then = g_builder.GetInsertBlock();

    // insert the else block
    func->getBasicBlockList().push_back(bl_else);
    g_builder.SetInsertPoint(bl_else);

    if (els) {
      llvm::Value* vl_else = els->codegen();
      if (!vl_else) return nullptr;
    }

    // Also
    if (g_builder.GetInsertBlock()->getInstList().empty()
      || !g_builder.GetInsertBlock()->getInstList().back().isTerminator())
      g_builder.CreateBr(bl_merg);

    if (bl_else->getInstList().empty()
      || !bl_else->getInstList().back().isTerminator())
      g_builder.SetInsertPoint(bl_else),
      g_builder.CreateBr(bl_merg);

    func->getBasicBlockList().push_back(bl_merg);
    g_builder.SetInsertPoint(bl_merg);

    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }

  virtual std::string get_name() const override {
    return "if";
  }
};

class ForStmtNode : public ExprNode {
  stmt_t start, every;
  expr_t cond;
  block_t body;
public:
  ForStmtNode(stmt_t start, expr_t cond, stmt_t every, block_t body) :
    start(std::move(start)), every(std::move(every)),
    cond(std::move(cond)), body(std::move(body)) {  }
  
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"ForStmt\", \"start\": ";
    start->output();
    os << ", \"cond\": ";
    cond->output();
    os << ", \"every\": ";
    every->output();
    os << ", \"body\": ";
    body->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto func = g_builder.GetInsertBlock()->getParent();

    auto vl_start = start->codegen();
    if (!vl_start) return nullptr;
    
    auto bl_loop  = llvm::BasicBlock::Create(g_context, "loop", func),
         bl_body  = llvm::BasicBlock::Create(g_context, "body", func),
         bl_lend  = llvm::BasicBlock::Create(g_context, "lend", func),
         bl_after = llvm::BasicBlock::Create(g_context, "after", func);

    g_builder.CreateBr(bl_loop);
    g_builder.SetInsertPoint(bl_loop);

    // re-eval cond every loop
    auto vl_cond = cond->codegen();
    if (!vl_cond->getType()->isIntegerTy(1)) {
      if (vl_cond->getType()->isIntegerTy())
        vl_cond = g_builder.CreateICmpNE(
          vl_cond,
          llvm::ConstantInt::get(vl_cond->getType(), 0),
          "loopcond");
      else if (vl_cond->getType()->isDoubleTy())
        vl_cond = g_builder.CreateFCmpUNE(
          vl_cond,
          llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), 0),
          "loopcond");
      else return log_err("unsupported condition expression type.");
    }

    g_builder.CreateCondBr(vl_cond, bl_body, bl_after);

    g_builder.SetInsertPoint(bl_body);

    auto vl_body = body->codegen();
    if (!vl_body) return nullptr;

    g_builder.CreateBr(bl_lend);
    g_builder.SetInsertPoint(bl_lend);

    auto vl_every = every->codegen();
    if (!vl_every) return nullptr;

    g_builder.CreateBr(bl_loop);

    g_builder.SetInsertPoint(bl_after);
    
    if (instof<VarDeclNode>(*start)) {
      auto& lst = ((VarDeclNode*)start.get())->get_list();
      for (auto&& [var, t] : lst)
        g_named_values[var.name].pop();
    }

    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }

  virtual std::string get_name() const override {
    return "for";
  }
};

// classes for functions

class ProtoNode : public Node {
  std::string id;
  tag_tok type;
  std::vector<Argu> args;
  bool var_args;
public:
  ProtoNode(std::string id, tag_tok type, std::vector<Argu> args, bool var_args)
    : id(id), type(type), args(std::move(args)), var_args(var_args) {  }
  
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }

  // { Prototype, "id": "cos", "type": "number", "args": ["number theta"] }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Prototype\", \"id\": \"" << id << "\""
       << ", \"type\": \"" << map_tok[type] << "\"";
    if (args.size()) {
      os << ", \"args\": ";
      output_list<Argu>(args, [](auto &x, auto &os){ os << '"' << x.to_string(true) << '"'; }, os);
    }
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    std::vector<llvm::Type*> vArgsTy;
    for (const auto& x:args)
      vArgsTy.push_back(x.type);
    
    auto l_func_ty = llvm::FunctionType::get(get_type_of_tok(type), vArgsTy, var_args);
    auto l_func = llvm::Function::Create(l_func_ty, llvm::Function::ExternalLinkage, id, g_module.get());
    
    unsigned idx = 0;
    for (auto& arg: l_func->args())
      arg.setName(args[idx++].to_string());
    
    return l_func;
  }

  virtual std::string get_name() const override { return id; }
  // return type
  tag_tok get_return_type() { return type; }
  void set_type(tag_tok t) { type = t; }
};

using proto_t = std::unique_ptr<ProtoNode>;

class FuncNode : public LeftExprNode {
  proto_t proto;
  block_t body; // what it will be calculated.
public:
  FuncNode(proto_t proto,
    block_t body) : proto(std::move(proto)),
      body(std::move(body)) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(g_context); }

  // { Function, { Prototype, $...$ }, { Body, "..." } }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Function\", \"prototype\": ";
    proto->output(os);
    os << ", \"body\": ";
    body->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    // ownership transfered, we need a reference
    auto &rpr = *proto;
    g_protos[proto->get_name()] = std::move(proto);
    // second
    llvm::Function* fun = get_func(rpr.get_name());
    if (!fun) return nullptr;

    if (!fun->empty())
      return log_err("function cannot be redefined.");

    auto bas_blo = llvm::BasicBlock::Create(g_context, "entry", fun);
    g_builder.SetInsertPoint(bas_blo);

    std::vector<std::string> extra_pop;

    for (auto& arg: fun->args()) {
      extra_pop.push_back(arg.getName());
      auto alloca = create_alloca_in_entry(
          fun, Var { arg.getName(), get_tok_of_type(arg.getType()) });
      g_builder.CreateStore(&arg, alloca);
      g_named_values[arg.getName()].push(alloca);
    }
    auto ret_ty = fun->getReturnType();
    llvm::AllocaInst* var_ret;
    if (!ret_ty->isVoidTy())
    {
      g_bl_ret = llvm::BasicBlock::Create(g_context, "__return_block");
      var_ret = 
        create_alloca_in_entry(
          fun,   Var {
          "__return_value",
          get_tok_of_type(ret_ty)
        });
      
      g_named_values["__return_value"].push(var_ret);
      extra_pop.push_back("__return_value");
    }
    auto vl_body = body->codegen();
    if (vl_body) {
      for (auto &x:extra_pop) g_named_values[x].pop();

      if (ret_ty->isVoidTy())
        g_builder.CreateRetVoid();
      else {
        auto& ls = fun->getBasicBlockList().back().getInstList();
        bool term = ls.empty() | !ls.back().isTerminator();
        fun->getBasicBlockList().push_back(g_bl_ret);
        if (term) g_builder.CreateBr(g_bl_ret);
        g_builder.SetInsertPoint(g_bl_ret);
        g_builder.CreateRet(g_builder.CreateLoad(var_ret));
      }

      llvm::verifyFunction(*fun);

      // optimize it
      g_fpm->run(*fun);

      return fun;
    }
    fun->eraseFromParent();
    return nullptr;
  }

  virtual llvm::Value* codegen_left() const override {
    auto fun = get_func(proto->get_name());
    if (!fun) return log_err("unknown function name!");
    return fun;
  }

  virtual std::string get_name() const override { return proto->get_name(); }
};

using func_t = std::unique_ptr<FuncNode>;

inline llvm::Function* get_func(std::string name) {
  if (auto *res = g_module->getFunction(name))
    return res;
  
  auto res = g_protos.find(name);
  if (res != g_protos.end())
    return (llvm::Function*)res->second->codegen();
  
  return nullptr;
}

// class AddressExprNode : public ExprNode {
//   expr_t operand;
// public:
//   AddressExprNode(expr_t operand) : operand(std::move(operand)) {  }
//   virtual llvm::Type* get_type() const override { return operand->get_type()->getPointerTo(); }
//   virtual void output(std::ostream & os = std::cerr) override {
//     os << "{ \"node_type\": \"AddressExpr\", \"operand\": \"";
//     if (instof<FuncNode>(*operand))
//       os << operand->get_name();
//     else operand->output(os);
//     os << "\" }";
//   }

//   virtual llvm::Value* codegen() override {
//     return dynamic_cast<ILeftValue*>(operand.get())->codegen_left();
//   }
//   virtual std::string get_name() const override {
//     return operand->get_name();
//   }
// };

// class DisAddrExprNode : public ExprNode {
//   expr_t operand;
// public:
//   DisAddrExprNode(expr_t operand) : operand(std::move(operand)) {  }
//   virtual llvm::Type* get_type() const override { return operand->get_type()->getPointerElementType(); }
//   virtual void output(std::ostream & os = std::cerr) override {
//     os << "{ \"node_type\": \"DisAddressExpr\", \"operand\": ";
//     operand->output(os);
//     os << " }";
//   }
//   virtual llvm::Value* codegen() override {
//     return g_builder.CreateLoad(operand->codegen());
//   }
//   virtual std::string get_name() const override {
//     return "DisAddr(" + operand->get_name() + ")";
//   }
// };

class UnaryExprNode : public ExprNode {
  char op;
  expr_t operand;
public:
  UnaryExprNode(char op, expr_t operand) : op(op), operand(std::move(operand)) {  }
  virtual llvm::Type* get_type() const override {
    if (auto t = operand->get_type()) {
      if (op == '*') {
        if (t->isPointerTy())
          return t->getPointerElementType();
        else return log_err("invalid disaddress expression operand type!");
      }
      if (op == '&') return t->getPointerTo();
      if (op == '!') return ty::getInt8Ty(g_context);
      if (op == '~') return t;
      return log_err("unknown unary operator.");
    } else return log_err("invalid unary expression operand type!");
  }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"UnaryExpr\", \"operator\": '"
    << op << "', \"operand\": ";
    operand->output(os);
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto t = operand->get_type();
    switch (op) {
      case '*': return g_builder.CreateLoad(operand->codegen());
      case '&': return dynamic_cast<ILeftValue*>(operand.get())->codegen_left();
      case '!':
        if (t->isIntegerTy()) return g_builder.CreateICmpEQ(operand->codegen(), llvm::ConstantInt::get(t, 0));
        else if (t->isDoubleTy()) return g_builder.CreateFCmpOEQ(operand->codegen(), llvm::ConstantFP::get(t, 0));
        else return log_err("invalid logical not expression operand type!");
      case '~':
        if (!t->isIntegerTy()) return log_err("only integer can get bitwise not.");
        return g_builder.CreateNot(operand->codegen());
      default:
        return log_err("unknown unary operator.");
    }
  }
  virtual std::string get_name() const override {
    return "UnaryExpression";
  }
};

class SelfFrontCreExprNode : public ExprNode, public ILeftValue {
  expr_t left;
  bool is_add;
public:
  SelfFrontCreExprNode(expr_t left, bool is_add)
      : left(std::move(left)), is_add(is_add) {  }

  virtual llvm::Type* get_type() const override { return left->get_type(); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Self" << (is_add ? "In" : "De") << "crementExpr\", \"left\": ";
    left->output(os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    auto P = left->codegen();
    auto Q = g_builder.CreateAdd(P, llvm::ConstantInt::get(g_context, llvm::APInt(32, is_add ? 1 : -1)));
    auto L = dynamic_cast<ILeftValue*>(left.get())->codegen_left();
    g_builder.CreateStore(Q, L);

    return Q;
  }
  virtual std::string get_name() const override {
    return std::string(is_add ? "in" : "de") + "crement";
  }
  virtual llvm::Value* codegen_left() const override {
    return dynamic_cast<ILeftValue*>(left.get())->codegen_left();
  }
};

class SelfBackCreExprNode : public ExprNode {
  expr_t left;
  bool is_add;
public:
  SelfBackCreExprNode(expr_t left, bool is_add)
      : left(std::move(left)), is_add(is_add) {  }

  virtual llvm::Type* get_type() const override { return left->get_type(); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"SelfBack" << (is_add ? "In" : "De")
       << "crementExpr\", \"left\": ";
    left->output(os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    auto P = left->codegen();
    auto Q = g_builder.CreateAdd(P, llvm::ConstantInt::get(g_context, llvm::APInt(32, is_add ? 1 : -1)));
    auto L = dynamic_cast<ILeftValue*>(left.get())->codegen_left();
    g_builder.CreateStore(Q, L);

    return P;
  }
  virtual std::string get_name() const override {
    return std::string(is_add ? "in" : "de") + "crement";
  }

};

}