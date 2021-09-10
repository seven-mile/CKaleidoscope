#pragma once

#include <algorithm>
#include <any>
#include <bits/stdint-intn.h>
#include <bits/stdint-uintn.h>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <llvm/Support/Casting.h>
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
#include <llvm/ADT/APSInt.h>
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
#include "ioagent.hpp"
#include "lexer.hpp"
#include "optimizer.hpp"

namespace zMile {

template <class err_t = syntax_error>
inline auto log_err_with_loc(std::string errinfo, SourceLoc loc) {
  throw err_t(errinfo + " [Line " + std::to_string(loc.line) + ", Column " + std::to_string(loc.column) + "]");
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

template <class T>
inline std::string textize_object(T* x) {
  std::string s = "";
  llvm::raw_string_ostream os(s);
  os << *x;
  return os.str();
}

// print R("\n") as "\\n"
inline std::string get_raw_string(const std::string& str) {
  std::string res = "";
  for (char c : str)
    switch (c) {
      case '\n': res.append("\\n"); break;
      case '\r': res.append("\\r"); break;
      case '\t': res.append("\\t"); break;
      default: res.push_back(c); break;
    }
  return res;
}

inline llvm::ConstantInt* get_const_int_value(int64_t v, int b = 32, bool is_signed = true) {
  return llvm::ConstantInt::get(*g_context, llvm::APInt(b, v, is_signed));
}

inline llvm::Constant* get_const_double_value(double v) {
  return llvm::ConstantFP::get(llvm::Type::getDoubleTy(*g_context), v);
}

inline llvm::Constant* get_default_value(llvm::Type* t, SourceLoc loc) {
  if (t->isAggregateType()) return llvm::ConstantAggregateZero::get(t);
  if (t->isPointerTy()) return llvm::ConstantPointerNull::get((llvm::PointerType*)t);
  if (t->isIntegerTy()) return get_const_int_value(0, t->getScalarSizeInBits());
  if (t->isDoubleTy()) return get_const_double_value(0);
  return log_err_with_loc<std::invalid_argument>("failed to get default value, invalid type.", loc);
}

inline llvm::Function* get_func(std::string name);

inline llvm::AllocaInst* create_alloca_in_entry(
    llvm::Function *func, const std::string var);


// =========================
// AST Node
// =========================

using ty = llvm::Type;
inline uint get_type_prec(ty* t, SourceLoc loc) {
  switch (t->getTypeID()) {
  case ty::DoubleTyID: return 0x3f3f3f3fu;
  case ty::IntegerTyID: return t->getIntegerBitWidth();
  case ty::PointerTyID:
  case ty::ArrayTyID:
    return 32; // hey?
  default: return (log_err_with_loc<std::logic_error>("cannot determine the type precedence.", loc), 0);
  }
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
  virtual llvm::Value* codegen_left() = 0;
};

class INamedObject {
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

class IHasSourceLoc {
public:
  SourceLoc loc {0,0};
  IHasSourceLoc(SourceLoc loc) : loc(loc) {  }
  template<class err_t = syntax_error>
  auto log_err(std::string errinfo) const {
    return log_err_with_loc(errinfo, loc);
  }
};

class Node : public IOutputable, public IValueGenable, public IHasType, public IHasSourceLoc {
public:
  Node(SourceLoc loc) : IHasSourceLoc(loc) {  }
  virtual ~Node() = default;
  virtual llvm::Value* codegen() override = 0;
  virtual llvm::Type* get_type() const override = 0;
  virtual bool is_left() const { return false; }
};

using node_t = std::unique_ptr<Node>;

using StmtNode = Node;
using stmt_t = std::unique_ptr<StmtNode>;


using ExprNode = Node;
using expr_t = std::unique_ptr<ExprNode>;

class LeftExprNode : public ExprNode, public ILeftValue {
public:
  LeftExprNode(SourceLoc loc) : ExprNode(loc) {  }
  virtual bool is_left() const override { return true; }
};
using left_t = std::unique_ptr<LeftExprNode>;

class DeclNode : public StmtNode {
public:
  DeclNode(SourceLoc loc) : StmtNode(loc) {  }
  virtual ~DeclNode() = default;
  virtual llvm::Value* codegen() = 0;
};

// For var

typedef struct Var {
  std::string name;
  llvm::Type* type;

  Var(const std::string& name, llvm::Type* type) : name(name), type(type) {  }

  std::string get_type_name() const { return textize_object(type); }

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
  NullStmt(SourceLoc loc) : StmtNode(loc) { this->loc = loc; }
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"NullStmt\", \"loc\": [" << loc.line << ',' << loc.column << "] }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::UndefValue::get(get_type());
  }
};

class NumExprNode : public ExprNode {
  std::any val;

public:
  NumExprNode(std::any val, SourceLoc loc) : val(val), ExprNode(loc) {  }
  virtual llvm::Type* get_type() const override {
    if (val.type() == typeid(int) || val.type() == typeid(uint))
      return ty::getInt32Ty(*g_context);
    else if (val.type() == typeid(int64_t) || val.type() == typeid(uint64_t))
      return ty::getInt64Ty(*g_context);
    else if (val.type() == typeid(double)) return ty::getDoubleTy(*g_context);
    else return log_err("invalid val type.");
  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"NumExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"val\": ";
    print_any(val);
    os << ", \"val_type\": \"" << textize_object(get_type()) << "\"";
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    if (val.type() == typeid(double))
      return get_const_double_value(std::any_cast<double>(val));
    if (val.type() == typeid(int))
      return get_const_int_value(std::any_cast<int>(val), 32);
    if (val.type() == typeid(uint))
      return get_const_int_value(std::any_cast<int>(val), 32, false);
    if (val.type() == typeid(int64_t))
      return get_const_int_value(std::any_cast<int64_t>(val), 64);
    if (val.type() == typeid(uint64_t))
      return get_const_int_value(std::any_cast<int64_t>(val), 64, false);
    return log_err<std::invalid_argument>("invalid val type.");
  }
  virtual const std::any& get_value() const { return val; }
  
};

class CharExprNode : public ExprNode {
  char val;

public:
  CharExprNode(char val, SourceLoc loc) : val(val), ExprNode(loc) {  }
  virtual llvm::Type* get_type() const override { return ty::getInt8Ty(*g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"CharExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"val\": \"" << val << "\" }";
  }
  virtual llvm::Value* codegen() override {
    return get_const_int_value(val, 8);
  }
  virtual char get_value() const { return val; }
  
};

class StringExprNode : public ExprNode {
  std::string val;

public:
  StringExprNode(const std::string& val, SourceLoc loc) : val(val), ExprNode(loc) {  }
  virtual llvm::Type* get_type() const override { return llvm::ArrayType::get(ty::getInt8Ty(*g_context), val.size()); }
  virtual void output(std::ostream & os = std::cerr) override {
    // we need to display raw string
    os << "{ \"node_type\": \"StringExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"val\": \"" << get_raw_string(val) << "\" }";
  }
  virtual llvm::Value* codegen() override {
    return g_builder->CreateGlobalStringPtr(val);
  }
  virtual std::string get_value() const { return val; }
};

class BoolExprNode : public ExprNode {
  bool val;

public:
  BoolExprNode(bool val, SourceLoc loc) : val(val), ExprNode(loc) {  }
  virtual llvm::Type* get_type() const override { return ty::getInt1Ty(*g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"BoolExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"val\": " << (val ? "true" : "false") << " }";
  }
  virtual llvm::Value* codegen() override {
    return get_const_int_value(val, 1);
  }
  virtual bool get_value() const { return val; }
  
};

class ParenExprNode : public LeftExprNode {
public:
  expr_t val;

  ParenExprNode(expr_t val, SourceLoc loc) : val(std::move(val)), LeftExprNode(loc) {  }
  virtual llvm::Type* get_type() const override { return val->get_type(); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"ParenExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"val\": ";
    val->output();
    os << " }";
  }
  virtual llvm::Value* codegen() override { return val->codegen(); }
  virtual bool is_left() const override { return val->is_left(); }
  virtual llvm::Value* codegen_left() override { return val->codegen(); }
  virtual expr_t& get_value() { return val; }
};

class VarExprNode : public LeftExprNode, public INamedObject {
  std::string id;

public:
  VarExprNode(const std::string &id, SourceLoc loc) : id(id), LeftExprNode(loc) {  }
  virtual llvm::Type* get_type() const override {
    if (g_named_values.find(id) != g_named_values.end()
        && g_named_values[id].size()) {
      if (is_const()) return get_const_value()->getType();
      // AllocaInst::getType === [VarType]*
      else return g_named_values[id].top()->getType()->getPointerElementType();
    }
    return log_err("undefined variable (maybe haven't codegen it.)");
  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"VarExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"id\": \"" << id << "\" }";
  }

  virtual bool is_global() const {
    if (g_named_values.find(id) != g_named_values.end()
        && g_named_values[id].size())
      return llvm::isa<llvm::GlobalVariable>(g_named_values[id].top());
    throw object_invalid("undefined variable (maybe haven't codegen it.)");
  }

  virtual bool is_const() const {
    if (g_named_values.find(id) != g_named_values.end()
        && g_named_values[id].size()) {
      // GlobalVariable* itself is always Constant*,
      // but whether it is const depends on the GlobalVariable's setting.
      if (auto ptrg = llvm::dyn_cast<llvm::GlobalVariable>(g_named_values[id].top()))
        return ptrg->isConstant();
      return llvm::isa<llvm::Constant>(g_named_values[id].top());
    }
    throw object_invalid("undefined variable (maybe haven't codegen it.)");
  }

  // please ensure constant
  virtual llvm::Constant* get_const_value() const {
    if (is_const()) {
      if (is_global())
        // global constant needs to return its initializer
        return llvm::dyn_cast<llvm::GlobalVariable>(g_named_values[id].top())->getInitializer();
      else
        // local constant is a direct Constant*
        return llvm::dyn_cast<llvm::Constant>(g_named_values[id].top());
    }
    return nullptr;
  }

  virtual llvm::Value* codegen() override {
    // if it's a constant non-aggregate variable
    if (is_const() && !get_type()->isArrayTy())
      return get_const_value();
    
    // now the only left situation is AllocaInst*
    // gives the whole variable pointer, llvm type is [VarType]*
    auto ptr = g_named_values[id].top();
    if (!ptr) return nullptr;
    if (get_type()->isArrayTy()) {
      // deprecated: gives the first element pointer
      // return g_builder->CreateGEP(ptr, {get_const_int_value(0), get_const_int_value(0)});
      // now: return the whole variable pointer, such as [100 x i32][50 x i32]*
      //      and when it's necessary, (for example, arithmetic operation)
      //      the pointer is implicitly casted into first element pointer
      return ptr;
    }
    // gives the first element value
    else return g_builder->CreateLoad(ptr, id);
  }

  virtual llvm::Value* codegen_left() override {
    if (is_const())
      return log_err("constant value cannot be a left value!");
    llvm::Value* ptr = g_named_values[id].top();
    return ptr;
  }

  virtual bool is_left() const override {
    return !is_const();
  }

  virtual std::string get_name() const override { return id; }
};

class SubScriptExprNode : public LeftExprNode {
public:
// arr has there situations
//   1. prvalue pointer.
//      (new int[18])[6] = 7; -> arr is i32*
//      can be resolved using direct (GEP arr idx)
//   2. lvalue array or pointer.
//      int C[10][10], C[5][3] = 10; -> arr is [[i32 x 10] x 10]*
//      int *a; a[5] = 1; arr is (i32*)*
//      can be simply resolved using (GEP arr (0 idx))
//   3. constant array(direct array, prvalue array). todo_prio: [temporarily ignore]
//      const int dx[] = {-1,1}, printf("%d\n", dx[0]);
//      two solutions: ConstantArray + extractvalue, or GlobalVariable<Constant> + GEP

  expr_t arr;
  expr_t idx;
  SubScriptExprNode(expr_t arr, expr_t idx, SourceLoc loc) : arr(std::move(arr)), idx(std::move(idx)), LeftExprNode(loc) {  }
  virtual llvm::Type* get_type() const override {
    auto t = arr->get_type();
    if (t->isArrayTy()) return t->getArrayElementType();
    if (t->isPointerTy()) return t->getPointerElementType();

    return log_err("invalid type for subscript member access!");
  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"SubScriptExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"arr\": ";
    arr->output();
    os << ", \"index\": ";
    idx->output();
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    auto v = codegen_left();
    // if it is the last dimension, return the exact value
    if (!v->getType()->getPointerElementType()->isArrayTy())
      v = g_builder->CreateLoad(v);
    // otherwise, return the current pointer!
    // future: return reference to array [ int (&)[N] ]
    return v;
  }
  virtual llvm::Value* codegen_left() override {
    auto pos = idx->codegen();
    if (!pos) return nullptr;
    
    if (pos->getType()->isDoubleTy())
      pos = g_builder->CreateFPToUI(pos, llvm::Type::getInt32Ty(*g_context));
    
    // For lvalue array/struct, we must get its pointer Value*.
    // That's because some non-pointer type such as
    // [i32 x N]*'s address information will be lost
    // after codegen(), which generally calls load intruction.
    // Apart from them, all generic value should use its codegen.
    if (!arr->is_left() || !arr->get_type()->isArrayTy()) {
      // prvalue
      if (!arr->get_type()->isPointerTy())
        throw syntax_error("arr of prvalue must be a pointer.");
      return g_builder->CreateGEP(arr->codegen(), pos);
    }

    auto ptr = dynamic_cast<ILeftValue*>(arr.get())->codegen_left();
    if (!ptr) return nullptr;

    return g_builder->CreateGEP(ptr, {
      get_const_int_value(0), // dereference
      pos
    });
  }
};


inline llvm::Constant* get_calcuate_const_value(llvm::Value* value, llvm::Type* target_type, SourceLoc loc) {
  if (target_type->isIntegerTy()) {
    if (value->getType()->isDoubleTy()) {
      double detailv = llvm::dyn_cast<llvm::ConstantFP>(value)->getValueAPF().convertToDouble();
      if (target_type->isIntegerTy(1))
        return get_const_int_value(!!detailv, 1, false);
      else if (target_type->isIntegerTy(32))
        return get_const_int_value((int)detailv);
      else if (target_type->isIntegerTy(64))
        return get_const_int_value((int64_t)detailv);
      else return log_err_with_loc("invalid initial value for integer.", loc);
    } else {
      return get_const_int_value(llvm::dyn_cast<llvm::ConstantInt>(value)->getZExtValue(), target_type->getIntegerBitWidth());
    }
  } else if (target_type->isDoubleTy()) {
    if (value->getType()->isDoubleTy()) {
      return get_const_double_value(llvm::dyn_cast<llvm::ConstantFP>(value)->getValueAPF().convertToDouble());
    } else {
      return get_const_double_value(llvm::dyn_cast<llvm::ConstantInt>(value)->getZExtValue());
    }
  }
  return log_err_with_loc("unsupported target type to calculate.", loc);
}

class VarDeclNode : public DeclNode {
  using ele_t = std::pair<Var, expr_t>;
  using list_t = std::vector<ele_t>;
  list_t lst;

  bool is_const;

public:
  VarDeclNode(list_t& lst, bool is_const = false, SourceLoc loc = {0,0}) : lst(std::move(lst)), is_const(is_const), DeclNode(loc) {  }
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"VarDecl\", \"loc\": [" << loc.line << ',' << loc.column << "], \"lst\": ";
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
      
      llvm::Value* I = nullptr;
      if (init) {
        I = init->codegen();
        if (!I) return nullptr;
      }
      
      if (g_bl_now.empty())
      { // global

        if (g_named_values.find(v.name) != g_named_values.end()
            && !g_named_values[v.name].empty())
          throw syntax_error("global variable name has been occupied.");

        if (is_const && !I)
          return log_err("constant global variable must have a constant initial value.");
        
        llvm::Constant* init_res = nullptr;
        if (!bArr && I) init_res = get_calcuate_const_value(I, v.type, loc);

        auto A = new llvm::GlobalVariable(*g_module, v.type, is_const,
              llvm::Function::InternalLinkage,
              init_res ?: get_default_value(v.type, loc), v.name);
        
        if (is_const)
          assert(llvm::isa<llvm::Constant>(A));

        // maintain the symbol table
        g_named_values[v.name].push(A);
      } else { // local
        if (is_const) {
          if (!I) return log_err("const variable must have a initial value.");
          g_named_values[v.name].push(get_calcuate_const_value(I, v.type, loc));
        } else {
          // for possible cast
          if (I) {
            auto p1 = get_type_prec(I->getType(), loc), p2 = get_type_prec(v.type, loc);
            if (p1 > p2) {
              if (p1 == 0x3f3f3f3fu) return log_err("the initial value type is not capable for the variable.");
              else I = g_builder->CreateTrunc(I, v.type);
            }
            if (I->getType()->isIntegerTy() && v.type->isDoubleTy())
              I = g_builder->CreateSIToFP(I, v.type);
          }

          auto A = g_builder->CreateAlloca(v.type, nullptr, v.name);
          if (I) g_builder->CreateStore(I, A);
          // maintain the symbol table
          g_named_values[v.name].push(A);
        }

      }
    }
    return llvm::UndefValue::get(llvm::Type::getVoidTy(*g_context));
  }

  list_t& get_list() { return lst; }
};

class BinExprNode : public LeftExprNode {
public:
  std::string op;
  expr_t left, right;

  BinExprNode(std::string op, expr_t left, expr_t right, SourceLoc loc)
    : op(op), left(std::move(left)), right(std::move(right)), LeftExprNode(loc) {  }

  llvm::Type* get_max_type() const {
    auto lp = get_type_prec(left->get_type(), loc), rp = get_type_prec(right->get_type(), loc);
    // auto &refa = (lp <= rp ? left : right), &refb = (lp <= rp ? right : left);
    return lp >= rp ? left->get_type() : right->get_type();
  }
  
  virtual llvm::Type* get_type() const override {
    if (op == ">" || op == "<" || op == "||" || op == "&&" || op == "<=" || op == ">=")
      return ty::getInt1Ty(*g_context);
    return get_max_type();
  }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"BinExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"operator\": \"" << op << "\", \"left\": ";
    left->output();
    os << ", \"right\": ";
    right->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    // special for assignment
    if (op == "=") {
      auto p1 = get_type_prec(left->get_type(), loc), p2 = get_type_prec(right->get_type(), loc);

      auto R = right->codegen();

      auto L = dynamic_cast<ILeftValue*>(left.get())->codegen_left();

      if (!L || !R) return nullptr;

      if (left->get_type()->isDoubleTy() && right->get_type()->isIntegerTy())
        R = g_builder->CreateSIToFP(R, left->get_type());

      if (p1 < p2) {
        if (p2 == 0x3f3f3f3fu) return log_err("invalid assignment!");
        else R = g_builder->CreateTrunc(R, left->get_type());
      }

      g_builder->CreateStore(R, L);
      // the value of assignment is rhs
      return R;
    }

    auto L = left->codegen(), R = right->codegen();
    if (!L || !R) return nullptr;

    // do not require two types are proper
    if (op == ",") return R;

    auto res_ty = ((get_type_prec(L->getType(), loc) > get_type_prec(R->getType(), loc) ? L->getType() : R->getType()));
    // get_type();
    
    if (res_ty->isDoubleTy()) {

      if (L->getType()->isIntegerTy())
        L = g_builder->CreateSIToFP(L, res_ty);
      if (R->getType()->isIntegerTy())
        R = g_builder->CreateSIToFP(R, res_ty);
      if (op == "+") return g_builder->CreateFAdd(L, R, "add");
      else if (op == "-") return g_builder->CreateFSub(L, R, "sub");
      else if (op == "*") return g_builder->CreateFMul(L, R, "mul");
      else if (op == "/") return g_builder->CreateFDiv(L, R, "div");
      else if (op == "%") return g_builder->CreateFRem(L, R, "mod");
      else if (op == "<") return g_builder->CreateFCmpULT(L, R, "lt");
      else if (op == ">") return g_builder->CreateFCmpUGT(L, R, "gt");
      else if (op == "<=") return g_builder->CreateFCmpULE(L, R, "le");
      else if (op == ">=") return g_builder->CreateFCmpUGE(L, R, "ge");
      else if (op == "==") return g_builder->CreateFCmpUEQ(L, R, "ueq");
      else if (op == "!=") return g_builder->CreateFCmpUNE(L, R, "une");
      else if (op == "+=") g_builder->CreateStore(R = g_builder->CreateFAdd(L, R, "add"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "-=") g_builder->CreateStore(R = g_builder->CreateFSub(L, R, "sub"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "*=") g_builder->CreateStore(R = g_builder->CreateFMul(L, R, "mul"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "/=") g_builder->CreateStore(R = g_builder->CreateFDiv(L, R, "div"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "%=") g_builder->CreateStore(R = g_builder->CreateFRem(L, R, "mod"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());

      if (op[1] == '=') return R;
    } else if (res_ty->isIntegerTy()) {

      L = g_builder->CreateIntCast(L, res_ty, true);
      R = g_builder->CreateIntCast(R, res_ty, true);

      if (op == "+") return g_builder->CreateAdd(L, R, "add");
      else if (op == "-") return g_builder->CreateSub(L, R, "sub");
      else if (op == "*") return g_builder->CreateMul(L, R, "mul");
      else if (op == "/") return g_builder->CreateSDiv(L, R, "div");
      else if (op == "%") return g_builder->CreateSRem(L, R, "mod");
      else if (op == "<") return g_builder->CreateICmpSLT(L, R, "lt");
      else if (op == ">") return g_builder->CreateICmpSGT(L, R, "gt");
      else if (op == "<=") return g_builder->CreateICmpSLE(L, R, "le");
      else if (op == ">=") return g_builder->CreateICmpSGE(L, R, "ge");
      else if (op == "==") return g_builder->CreateICmpEQ(L, R, "eq");
      else if (op == "!=") return g_builder->CreateICmpNE(L, R, "ne");
      else if (op == "&") return g_builder->CreateAnd(L, R, "and");
      else if (op == "|") return g_builder->CreateOr (L, R, "or");
      else if (op == "^") return g_builder->CreateXor(L, R, "xor");
      else if (op == "&&") return g_builder->CreateAnd(
          g_builder->CreateICmpNE(L, get_const_int_value(0, L->getType()->getIntegerBitWidth())),
          g_builder->CreateICmpNE(R, get_const_int_value(0, L->getType()->getIntegerBitWidth())),
          "and"
        );
      else if (op == "||") return g_builder->CreateOr(
          g_builder->CreateICmpNE(L, get_const_int_value(0, R->getType()->getIntegerBitWidth())),
          g_builder->CreateICmpNE(R, get_const_int_value(0, R->getType()->getIntegerBitWidth())),
          "or"
        );
      else if (op == "<<") return g_builder->CreateShl(L, R, "shl");
      else if (op == ">>") return g_builder->CreateLShr(L, R, "shr");

      else if (op == "+=") g_builder->CreateStore(R = g_builder->CreateAdd(L, R, "add"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "-=") g_builder->CreateStore(R = g_builder->CreateSub(L, R, "sub"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "*=") g_builder->CreateStore(R = g_builder->CreateMul(L, R, "mul"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "/=") g_builder->CreateStore(R = g_builder->CreateSDiv(L, R, "div"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "%=") g_builder->CreateStore(R = g_builder->CreateSRem(L, R, "mod"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());

      else if (op == "&=") g_builder->CreateStore(R = g_builder->CreateAnd(L, R, "and"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "|=") g_builder->CreateStore(R = g_builder->CreateOr (L, R, "or"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == "^=") g_builder->CreateStore(R = g_builder->CreateXor(L, R, "xor"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());

      else if (op == "<<=") g_builder->CreateStore(R = g_builder->CreateShl(L, R, "shl"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());
      else if (op == ">>=") g_builder->CreateStore(R = g_builder->CreateLShr(L, R, "shr"),
                                    dynamic_cast<ILeftValue*>(left.get())->codegen_left());

      if (op == "<<=" || op == ">>=" || op[1] == '=')
        return R;
    }

    return log_err("invalid binary operator.");
  }

  virtual bool is_left() const override {
    return (op == "=" && left->is_left())
        || (op == "," && right->is_left()) || (op[0] != '=' && op.back() == '=');
  }

  virtual llvm::Value* codegen_left() override {
    if (left->is_left()) {
      if (op.back() == '=')
        return dynamic_cast<ILeftValue*>(left.get())->codegen_left();
      else if (op == ",")
        return dynamic_cast<ILeftValue*>(right.get())->codegen_left();
    }
    
    return log_err("invalid left value expr!");
  }

};

class CallExprNode : public ExprNode, public INamedObject {
  std::string func;
  std::vector<expr_t> args;

public:
  CallExprNode(const std::string &func, std::vector<expr_t> args, SourceLoc loc)
    : func(func), args(std::move(args)), ExprNode(loc) {  }

  virtual llvm::Type* get_type() const override { return get_func(func)->getReturnType(); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"CallExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"func_name\": \"" << func << "\", \"args\": ";
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
        auto p1 = get_type_prec(V->getType(), loc), p2 = get_type_prec(it->getType(), loc);
        if (p1 > p2) {
          if (p1 == 0x3f3f3f3fu) return log_err("the param value type is not capable for the argument.");
          else V = g_builder->CreateTrunc(V, it->getType());
        }
        if (V->getType()->isIntegerTy() && it->getType()->isDoubleTy())
          V = g_builder->CreateSIToFP(V, it->getType());
        it++;
      }
      lv_args.push_back(V);
    }

    return l_func->getReturnType()->isVoidTy() ? 
            g_builder->CreateCall(l_func, lv_args) :
            g_builder->CreateCall(l_func, lv_args, "call");
  }

  virtual std::string get_name() const override {
    return func;
  }
};

class RetStmtNode : public StmtNode {
  expr_t val;
public:
  RetStmtNode(expr_t val, SourceLoc loc) : val(std::move(val)), StmtNode(loc) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"RetStmt\", \"loc\": [" << loc.line << ',' << loc.column << "], \"val\": ";
    val->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto V = val->codegen();
    if (!V) return nullptr;

    auto ret_ty = g_builder->getCurrentFunctionReturnType();
    if (ret_ty->isVoidTy())
      return g_builder->CreateRetVoid();
    auto p1 = get_type_prec(val->get_type(), loc), p2 = get_type_prec(ret_ty, loc);
    if (p1 > p2) {
      if (p1 == 0x3f3f3f3fu) return log_err("return value type is not capable for the function.");
      else V = g_builder->CreateTrunc(V, ret_ty);
    }
    if (ret_ty->isDoubleTy() && val->get_type()->isIntegerTy())
      V = g_builder->CreateSIToFP(V, ret_ty);

    g_builder->CreateStore(V, g_named_values["__return_value"].top());
    return g_builder->CreateBr(g_bl_ret);
  }
};

class BlockNode : public StmtNode {
  using list_t = std::vector<stmt_t>;
  list_t v;
public:
  BlockNode(list_t v, SourceLoc loc) : v(std::move(v)), StmtNode(loc) {  }
  virtual ~BlockNode() = default;
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Block\", \"loc\": [" << loc.line << ',' << loc.column << "], \"list\": ";
    output_list<stmt_t>(v, [](auto& x, auto& os){ x->output(os); }, os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    g_bl_now.push(this);

    llvm::Value* res = llvm::UndefValue::get(llvm::Type::getVoidTy(*g_context));

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
  virtual list_t& get_list() { return v; }
};

using block_t = std::unique_ptr<class BlockNode>;

class IfStmtNode : public StmtNode {
  expr_t cond;
  block_t then, els;

public:
  IfStmtNode(expr_t cond, block_t then, block_t els, SourceLoc loc)
    : cond(std::move(cond)), then(std::move(then)), els(std::move(els)), StmtNode(loc) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"IfStmt\", \"loc\": [" << loc.line << ',' << loc.column << "], \"cond\": ";
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
    if (vl_cond->getType() != llvm::Type::getInt1Ty(*g_context) || vl_cond->getType()->getIntegerBitWidth() != 1)
    {
      if (vl_cond->getType()->isIntegerTy())
        vl_cond = g_builder->CreateICmpNE(
          vl_cond,
          llvm::ConstantInt::get(vl_cond->getType(), 0),
          "ifcond");
      else if (vl_cond->getType()->isDoubleTy())
        vl_cond = g_builder->CreateFCmpONE(
          vl_cond,
          llvm::ConstantFP::get(llvm::Type::getDoubleTy(*g_context), 0),
          "ifcond");
      else return log_err("unsupported condition expression type.");
    }
    
    // func is the current block's parent, not absolutely really a function.
    auto func = g_builder->GetInsertBlock()->getParent();
    
    // bl_then is binded with func, so you needn't use func.gBBL().pb() for it
    auto bl_then = llvm::BasicBlock::Create(*g_context, "then", func),
         bl_else = llvm::BasicBlock::Create(*g_context, "else"),
         bl_merg = llvm::BasicBlock::Create(*g_context, "afterif");
    
    g_builder->CreateCondBr(vl_cond, bl_then, bl_else);
    // pour the newly created instructions into bl_then
    g_builder->SetInsertPoint(bl_then);
    
    // create instructions
    auto vl_then = then->codegen();
    if (!vl_then) return nullptr;

    // skip else, br to merg
    if (g_builder->GetInsertBlock()->getInstList().empty()
      || !g_builder->GetInsertBlock()->getInstList().back().isTerminator())
      g_builder->CreateBr(bl_merg);
    bl_then = g_builder->GetInsertBlock();

    // insert the else block
    func->getBasicBlockList().push_back(bl_else);
    g_builder->SetInsertPoint(bl_else);

    if (els) {
      llvm::Value* vl_else = els->codegen();
      if (!vl_else) return nullptr;
    }

    // Also
    if (g_builder->GetInsertBlock()->getInstList().empty()
      || !g_builder->GetInsertBlock()->getInstList().back().isTerminator())
      g_builder->CreateBr(bl_merg);

    if (bl_else->getInstList().empty()
      || !bl_else->getInstList().back().isTerminator())
      g_builder->SetInsertPoint(bl_else),
      g_builder->CreateBr(bl_merg);

    func->getBasicBlockList().push_back(bl_merg);
    g_builder->SetInsertPoint(bl_merg);

    return llvm::UndefValue::get(llvm::Type::getVoidTy(*g_context));
  }
};

class ForStmtNode : public StmtNode {
  stmt_t start, every;
  expr_t cond;
  block_t body;
public:
  ForStmtNode(stmt_t start, expr_t cond, stmt_t every, block_t body, SourceLoc loc) :
    start(std::move(start)), every(std::move(every)),
    cond(std::move(cond)), body(std::move(body)), StmtNode(loc) {  }
  
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"ForStmt\", \"loc\": [" << loc.line << ',' << loc.column << "], \"start\": ";
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
    auto func = g_builder->GetInsertBlock()->getParent();

    auto vl_start = start->codegen();
    if (!vl_start) return nullptr;
    
    auto bl_loop  = llvm::BasicBlock::Create(*g_context, "loop", func),
         bl_body  = llvm::BasicBlock::Create(*g_context, "body", func),
         bl_lend  = llvm::BasicBlock::Create(*g_context, "lend", func),
         bl_after = llvm::BasicBlock::Create(*g_context, "after", func);
    
    g_named_values["__loop_break"].push(bl_after);
    g_named_values["__loop_continue"].push(bl_lend);

    g_builder->CreateBr(bl_loop);
    g_builder->SetInsertPoint(bl_loop);

    // re-eval cond every loop
    auto vl_cond = cond->codegen();
    if (!vl_cond->getType()->isIntegerTy(1)) {
      if (vl_cond->getType()->isIntegerTy())
        vl_cond = g_builder->CreateICmpNE(
          vl_cond,
          llvm::ConstantInt::get(vl_cond->getType(), 0),
          "loopcond");
      else if (vl_cond->getType()->isDoubleTy())
        vl_cond = g_builder->CreateFCmpUNE(
          vl_cond,
          llvm::ConstantFP::get(llvm::Type::getDoubleTy(*g_context), 0),
          "loopcond");
      else return log_err("unsupported condition expression type.");
    }

    g_builder->CreateCondBr(vl_cond, bl_body, bl_after);

    g_builder->SetInsertPoint(bl_body);

    auto vl_body = body->codegen();
    if (!vl_body) return nullptr;

    g_builder->CreateBr(bl_lend);
    g_builder->SetInsertPoint(bl_lend);

    auto vl_every = every->codegen();
    if (!vl_every) return nullptr;

    g_builder->CreateBr(bl_loop);

    g_builder->SetInsertPoint(bl_after);
    
    if (instof<VarDeclNode>(*start)) {
      auto& lst = ((VarDeclNode*)start.get())->get_list();
      for (auto&& [var, t] : lst)
        g_named_values[var.name].pop();
    }
    g_named_values["__loop_break"].pop();
    g_named_values["__loop_continue"].pop();

    return llvm::UndefValue::get(llvm::Type::getVoidTy(*g_context));
  }
};

// classes for functions

class ProtoNode : public Node, public INamedObject {
  std::string id;
  tag_tok type;
  std::vector<Argu> args;
  bool var_args;
public:
  ProtoNode(std::string id, tag_tok type, std::vector<Argu> args, bool var_args, SourceLoc loc)
    : id(id), type(type), args(std::move(args)), var_args(var_args), Node(loc) {  }
  
  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }

  // { Prototype, "id": "cos", "type": "number", "args": ["number theta"] }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Prototype\", \"loc\": [" << loc.line << ',' << loc.column << "], \"id\": \"" << id << "\""
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

class FuncNode : public LeftExprNode, public INamedObject {
  proto_t proto; // it will be moved after codegen.
  block_t body; // what it will be calculated.

  std::string name; // record its name to find its proto.
public:
  FuncNode(proto_t proto, block_t body, SourceLoc loc)
    : proto(std::move(proto)), body(std::move(body)), name(this->proto->get_name()), LeftExprNode(loc) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }

  // { Function, { Prototype, $...$ }, { Body, "..." } }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Function\", \"loc\": [" << loc.line << ',' << loc.column << "], \"prototype\": ";
    if (proto) proto->output(os);
    else g_protos[name]->output(os);
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

    auto bas_blo = llvm::BasicBlock::Create(*g_context, "entry", fun);
    g_builder->SetInsertPoint(bas_blo);

    std::vector<std::string> extra_pop;

    for (auto& arg: fun->args()) {
      extra_pop.push_back(arg.getName().str());
      auto alloca = create_alloca_in_entry(
          fun, Var { arg.getName().str(), arg.getType() });
      g_builder->CreateStore(&arg, alloca);
      g_named_values[arg.getName().str()].push(alloca);
    }
    auto ret_ty = fun->getReturnType();
    llvm::AllocaInst* var_ret;
    if (!ret_ty->isVoidTy())
    {
      g_bl_ret = llvm::BasicBlock::Create(*g_context, "__return_block");
      var_ret = 
        create_alloca_in_entry(
          fun,   Var {
          "__return_value",
          ret_ty
        });
      
      g_named_values["__return_value"].push(var_ret);
      extra_pop.push_back("__return_value");
    }
    auto vl_body = body->codegen();
    if (vl_body) {
      for (auto &x:extra_pop) g_named_values[x].pop();

      if (ret_ty->isVoidTy())
        g_builder->CreateRetVoid();
      else {
        auto& ls = fun->getBasicBlockList().back().getInstList();
        bool term = ls.empty() | !ls.back().isTerminator();
        fun->getBasicBlockList().push_back(g_bl_ret);
        if (term) g_builder->CreateBr(g_bl_ret);
        g_builder->SetInsertPoint(g_bl_ret);
        g_builder->CreateRet(g_builder->CreateLoad(var_ret));
      }

      llvm::verifyFunction(*fun);

      // optimize it
      g_fpm->run(*fun);

      return fun;
    }
    fun->eraseFromParent();
    return nullptr;
  }

  virtual llvm::Value* codegen_left() override {
    auto fun = get_func(name);
    if (!fun) return log_err("unknown function name!");
    return fun;
  }

  virtual std::string get_name() const override { return name; }
  virtual const proto_t& get_proto() const {
    if (proto) return proto;
    else return g_protos[name];
  }
  virtual const block_t& get_body() const { return body; }
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

class UnaryExprNode : public LeftExprNode {
public:
  std::string op;
  expr_t operand;
  UnaryExprNode(std::string op, expr_t operand, SourceLoc loc) : op(op), operand(std::move(operand)), LeftExprNode(loc) {  }
  virtual llvm::Type* get_type() const override {
    if (auto t = operand->get_type()) {
      if (op == "*") {
        if (t->isPointerTy())
          return t->getPointerElementType();
        else return log_err("invalid disaddress expression operand type!");
      }
      if (op == "&") return t->getPointerTo();
      if (op == "!") return ty::getInt8Ty(*g_context);
      if (op == "~") return t;
      return log_err("unknown unary operator.");
    } else return log_err("invalid unary expression operand type!");
  }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"UnaryExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"operator\": '"
    << op << "', \"operand\": ";
    operand->output(os);
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    auto t = operand->get_type();
    if (op == "*") return g_builder->CreateLoad(operand->codegen());
    else if (op == "&") return dynamic_cast<ILeftValue*>(operand.get())->codegen_left();
    else if (op == "!") {
      if (t->isIntegerTy()) return g_builder->CreateICmpEQ(operand->codegen(), llvm::ConstantInt::get(t, 0));
      else if (t->isDoubleTy()) return g_builder->CreateFCmpOEQ(operand->codegen(), llvm::ConstantFP::get(t, 0));
      else return log_err("invalid logical not expression operand type!");
    } else if (op == "~") {
      if (!t->isIntegerTy()) return log_err("only integer can get bitwise not.");
        return g_builder->CreateNot(operand->codegen());
    } else return log_err("unknown unary operator.");
  }
  virtual llvm::Value* codegen_left() override {
    if (op == "*") return operand->codegen();
    else return log_err<std::logic_error>("this unary expression is not left value.");
  }
};

class SelfFrontCreExprNode : public LeftExprNode {
  expr_t left;
  bool is_add;
public:
  SelfFrontCreExprNode(expr_t left, bool is_add, SourceLoc loc)
      : left(std::move(left)), is_add(is_add), LeftExprNode(loc) {  }

  virtual llvm::Type* get_type() const override { return left->get_type(); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"Self" << (is_add ? "In" : "De") << "crementExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"left\": ";
    left->output(os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    if (!left->is_left())
      return log_err("invalid expression for addadd, expected a left value.");
    auto P = left->codegen();
    auto Q = g_builder->CreateAdd(P, llvm::ConstantInt::get(*g_context, llvm::APInt(32, is_add ? 1 : -1)), "heymine");
    auto L = dynamic_cast<ILeftValue*>(left.get())->codegen_left();
    g_builder->CreateStore(Q, L);

    return Q;
  }
  virtual llvm::Value* codegen_left() override {
    if (!left->is_left())
      return log_err("invalid expression for addadd, expected a left value.");
    return dynamic_cast<ILeftValue*>(left.get())->codegen_left();
  }
};

class SelfBackCreExprNode : public ExprNode {
  expr_t left;
  bool is_add;
public:
  SelfBackCreExprNode(expr_t left, bool is_add, SourceLoc loc)
      : left(std::move(left)), is_add(is_add), ExprNode(loc) {  }

  virtual llvm::Type* get_type() const override { return left->get_type(); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"SelfBack" << (is_add ? "In" : "De")
       << "crementExpr\", \"loc\": [" << loc.line << ',' << loc.column << "], \"left\": ";
    left->output(os);
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    auto P = left->codegen();
    auto Q = g_builder->CreateAdd(P, llvm::ConstantInt::get(*g_context, llvm::APInt(32, is_add ? 1 : -1)));
    auto L = dynamic_cast<ILeftValue*>(left.get())->codegen_left();
    g_builder->CreateStore(Q, L);

    return P;
  }
};

class LoopJumpStmtNode : public StmtNode {
public:
  bool is_break; // break or continue
  
  LoopJumpStmtNode(bool is_break, SourceLoc loc) : is_break(is_break), StmtNode(loc) {  }

  virtual llvm::Type* get_type() const override { return ty::getVoidTy(*g_context); }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node_type\": \"" << (is_break ? "BreakStmt" : "ContinueStmt") << "\", \"loc\": [" << loc.line << ',' << loc.column << "] }";
  }

  virtual llvm::Value* codegen() override {
    if (is_break) {
      if (g_named_values["__loop_break"].empty())
        return log_err("break statement should be used in the loop!");
      return g_builder->CreateBr(llvm::dyn_cast<llvm::BasicBlock>(g_named_values["__loop_break"].top()));
    } else {
      if (g_named_values["__loop_continue"].empty())
        return log_err("continue statement should be used in the loop!");
      return g_builder->CreateBr(llvm::dyn_cast<llvm::BasicBlock>(g_named_values["__loop_continue"].top()));
    }
  }
};

}