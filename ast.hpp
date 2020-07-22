#pragma once

#include <iostream>
#include <llvm-9/llvm/IR/Value.h>
#include <llvm-9/llvm/Support/raw_ostream.h>
#include <map>
#include <memory>
#include <ostream>
#include <stdexcept>
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

inline llvm::Value* get_default_value(tag_tok t) {
  static llvm::Value
    *bl = llvm::ConstantInt::get(g_context, llvm::APInt(1, 0)),
    *ch = llvm::ConstantInt::get(g_context, llvm::APInt(8, 0)),
    *st = g_builder.CreateGlobalStringPtr(""),
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
  default:
    return nullptr; // log_err<std::logic_error>("no default value found!");
  }
}

inline llvm::Function* get_func(std::string name);

inline llvm::AllocaInst* create_alloca_in_entry(
    llvm::Function *func, const std::string var);

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

class Node : public Outputable, ValueGenable {
public:
  virtual ~Node() = default;
  virtual llvm::Value* codegen() = 0;
};

using node_t = std::unique_ptr<Node>;

using StmtNode = Node;
using stmt_t = std::unique_ptr<StmtNode>;


using ExprNode = Node;
using expr_t = std::unique_ptr<ExprNode>;

class DeclNode : public StmtNode {
public:
  virtual ~DeclNode() = default;
  virtual llvm::Value* codegen() = 0;
};

// For var

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

inline llvm::AllocaInst* create_alloca_in_entry(
    llvm::Function *func, const Var var)
{
  llvm::IRBuilder<> b_(&func->getEntryBlock(), func->getEntryBlock().begin());
  return b_.CreateAlloca(get_type_of_tok(var.type), nullptr, var.name);
}

class BlockNode : public StmtNode {
  using list_t = std::vector<stmt_t>;
  list_t v;
  std::vector<std::string> named_values;
public:
  BlockNode(list_t v) : v(std::move(v)) { named_values.clear(); }
  virtual ~BlockNode() = default;
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ BlockNode, \"list\": ";
    output_list<stmt_t>(v, [](auto& x, auto& os){ x->output(os); });
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    g_bl_now = this;

    for (auto &x:v) x->codegen();

    for (auto &x:named_values) g_named_values[x].pop();

    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }
  virtual std::vector<std::string>& get_nv() { return named_values; }
};
using block_t = std::unique_ptr<BlockNode>;

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
    llvm::Value* ptr = g_named_values[id].top();
    if (!ptr) return log_err("unknown variable name.");

    return g_builder.CreateLoad(ptr, id);
  }

  std::string get_name() const { return id; }
};

class VarDeclNode : public DeclNode {
  using ele_t = std::pair<Var, expr_t>;
  using list_t = std::vector<ele_t>;
  list_t lst;

public:
  VarDeclNode(list_t& lst) : lst(std::move(lst)) {  }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ VarDeclNode, \"lst\": ";
    output_list<ele_t>(lst, [](auto& x, auto& os) {
      os << "{ \"name\": \"" << x.first.name << "\", "
         << "\"type\": \"" << map_tok[x.first.type] << "\", "
         << "\"init\": ";
      if (x.second) x.second->output();
      else os << "null";
      os << " }";
    });
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    for (auto&& [v, init] : lst) {
      // codegen: v = init
      auto I = init ? init->codegen() : 
        get_default_value(v.type);
      if (init && !I) return nullptr;
      auto A = g_builder.CreateAlloca(get_type_of_tok(v.type), nullptr, v.name);
      if (I) g_builder.CreateStore(I, A);
      // maintain the symbol table
      g_named_values[v.name].push(A);
      // record itself for later erasing
      g_bl_now->get_nv().push_back(v.name);
    }
    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
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

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ BinExpr, \"operator\": '" << op << "', \"left\": ";
    left->output();
    os << ", \"right\": ";
    right->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    // special for assignment
    if (op == '=') {
      auto R = right->codegen();

      auto L = g_named_values[dynamic_cast<VarExprNode*>(left.get())->get_name()].top();
      if (!L || !R) return nullptr;
      g_builder.CreateStore(R, L);
      // the value of assignment is rhs
      return R;
    }

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
    case -'<':
      return g_builder.CreateFCmpULE(L, R, "le");
    case -'>':
      return g_builder.CreateFCmpUGE(L, R, "ge");
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
    
    if ((l_fun->isVarArg() && args.size() < l_fun->arg_size()) ||
         (!l_fun->isVarArg() && args.size() != l_fun->arg_size()))
            return log_err("incorrect # arguments passed.");

    std::vector<llvm::Value*> lv_args;
    for (size_t i = 0, e = args.size(); i != e; i++) {
      lv_args.push_back(args[i]->codegen());
      if (!lv_args.back()) return nullptr;
    }

    return g_builder.CreateCall(l_fun, lv_args, "call");
  }
};

class IfStmtNode : public StmtNode {
  expr_t cond;
  block_t then, els;

public:
  IfStmtNode(expr_t cond, block_t then, block_t els)
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
    auto ref1 = g_builder.CreateBr(bl_merg);
    bl_then = g_builder.GetInsertBlock();

    // insert the else block
    func->getBasicBlockList().push_back(bl_else);
    g_builder.SetInsertPoint(bl_else);

    llvm::Value* vl_else = els->codegen();
    if (!vl_else) return nullptr;

    // Also
    auto ref2 = g_builder.CreateBr(bl_merg);
    bl_else = g_builder.GetInsertBlock();

    func->getBasicBlockList().push_back(bl_merg);
    g_builder.SetInsertPoint(bl_merg);

    g_ref_bb[bl_merg->getName()].push_back(ref1);
    g_ref_bb[bl_merg->getName()].push_back(ref2);

    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }
};

class ForExprNode : public ExprNode {
  stmt_t start, every;
  expr_t cond;
  block_t body;
  bool hasVar;
public:
  ForExprNode(stmt_t start, expr_t cond, stmt_t every, block_t body, bool hasVar) :
    start(std::move(start)), cond(std::move(cond)),
    every(std::move(every)), body(std::move(body)), hasVar(hasVar) {  }
  
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ ForStmt, \"start\": ";
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

    if (hasVar) {
      auto& lst = ((VarDeclNode*)start.get())->get_list();
      for (auto&& [var, t] : lst)
        g_named_values[var.name].push((llvm::AllocaInst*)vl_start);
    }


    auto vl_cond = cond->codegen();
    if (vl_cond->getType()->getTypeID() != llvm::Type::getInt1Ty(g_context)->getTypeID())
    vl_cond = g_builder.CreateFCmpONE(
      vl_cond,
      llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), 0),
      "ifcond");
    
    auto bl_loop  = llvm::BasicBlock::Create(g_context, "loop", func),
         bl_body  = llvm::BasicBlock::Create(g_context, "body", func),
         bl_lend  = llvm::BasicBlock::Create(g_context, "lend", func),
         bl_after = llvm::BasicBlock::Create(g_context, "after", func);

    g_builder.CreateBr(bl_loop);
    g_builder.SetInsertPoint(bl_loop);

    g_builder.CreateCondBr(vl_cond, bl_body, bl_after);

    g_builder.SetInsertPoint(bl_body);

    auto vl_body = body->codegen();
    if (!vl_body) return nullptr;

    g_builder.CreateBr(bl_lend);
    g_builder.SetInsertPoint(bl_lend);

    auto vl_every = body->codegen();
    if (!vl_every) return nullptr;

    g_ref_bb[bl_loop->getName()].push_back(g_builder.CreateBr(bl_loop));

    g_builder.SetInsertPoint(bl_after);
    

    if (hasVar) {
      auto& lst = ((VarDeclNode*)start.get())->get_list();
      for (auto&& [var, t] : lst)
        g_named_values[var.name].pop();
    }

    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }
};

class RetStmtNode : public StmtNode {
  expr_t val;
public:
  RetStmtNode(expr_t val) : val(std::move(val)) {  }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ RetStmtNode, \"val\": ";
    val->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    return g_builder.CreateRet(val->codegen());
  }
};

// classes for functions

class ProtoNode : public Outputable, FuncGenable {
  std::string id;
  tag_tok type;
  std::vector<Argu> args;
  bool var_args;
public:
  ProtoNode(std::string id, tag_tok type, std::vector<Argu> args, bool var_args)
    : id(id), type(type), args(std::move(args)), var_args(var_args) {  }
  
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
    
    auto l_fun_ty = llvm::FunctionType::get(get_type_of_tok(type), vArgsTy, var_args);
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
  block_t body; // what it will be calculated.
public:
  FuncNode(proto_t proto,
    block_t body) : proto(std::move(proto)),
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

    
    for (auto& arg: fun->args()) {
      body->get_nv().push_back(arg.getName());
      auto alloca = create_alloca_in_entry(
          fun, Var { arg.getName(), find_value_type(arg.getType()) });
      g_builder.CreateStore(&arg, alloca);
      g_named_values[arg.getName()].push(alloca);
    }

    auto vl_body = body->codegen();
    if (vl_body) {
      auto & bbl = fun->getBasicBlockList();

      // llvm doesn't allow dead(empty) blocks,
      // so we are to remove them.
      // since removing may lead to more dead blocks,
      // we simply repeat it.
      while (1) {
        // if there's no other dead blocks, break;
        bool flg = 1;
        size_t sz = bbl.size(), cnt = 0;
        for (auto& x:bbl)
        {
          if (++cnt > sz) break;
          if (x.empty()) {
            flg = 0;
            auto &ref = g_ref_bb[x.getName()];
            auto szz = ref.size();
            for (auto& y:ref)
              y->removeFromParent();
            fun->getBasicBlockList().erase(x);
          }
        }
        if (flg) break;
      }

      g_module->print(llvm::errs(), nullptr);

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
