#pragma once

#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <vector>
#include <functional>

#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
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

#include "global.hpp"
#include "lexer.hpp"
#include "optimizer.hpp"

#include "magic_enum.hpp"

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
// AST Node
// =========================

enum NodeType : int {
  BlockNodeTy,
  NumExprNodeTy,
  CharExprNodeTy,
  StringExprNodeTy,
  BoolExprNodeTy,
  VarExprNodeTy,
  VarDeclNodeTy,
  BinExprNodeTy,
  CallExprNodeTy,
  IfStmtNodeTy,
  ForStmtNodeTy,
  RetStmtNodeTy,
  ProtoNodeTy,
  FuncNodeTy,
};

class IOutputable {
public:
  virtual void output(std::ostream & os = std::cerr) = 0;
};

class IValueGenable {
public:
  virtual llvm::Value* codegen() = 0;
};

class INameGetable {
public:
  virtual std::string get_name() const = 0;
};

class IHasType {
public:
  virtual NodeType get_type() const = 0;
  virtual std::string_view get_type_str() const {
    return magic_enum::enum_name(get_type());
  }
};

class Node : public IOutputable, public IValueGenable, public IHasType, public INameGetable {
public:
  virtual ~Node() = default;
  virtual llvm::Value* codegen() override = 0;
  virtual NodeType get_type() const override = 0;
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
  bool rb; // require_brace
public:
  BlockNode(list_t v, bool rb = false) : v(std::move(v)), rb(rb) { named_values.clear(); }
  virtual ~BlockNode() = default;
  virtual NodeType get_type() const override { return NodeType::BlockNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ BlockNode, \"list\": ";
    output_list<stmt_t>(v, [](auto& x, auto& os){ x->output(os); });
    os << " }";
  }
  virtual llvm::Value* codegen() override {
    g_bl_now.push(this);

    auto func = g_builder.GetInsertBlock()->getParent();
    auto ret_ty = func->getReturnType();
    llvm::AllocaInst* var_ret;
    if (rb && !ret_ty->isVoidTy())
    {
      g_bl_ret = llvm::BasicBlock::Create(g_context, "__return_block");
      var_ret = 
        create_alloca_in_entry(
          func,   Var {
          "__return_value",
          find_value_type(ret_ty)
        });
      
      g_named_values["__return_value"].push(var_ret);
      named_values.push_back("__return_value");
    }

    for (auto &x:v) {
      x->codegen();
      if (x->get_type() == RetStmtNodeTy)
        break; // ignore the following stmts.
    }
    for (auto &x:named_values) g_named_values[x].pop();

    if (rb) {
      if (ret_ty->isVoidTy())
        g_builder.CreateRetVoid();
      else {
        auto& ls = func->getBasicBlockList().back().getInstList();
        bool term = ls.empty() | !ls.back().isTerminator();
        func->getBasicBlockList().push_back(g_bl_ret);
        if (term) g_builder.CreateBr(g_bl_ret);
        g_builder.SetInsertPoint(g_bl_ret);
        g_builder.CreateRet(g_builder.CreateLoad(var_ret));
      }
    }

    return llvm::UndefValue::get(llvm::Type::getVoidTy(g_context));
  }
  virtual std::string get_name() const override {
    return "block";
  }
  auto& get_nv() { return named_values; }
};
using block_t = std::unique_ptr<BlockNode>;

class NumExprNode : public ExprNode {
  double val;

public:
  NumExprNode(double val) : val(val) {  }
  virtual NodeType get_type() const override { return NodeType::NumExprNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"NumExpr\", \"val\": " << val << " }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantFP::get(g_context, llvm::APFloat(val));
  }
  virtual std::string get_name() const override {
    return "number";
  }
};

class CharExprNode : public ExprNode {
  char val;

public:
  CharExprNode(char val) : val(val) {  }
  virtual NodeType get_type() const override { return NodeType::CharExprNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"CharExpr\", \"val\": '" << val << "' }";
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
  virtual NodeType get_type() const override { return NodeType::StringExprNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    // we need to display raw string
    os << "{ \"node type\": \"StringExpr\", \"val\": \"" << get_raw_string(val) << "\" }";
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
  virtual NodeType get_type() const override { return NodeType::BoolExprNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"BoolExpr\", \"val\": " << (val ? "true" : "false") << " }";
  }
  virtual llvm::Value* codegen() override {
    return llvm::ConstantInt::get(g_context, llvm::APInt(1, val));
  }
  virtual std::string get_name() const override {
    return val ? "true" : "false";
  }
};

class VarExprNode : public ExprNode {
  std::string id;

public:
  VarExprNode(const std::string &id) : id(id) {  }
  virtual NodeType get_type() const override { return NodeType::VarExprNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"VarExpr\", \"id\": \"" << id << "\" }";
  }

  virtual llvm::Value* codegen() override {
    llvm::Value* ptr = g_named_values[id].top();
    if (!ptr) return log_err("unknown variable name.");

    return g_builder.CreateLoad(ptr, id);
  }

  virtual std::string get_name() const override { return id; }
};

class VarDeclNode : public DeclNode {
  using ele_t = std::pair<Var, expr_t>;
  using list_t = std::vector<ele_t>;
  list_t lst;

public:
  VarDeclNode(list_t& lst) : lst(std::move(lst)) {  }
  virtual NodeType get_type() const override { return NodeType::VarDeclNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"VarDeclNode\", \"lst\": ";
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
      g_bl_now.top()->get_nv().push_back(v.name);
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
  
  virtual NodeType get_type() const override { return NodeType::BinExprNodeTy; }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"BinExpr\", \"operator\": '" << op << "', \"left\": ";
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

  virtual NodeType get_type() const override { return NodeType::CallExprNodeTy; }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"CallExpr\", \"func_name\": \"" << func << "\", \"args\": ";
    output_list<expr_t>(args, [](auto &x, auto &os){ x->output(os); });
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    llvm::Function *l_func = get_func(func);
    if (!l_func) return log_err("unknown function reference.");
    
    if ((l_func->isVarArg() && args.size() < l_func->arg_size()) ||
         (!l_func->isVarArg() && args.size() != l_func->arg_size()))
            return log_err("incorrect # arguments passed.");

    std::vector<llvm::Value*> lv_args;
    for (size_t i = 0, e = args.size(); i != e; i++) {
      lv_args.push_back(args[i]->codegen());
      if (!lv_args.back()) return nullptr;
    }

    return g_builder.CreateCall(l_func, lv_args, "call");
  }

  virtual std::string get_name() const override {
    return func;
  }
};

class IfStmtNode : public StmtNode {
  expr_t cond;
  block_t then, els;

public:
  IfStmtNode(expr_t cond, block_t then, block_t els)
    : cond(std::move(cond)), then(std::move(then)), els(std::move(els)) {  }

  virtual NodeType get_type() const override { return NodeType::IfStmtNodeTy; }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"IfStmt\", \"cond\": ";
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
    if (!g_builder.GetInsertBlock()->getInstList()
                          .back().isTerminator())
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
    if (!g_builder.GetInsertBlock()->getInstList()
                          .back().isTerminator())
      g_builder.CreateBr(bl_merg);
    bl_else = g_builder.GetInsertBlock();

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
  
  virtual NodeType get_type() const override { return NodeType::ForStmtNodeTy; }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"ForStmt\", \"start\": ";
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
    if (!vl_cond->getType()->isIntegerTy(1))
      vl_cond = g_builder.CreateFCmpONE(
        vl_cond,
        llvm::ConstantFP::get(llvm::Type::getDoubleTy(g_context), 0),
        "ifcond");

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
    
    if (start->get_type() == NodeType::VarDeclNodeTy) {
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

class RetStmtNode : public StmtNode {
  expr_t val;
public:
  RetStmtNode(expr_t val) : val(std::move(val)) {  }

  virtual NodeType get_type() const override { return NodeType::RetStmtNodeTy; }

  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"RetStmt\", \"val\": ";
    val->output();
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    g_builder.CreateStore(val->codegen(), g_named_values["__return_value"].top());
    return g_builder.CreateBr(g_bl_ret);
  }

  virtual std::string get_name() const override {
    return val->get_name();
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
  
  virtual NodeType get_type() const override { return NodeType::ProtoNodeTy; }

  // { Prototype, "id": "cos", "type": "number", "args": ["number theta"] }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"Prototype\", \"id\": \"" << id << "\""
       << ", \"type\": \"" << map_tok[type] << "\"";
    if (args.size()) {
      os << ", \"args\": ";
      output_list<Argu>(args, [](auto &x, auto &os){ os << '"' << x.to_string(true) << '"'; });
    }
    os << " }";
  }

  virtual llvm::Value* codegen() override {
    std::vector<llvm::Type*> vArgsTy;
    for (const auto& x:args)
      vArgsTy.push_back(get_type_of_tok(x.type));
    
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

class FuncNode : public Node {
  proto_t proto;
  block_t body; // what it will be calculated.
public:
  FuncNode(proto_t proto,
    block_t body) : proto(std::move(proto)),
      body(std::move(body)) {  }

  virtual NodeType get_type() const override { return NodeType::FuncNodeTy; }

  // { Function, { Prototype, $...$ }, { Body, "..." } }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"Function\", \"prototype\": ";
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

    for (auto& arg: fun->args()) {
      body->get_nv().push_back(arg.getName());
      auto alloca = create_alloca_in_entry(
          fun, Var { arg.getName(), find_value_type(arg.getType()) });
      g_builder.CreateStore(&arg, alloca);
      g_named_values[arg.getName()].push(alloca);
    }
    auto vl_body = body->codegen();
    if (vl_body) {
      llvm::verifyFunction(*fun);
      // optimize it
      g_fpm->run(*fun);

      return fun;
    }
    fun->eraseFromParent();
    return nullptr;
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

class AddressExprNode : public ExprNode {
  expr_t operand;
public:
  AddressExprNode(expr_t operand) : operand(std::move(operand)) {  }
  virtual NodeType get_type() const override { return NodeType::VarExprNodeTy; }
  virtual void output(std::ostream & os = std::cerr) override {
    os << "{ \"node type\": \"VarExpr\", \"operand\": \"";
    if (operand->get_type() == VarExprNodeTy)
      os << ((VarExprNode*)(operand.get()))->get_name();
    else os << ((FuncNode*)(operand.get()))->get_name();
    os << "\" }";
  }

  virtual llvm::Value* codegen() override {
    if (operand->get_type() == VarExprNodeTy)
      return g_named_values[get_name()].top();
    else return get_func(get_name());
  }
  virtual std::string get_name() const override {
    if (operand->get_type() == VarExprNodeTy)
      return ((VarExprNode*)(operand.get()))->get_name();
    else return ((FuncNode*)(operand.get()))->get_name();
  }
};

}
