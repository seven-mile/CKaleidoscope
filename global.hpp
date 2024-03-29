#pragma once

#include <cstddef>
#include <memory>
#include <stack>
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
#include <llvm/IR/LegacyPassManager.h>

namespace zMile {

inline std::unique_ptr<llvm::LLVMContext> g_context;
inline std::unique_ptr<llvm::IRBuilder<>> g_builder;
inline std::unique_ptr<llvm::Module> g_module;
inline std::map<std::string, std::stack<llvm::Value*>> g_named_values;
inline std::unique_ptr<llvm::legacy::FunctionPassManager> g_fpm;
inline std::map<std::string, std::unique_ptr<class ProtoNode>> g_protos;
// nullptr: global block scope
inline std::stack<class BlockNode*> g_bl_now;
inline llvm::BasicBlock* g_bl_ret;

inline llvm::ExitOnError g_exit_err;
}
