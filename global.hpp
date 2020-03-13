#pragma once

#include <memory>
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

llvm::LLVMContext g_context;
llvm::IRBuilder<> g_builder(g_context);
std::unique_ptr<llvm::Module> g_module;
std::map<std::string, llvm::Value*> g_named_values;
std::unique_ptr<llvm::legacy::FunctionPassManager> g_fpm;
std::map<std::string, std::unique_ptr<class ProtoNode>> g_protos;

}
