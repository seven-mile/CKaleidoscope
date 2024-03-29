#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/IR/LegacyPassManager.h>
#include <memory>

#include "global.hpp"
#include "jit.hpp"

namespace zMile {

static inline void init_module_and_pass_mgr() {
  g_context = std::make_unique<llvm::LLVMContext>();
  
  g_module = std::make_unique<llvm::Module>("kaleido", *g_context);
  g_module->setDataLayout(g_jit->getDataLayout());

  g_builder = std::make_unique<llvm::IRBuilder<>>(*g_context);

  g_fpm = std::make_unique<llvm::legacy::FunctionPassManager>(g_module.get());

  g_fpm->add(llvm::createInstructionCombiningPass());
  g_fpm->add(llvm::createReassociatePass());
  g_fpm->add(llvm::createGVNPass());
  g_fpm->add(llvm::createCFGSimplificationPass());
  g_fpm->add(llvm::createSROAPass());
  g_fpm->add(llvm::createDeadCodeEliminationPass());
  g_fpm->add(llvm::createPromoteMemoryToRegisterPass());

  g_fpm->doInitialization();
}

}
