#pragma once

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
  g_module = std::make_unique<llvm::Module>("kaleido", g_context);
  g_module->setDataLayout(g_jit->getTargetMachine().createDataLayout());

  g_fpm = std::make_unique<llvm::legacy::FunctionPassManager>(g_module.get());

  g_fpm->add(llvm::createInstructionCombiningPass());
  g_fpm->add(llvm::createReassociatePass());
  g_fpm->add(llvm::createGVNPass());

  g_fpm->add(llvm::createCFGSimplificationPass());

  /// <summary>
  /// dead insts clear
  /// </summary>
  // g_fpm->add(llvm::createDeadInstEliminationPass());
  
  g_fpm->add(llvm::createSROAPass());
  g_fpm->add(llvm::createDeadInstEliminationPass());
  g_fpm->add(llvm::createPromoteMemoryToRegisterPass());

  g_fpm->doInitialization();
}

}
