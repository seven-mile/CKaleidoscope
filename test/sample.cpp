#include "llvm/ADT/APInt.h"
#include "llvm/IR/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstdlib>
#include <llvm-9/llvm/IR/GlobalValue.h>
#include <llvm-9/llvm/IR/GlobalVariable.h>
#include <llvm-9/llvm/IR/IRBuilder.h>
#include <llvm-9/llvm/IR/Operator.h>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

static GlobalVariable *CreateFibsGlobalVariable(Module *M, LLVMContext &Context) {
  auto aggr_type = ArrayType::get(Type::getInt32Ty(Context), 12);
  auto gv = new GlobalVariable(*M, aggr_type, false,
    GlobalVariable::CommonLinkage, ConstantAggregateZero::get(aggr_type), "fibs");
  return gv;
}

static Function *CreateFibFunction(Module *M, LLVMContext &Context, IRBuilder<> &Builder, GlobalVariable* GVFibs) {
  // Create the fib function and insert it into module M. This function is said
  // to return an int and take an int parameter.
  FunctionType *FibFTy = FunctionType::get(Type::getInt32Ty(Context),
                                           {Type::getInt32Ty(Context)}, false);
  Function *FibF =
      Function::Create(FibFTy, Function::ExternalLinkage, "fib", M);

  // Add a basic block to the function.
  BasicBlock *BB = BasicBlock::Create(Context, "EntryBlock", FibF);

  // Get pointers to the constants.
  Value *One = ConstantInt::get(Type::getInt32Ty(Context), 1);
  Value *Two = ConstantInt::get(Type::getInt32Ty(Context), 2);

  // Get pointer to the integer argument of the add1 function...
  Argument *ArgX = &*FibF->arg_begin(); // Get the arg.
  ArgX->setName("AnArg");            // Give it a nice symbolic name for fun.

  // Create the true_block.
  BasicBlock *RetBB = BasicBlock::Create(Context, "return", FibF);
  // Create an exit block.
  BasicBlock* RecurseBB = BasicBlock::Create(Context, "recurse", FibF);

  // Create the "if (arg <= 2) goto exitbb"
  Value *CondInst = new ICmpInst(*BB, ICmpInst::ICMP_SLE, ArgX, Two, "cond");
  BranchInst::Create(RetBB, RecurseBB, CondInst, BB);

  // Create: ret int 1
  ReturnInst::Create(Context, One, RetBB);

  // create fib(x-1)
  Value *Sub = BinaryOperator::CreateSub(ArgX, One, "arg", RecurseBB);
  CallInst *CallFibX1 = CallInst::Create(FibF, Sub, "fibx1", RecurseBB);
  CallFibX1->setTailCall();

  // create fib(x-2)
  Sub = BinaryOperator::CreateSub(ArgX, Two, "arg", RecurseBB);
  CallInst *CallFibX2 = CallInst::Create(FibF, Sub, "fibx2", RecurseBB);
  CallFibX2->setTailCall();

  // fib(x-1)+fib(x-2)
  Value *Sum = BinaryOperator::CreateAdd(CallFibX1, CallFibX2,
                                         "addresult", RecurseBB);
  // auto GEPX = GetElementPtrInst::Create(GVFibs->getType()->getPointerElementType(), GVFibs, {ArgX}, "GetIdx", RecurseBB);
  Builder.SetInsertPoint(RecurseBB);
  // auto SSSx = Builder.CreateAdd(Sum, CallFibX1);
  auto GEPX = Builder.CreateGEP(GVFibs, { ConstantInt::get(Context, APInt(32, 0)), ArgX }, "gepp");
  // auto GEPX = GetElementPtrInst::Create(GVFibs->getType(), GVFibs, { ConstantInt::get(Context, APInt(32, 0)), ArgX });

  dbgs() << "========\n";
  dbgs() << *GEPX << "\n";
  dbgs() << "========\n";

  // Value *EqInst = new StoreInst(Sum, GEPX, RecurseBB);
  Builder.CreateStore(Sum, GEPX);

  // Create the return instruction and add it to the basic block
  ReturnInst::Create(Context, Sum, RecurseBB);

  return FibF;
}

int main(int argc, char **argv) {
  int n = 30;//argc > 1 ? atol(argv[1]) : 24;

  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  LLVMContext Context;
  IRBuilder<> Builder(Context);

  // Create some module to put our function into it.
  std::unique_ptr<Module> Owner(new Module("test", Context));
  Module *M = Owner.get();

  // Create global array to memorize.
  GlobalVariable *GVFibs = CreateFibsGlobalVariable(M, Context);

  // We are about to create the "fib" function:
  Function *FibF = CreateFibFunction(M, Context, Builder, GVFibs);

  // Now we going to create JIT
  std::string errStr;
  ExecutionEngine *EE =
    EngineBuilder(std::move(Owner))
    .setErrorStr(&errStr)
    .create();

  if (!EE) {
    errs() << argv[0] << ": Failed to construct ExecutionEngine: " << errStr
           << "\n";
    return 1;
  }

  outs() << *M << "\n";

  errs() << "verifying... ";
  if (verifyModule(*M)) {
    errs() << argv[0] << ": Error constructing function!\n";
    return 1;
  }

  errs() << "OK\n";
  errs() << "We just constructed this LLVM module:\n\n---------\n" << *M;
  errs() << "---------\nstarting fibonacci(" << n << ") with JIT...\n";

  int* addr = (int*)EE->getGlobalValueAddress("fibs");
  printf("x: %p\n", (void*)addr);

  for (int i=1; i<=12; i++) {
    printf("fibs[%d] = %d\n", i, addr[i]);
  }

  // Call the Fibonacci function with argument n:
  std::vector<GenericValue> Args(1);
  Args[0].IntVal = APInt(32, n);
  GenericValue GV = EE->runFunction(FibF, Args);

  // import result of execution
  outs() << "Result: " << GV.IntVal << "\n";

  return 0;
}