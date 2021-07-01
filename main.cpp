#include <bits/stdc++.h>
// #include <llvm/ExecutionEngine/MCJIT.h>
#include <cassert>
#include <llvm-12/llvm/Support/Host.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>

#include "binary.h"
#include "global.hpp"
#include "ast.hpp"
#include "ioagent.hpp"
#include "lexer.hpp"
#include "jit.hpp"
#include "parser.hpp"
#include "optimizer.hpp"

// WARNING
#include "stdlib.def"

/// usage: CKaleidoscope                    == command line interface
///        CKaleidoscope source_file        == run source_file with jit engine
///   todo CKaleidoscope source_file -o ... == compile source_file to executable file ...

int main(const int nargs, const char *cargs[])
{
  std::cout << "==============================\n";
  zMile::FileSugar* fs;

  // if (nargs > 2) return fprintf(
        // stderr, "arguments error: too many arguments.")*0-1;
  std::ifstream ifs;
  if (nargs >= 2) {
    ifs = std::ifstream(cargs[1]);
    if (!ifs) throw zMile::io_error(
          "failed to get fstream for the argument file.");
    fs = new zMile::FileSugar(ifs);
  }
  else fs = new zMile::FileSugar(std::cin);

  zMile::Lexer lex(*fs, nargs < 2);

  zMile::Parser par(lex);

  zMile::init_jit_env();

  // first module
  zMile::init_module_and_pass_mgr();

  par.read_stream(nargs < 2);


  if (nargs >= 2) {
    zMile::g_module->print(llvm::dbgs(), nullptr);

    bool broken;
    bool ret = llvm::verifyModule(*zMile::g_module, &llvm::dbgs(), &broken);

    llvm::dbgs() << llvm::format("[Default TargetTriple = %s]\n", llvm::sys::getDefaultTargetTriple().c_str());
    zMile::g_module->setTargetTriple(llvm::sys::getDefaultTargetTriple());

    if (nargs == 2) {
      // JIT run main
      auto ptrFunc = zMile::get_func("main");
      if (!ptrFunc->getReturnType()->isIntegerTy(32)
      || !ptrFunc->arg_empty()) {
        llvm::errs() << "\nunknown main function signature" << '\n';
        exit(1);
      }

      auto RT = zMile::g_jit->getMainJITDylib().createResourceTracker();
      auto TSM = llvm::orc::ThreadSafeModule(
        std::move(zMile::g_module), std::unique_ptr<llvm::LLVMContext>(&zMile::g_context));

      // add new module to submit the current module.
      zMile::g_exit_err(zMile::g_jit->addModule(std::move(TSM), RT));
      zMile::init_module_and_pass_mgr();

      if (auto expr_main = zMile::g_jit->lookup("main")) {
        if (auto ptr = expr_main->getAddress()) {
          auto res = ((int(*)())ptr)();
          llvm::errs() << "\nProgram exited with return value " << res << "." << '\n';
        } else {
          llvm::errs() << "\nNo proper main function, exiting." << '\n';
        }
      } else {
        llvm::logAllUnhandledErrors(expr_main.takeError(), llvm::errs(), "kaleido err: ");
        exit(1);
      }
    } else {
      // Compile the source code and output
      assert(cargs[2] == std::string("-o"));
      assert(nargs >= 4);
      if (CreateBinary(zMile::g_module.get(), cargs[3], EmitType::Executable)) {

      } else {
        llvm::errs() << llvm::format("Failed to compile the module into executable. [Output = %s]\n", cargs[3]);
      }

    }

  }

  zMile::fin_jit_env();

  delete fs;
  fs = nullptr;

  return 0;
}
