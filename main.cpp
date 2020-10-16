#include <bits/stdc++.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>

#include "global.hpp"
#include "ast.hpp"
#include "ioagent.hpp"
#include "lexer.hpp"
#include "jit.hpp"
#include "parser.hpp"
#include "optimizer.hpp"

// WARNING
#include "stdlib.cpp"

int main(const int nargs, const char *cargs[])
{
  std::cout << "==============================\n";
  zMile::FileSugar* fs;

  if (nargs > 2) return fprintf(
        stderr, "arguments error: too many arguments.")*0-1;
  std::ifstream ifs;
  if (nargs == 2) {
    ifs = std::ifstream(cargs[1]);
    if (!ifs) return zMile::log_err<zMile::io_error>(
          "failed to get fstream for the argument file."), 0;
    fs = new zMile::FileSugar(ifs);
  }
  else fs = new zMile::FileSugar(std::cin);

  zMile::Lexer lex(*fs, nargs ^ 2);

  zMile::Parser par(lex);

  zMile::init_jit_env();

  // first module
  zMile::init_module_and_pass_mgr();

  par.read_stream(nargs ^ 2);

  if (nargs == 2) {
    zMile::g_module->print(llvm::dbgs(), nullptr);

    bool broken;
    bool ret = llvm::verifyModule(*zMile::g_module, &llvm::dbgs(), &broken);

    // add new module to submit the current module.
    zMile::g_jit->addModule(std::move(zMile::g_module));
    zMile::init_module_and_pass_mgr();
  }

  auto expr_main = zMile::g_jit->findSymbol("main");
  if (auto ptr = expr_main.getAddress())
  {
    auto res = ((double(*)())*ptr)();
    std::cerr << "\nProgram exited with return value " << res << std::endl;
  }
  else {
    llvm::logAllUnhandledErrors(ptr.takeError(), llvm::errs(), "kaleido err: ");
    exit(1);
  }

  zMile::fin_jit_env();

  delete fs;
  fs = nullptr;

  return 0;
}
