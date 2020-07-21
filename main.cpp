#include <bits/stdc++.h>
#include <cstdio>
#include <llvm-9/llvm/Support/Error.h>

#include "global.hpp"
#include "ast.hpp"
#include "ioagent.hpp"
#include "lexer.hpp"
#include "jit.hpp"
#include "parser.hpp"
#include "optimizer.hpp"

int main(const int nargs, const char *cargs[])
{
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

  auto expr_main = zMile::g_jit->findSymbol("main");
  if (expr_main) {
    auto res = ((double(*)())(intptr_t)llvm::cantFail(
      expr_main.getAddress()))();
    std::cerr << "\nProgram exited with return value " << res << std::endl;
  }

  zMile::fin_jit_env();

  delete fs;

  return 0;
}
