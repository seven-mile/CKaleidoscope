#include <bits/stdc++.h>

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

  if (nargs > 2) return fprintf(stderr, "arguments error: too many arguments.")*0-1;
  std::ifstream ifs;
  if (nargs == 2) {
    ifs = std::ifstream(cargs[1]);
    if (!ifs) return zMile::log_err<zMile::io_error>("failed to get fstream for the argument file."), 0;
    fs = new zMile::FileSugar(ifs);
  }
  else fs = new zMile::FileSugar(std::cin);

  zMile::Lexer lex(*fs, nargs ^ 2);

  zMile::Parser par(lex);

  zMile::init_jit_env();

  // first module
  zMile::init_module_and_pass_mgr();

  par.read_stream(nargs ^ 2);

  zMile::fin_jit_env();

  delete fs;

  return 0;
}
