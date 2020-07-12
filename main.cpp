#include <bits/stdc++.h>

#define LLVM_ENABLE_DUMP

#include "global.hpp"
#include "ast.hpp"
#include "lexer.hpp"
#include "jit.hpp"
#include "parser.hpp"
#include "optimizer.hpp"

int main(const int nargs, const char *cargs[])
{
  // std::ifstream ifs("/home/7mile/repos/Kaleidoscope/build/input.in");

  // if (!ifs)
    // zMile::log_err<zMile::io_error>("input file error!");

  zMile::Lexer lex(std::cin, true);
  // zMile::Lexer lex(ifs, true);
  zMile::Parser par(lex);

  zMile::init_jit_env();

  // first module
  zMile::init_module_and_pass_mgr();

  par.read_stream();

  zMile::fin_jit_env();

  return 0;
}
