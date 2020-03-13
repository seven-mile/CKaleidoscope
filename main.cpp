#include <bits/stdc++.h>
#include "global.hpp"
#include "ast.hpp"
#include "lexer.hpp"
#include "jit.hpp"
#include "parser.hpp"
#include "optimizer.hpp"

int main()
{
  zMile::Lexer lex(std::cin, true);
  zMile::Parser par(lex);

  zMile::init_jit_env();

  // first module
  zMile::init_module_and_pass_mgr();

  par.read_stream();

  return 0;
}
