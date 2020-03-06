#include <bits/stdc++.h>

#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

int main()
{
  zMile::Lexer lex(std::cin, true);
  zMile::Parser par(lex);

  zMile::g_module = std::make_unique<llvm::Module>("kaleido", zMile::g_context);

  par.read_stream();

  zMile::g_module->print(llvm::errs(), nullptr);

  return 0;
}
