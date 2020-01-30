#include <bits/stdc++.h>

#include "lexer.hpp"
#include "parser.hpp"

int main()
{
  zMile::Lexer lex(std::cin, true);
  zMile::Parser par(lex);

  par.read_stream();

  return 0;
}
