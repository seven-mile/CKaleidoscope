#include <bits/stdc++.h>

#include "lexer.hpp"
#include "parser.hpp"

int main()
{
  zMile::LexerHelper lex(std::cin, true);
  zMile::ParserHelper par(lex);

  par.read_stream(true);

  return 0;
}
