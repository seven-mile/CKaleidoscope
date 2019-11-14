#include <bits/stdc++.h>
#include "ioagent.hpp"
#include "lexer.hpp"

int main()
{
  zMile::FileSugar sugar(std::cin);
  zMile::LexerHelper hlp(sugar);
  while(1) {
    auto tok = hlp.get_token();
    tok.print();
  }


}