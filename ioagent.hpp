#pragma once

#include "errdef.hpp"

#include <iostream>
#include <fstream>
#include <vector>

namespace zMile {

struct SourceLoc {
  int line, column;
  SourceLoc(int line, int column) : line(line), column(column) {  }
};

// ((C6H7O2)===(ONO2)3)n
class FileSugar
{
  std::istream& file;
  char last_char = ' ';

  std::vector<char> his;

public:
  FileSugar(std::istream &is) : file(is) {}

  std::istream& get_stream() { return file; }

  SourceLoc cur_loc = {1, 0};
  char operator~() { return last_char; }
  char operator!() {
    if (his.size() >= 1000) his.clear();
    if (last_char == '\n')
      cur_loc.line++, cur_loc.column = 1;
    else cur_loc.column++;

    return his.push_back(last_char = file.get()), last_char;
  }
  char peek() { return file.peek(); }
  void reset() { last_char = ' '; }
};

}
