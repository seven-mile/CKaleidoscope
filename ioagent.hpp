#pragma once

#include <iostream>
#include <fstream>
#include <vector>

namespace zMile {

// ((C6H7O2)===(ONO2)3)n
class FileSugar
{
  std::istream& file;
  char last_char = ' ';

  std::vector<char> his;

public:
  FileSugar(std::istream &is) : file(is) {}

  std::istream& get_stream() { return file; }

  char operator~() { return last_char; }
  char operator!() { if (his.size() >= 1000) his.clear(); return his.push_back(last_char = file.get()), last_char; }
  char peek() { return file.peek(); }
  void reset() { last_char = ' '; }
};

}
