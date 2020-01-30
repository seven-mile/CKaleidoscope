#pragma once

#include <iostream>
#include <fstream>
#include <filesystem>

namespace zMile {

// ((C6H7O2)===(ONO2)3)n qwq
class FileSugar
{
  std::istream& file;
  char last_char = ' ';

  std::vector<char> his;

public:
  FileSugar(std::istream &is) : file(is) {}

  std::istream& get_stream() { return file; }

  char operator~() { return last_char; }
  char operator!() { return his.push_back(last_char = file.get()), last_char; }
  void reset() { last_char = ' '; }
};

}
