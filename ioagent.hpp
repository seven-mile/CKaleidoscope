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

public:
  FileSugar(std::istream &is) : file(is) {}

  std::istream& get_stream() { return file; }

  char operator~() { return last_char; }
  char operator!() { return last_char = file.get(); }
  void reset() { last_char = ' '; }
};

}
