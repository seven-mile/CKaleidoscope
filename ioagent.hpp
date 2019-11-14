#pragma once

#include <iostream>
#include <fstream>
#include <filesystem>

namespace zMile {

class FileSugar
{
  std::istream& file;
  char last_char = ' ';

public:
  FileSugar(std::istream &is) : file(is) {}

  std::istream& get_stream() { return file; }

  char operator~() { return last_char; }
  char operator!() { return last_char = file.get(); }
};

}
