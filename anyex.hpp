#pragma once

#include <iostream>
#include <any>

#include "errdef.hpp"

#define EXT_STR_ANY(x) std::any_cast<const std::string&>(x)

namespace zMile {

inline void print_any(std::any x)
{
  if (x.type() == typeid(char))
    std::cout << std::any_cast<char>(x);
  else if (x.type() == typeid(std::string))
    std::cout << std::any_cast<const std::string&>(x);
  else if (x.__is_valid_cast<double>())
    std::cout << std::any_cast<const double&>(x);
  else throw object_invalid("std::any object invalid.");
}

inline bool any_eq_char(const std::any& p, const char b)
{
  if (p.type() != typeid(char))
    //throw object_invalid("comparison between a non-char std::any and char.");
    return false;

  return std::any_cast<char>(p) == b;
}

};
