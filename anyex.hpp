#pragma once

#include <iostream>
#include <any>
#include <memory>

#include "errdef.hpp"

#define EXT_STR_ANY(x) std::any_cast<const std::string&>(x)

namespace zMile {

inline void print_any(const std::any x)
{
  if (x.type() == typeid(char))
    std::cout << std::any_cast<char>(x);
  else if (x.type() == typeid(std::string))
    std::cout << std::any_cast<const std::string&>(x);
  else if (x.type() == typeid(int))
    std::cout << std::any_cast<const int&>(x);
  else if (x.type() == typeid(double))
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

template<class T, class TFT>
bool instof(const TFT& x) {
  return typeid(x) == typeid(T);
}

};

template<typename Derived, typename Base>
std::unique_ptr<Derived> 
static_unique_ptr_cast(std::unique_ptr<Base> && p)
{
    auto d = static_cast<Derived *>(p.release());
    return std::unique_ptr<Derived>(d, std::move(p.get_deleter()));
}
