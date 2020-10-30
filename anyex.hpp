#pragma once

#include <bits/stdint-intn.h>
#include <iostream>
#include <any>
#include <memory>

#include "errdef.hpp"

namespace zMile {

inline void print_any(const std::any x)
{
  if (x.type() == typeid(char))
    std::cout << std::any_cast<char>(x);
  else if (x.type() == typeid(std::string))
    std::cout << std::any_cast<const std::string&>(x);
  else if (x.type() == typeid(int))
    std::cout << std::any_cast<const int&>(x);
  else if (x.type() == typeid(int64_t))
    std::cout << std::any_cast<const int64_t&>(x);
  else if (x.type() == typeid(double))
    std::cout << std::any_cast<const double&>(x);
  else throw object_invalid("std::any object invalid.");
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
