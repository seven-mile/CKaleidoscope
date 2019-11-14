#pragma once

#include <system_error>

namespace zMile {

class syntax_error : public std::logic_error {
  std::string msg;
  int code;

  public:
    syntax_error(const std::string& msg)
      : std::logic_error("syntax error: " + msg) {  }
};

}
