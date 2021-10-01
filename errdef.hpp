#pragma once

#include <stdexcept>
#include <system_error>
#include <iostream>

#define DECLARE_GENERAL_ERROR(err_name, err_base) \
class err_name : public err_base { \
  std::string msg; \
 \
  public: \
    err_name(const std::string& msg) \
      : err_base(#err_name": " + msg) {  } \
};

#define DECLARE_LOGIC_ERROR(err_name) DECLARE_GENERAL_ERROR(err_name, std::logic_error)

namespace zMile {

DECLARE_LOGIC_ERROR(syntax_error)
DECLARE_LOGIC_ERROR(object_invalid)
DECLARE_GENERAL_ERROR(io_error, std::runtime_error)

struct SourceLoc {
  int line, column;
  SourceLoc(int line, int column) : line(line), column(column) {  }
};

template <class err_t = syntax_error>
inline auto log_err_with_loc(std::string errinfo, SourceLoc loc) {
  std::cerr << errinfo + " [Line " + std::to_string(loc.line) + ", Column " + std::to_string(loc.column) + "]\n";
  exit(1);
  return nullptr;
}

}
