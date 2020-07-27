#pragma once

#include <stdexcept>
#include <system_error>

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

}
