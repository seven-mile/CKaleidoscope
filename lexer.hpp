#pragma once

#include "ioagent.hpp"
#include "errdef.hpp"

namespace zMile {

enum tag_tok : char {
    tok_eof,
    
    // keywords
    tok_kw_def,
    tok_kw_extern,

    // identifier
    tok_id,

    // literal
    tok_lit_num,
    tok_lit_char,
    tok_lit_string,

    tok_other
};

const std::string map_tok[] = {
  "EOF", "Keyword", "Keyword", "Identity", "Literal Number",
  "Literal Char", "Literal String", "Other Lang Tool"
};

template <class token_t>
std::ostream& token_printer(std::ostream &os, void* p) {
  os << *static_cast<token_t*>(p);
  return os;
}

template <typename token_t>
void token_deleter(void* p) {
   delete static_cast<token_t*>(p);
}

template <class token_t>
class token_shared_ptr {
  token_t* p;
  size_t* pcnt;
  using prtfun = std::function<std::ostream&(std::ostream&, void*)>;
  using delfun = std::function<void(void*)>;
  prtfun printer;
  delfun deleter;

public:
  template <class token_u>
  token_shared_ptr(
    token_u* p,
    prtfun printer = &token_printer<token_u>,
    delfun deleter = &token_deleter<token_u>
    ) : p(p), pcnt(new size_t(1)), printer(printer), deleter(deleter) {}

  using self_t = token_shared_ptr<token_t>;

  token_shared_ptr(const self_t &that)
    : p(that.p), pcnt(that.pcnt), printer(that.printer), deleter(that.deleter)
    { ++*pcnt; }

  token_shared_ptr(self_t&& that)
    : p(that.p), pcnt(that.pcnt), printer(that.printer), deleter(that.deleter)
    { that.pcnt = nullptr; ++*pcnt; }
  
  self_t& operator=(self_t& that) {
    ++*that.pcnt;
    if (!--*pcnt) { deleter(p); delete pcnt; }
    p = that.p, pcnt = that.pcnt, printer = that.printer, deleter = that.deleter;
  }
  
  ~token_shared_ptr() { if (!--*pcnt) deleter(p); }
  operator token_shared_ptr<void>() { return static_cast<token_shared_ptr<void>>(this); }
  
  friend std::ostream& operator<<(std::ostream &os, token_shared_ptr<void> pToken);
};

std::ostream& operator<<(std::ostream &os, token_shared_ptr<void> pToken)
{
  return pToken.printer(os, pToken.p);
}


struct TokenWrapper {
  tag_tok type;
  token_shared_ptr<void> ptr;

  TokenWrapper(tag_tok type) : type(type), ptr(new std::string("no value")) {}
  template <class token_t>
  TokenWrapper(tag_tok type, const token_t& cont)
    : type(type), ptr(new token_t(cont)) {}
  ~TokenWrapper() {  }

  void print() {
    std::cerr << "{ " << map_tok[type] << ", " << ptr << " }" << std::endl;
  }

};

class LexerHelper {
  FileSugar file;
  std::string tmp_str;
  double num;

public:
  LexerHelper(FileSugar file) : file(file) {}

  TokenWrapper get_token()
  {
    while(isspace(~file)) !file;

    if (isalpha(~file) || ~file == '_') {
      tmp_str = ~file;
      while (isalnum(!file) || ~file == '_')
        tmp_str += ~file;

      if (tmp_str == "def") {
        return tok_kw_def;
      } else if (tmp_str == "extern") {
        return tok_kw_extern;
      }

      return { tok_id, tmp_str };
    }

    if (isdigit(~file) || ~file == '.') {
      tmp_str.clear();
      do tmp_str += ~file; while(isdigit(!file) || ~file == '.');
      num = strtod(tmp_str.c_str(), nullptr);
      return { tok_lit_num, num };
    }

    if (~file == '#') {
      while (!file != EOF && ~file != '\n' && ~file != '\r');

      if (~file != EOF) return get_token();
    }

    if (~file == '\'') {
      char vch = !file, lch = !file;
      if (lch != '\'') throw zMile::syntax_error("Syntax Error: No ending \' character.");
      !file;
      return { tok_lit_char, vch };
    }

    if (~file == '\"') {
      tmp_str.clear();
      while (!file != '\n' && ~file != '\r' && ~file != '\"')
        tmp_str += ~file;
      if (~file == '\"') { !file; return { tok_lit_string, tmp_str }; }
      throw zMile::syntax_error("Syntax Error: No ending \" character.");
    }

    if (~file == EOF) return tok_eof;

    // other symbols
    char tmp_ch = ~file;
    !file;
    return { tok_other, tmp_ch };
  }

};

}
