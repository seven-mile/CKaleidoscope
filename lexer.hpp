#pragma once

#include <functional>
#include <any>

#include "ioagent.hpp"
#include "errdef.hpp"
#include "anyex.hpp"

namespace zMile {

enum tag_tok : char {
    tok_invalid = -1,

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

struct Token {
  tag_tok type;
  std::any val;

  Token(tag_tok type) : type(type),
    val(std::string("no value")) {  }
  template <class token_t>
  Token(tag_tok type, const token_t& cont)
    : type(type), val(token_t(cont)) {  }
  Token(const Token& rhs) = default;
  Token(Token&& rhs) = default;
  Token& operator=(Token &&rhs) = default;
    // { type = rhs.type, val = rhs.val; return *this; }
  // bool operator==(Token &&rhs) const
    // { return type == rhs.type && val == rhs.val; }
  ~Token() {  }

  void print() {
    std::cerr << "{ " << map_tok[type] << ", ";
    print_any(val);
    std::cerr << " }" << std::endl;
  }

};


class Lexer {
  FileSugar file;
  std::string tmp_str;
  double num;
  bool submitted = false, cmd;

public:
  Lexer(FileSugar file, bool cmd = false) : file(file), cmd(cmd) {}

  inline bool isenter(char ch) { return ch == '\r' || ch == '\n'; }
  // Not EnTer But SPace
  inline bool netbsp(char ch) { return !isenter(ch) && isspace(ch); }
  inline bool escape(char ch) { return cmd ? netbsp(ch) : isspace(ch); }
  void upd() {
    if (!cmd) return;
    while (escape(~file)) !file;
    if (isenter(~file)) {
      // eat '\r' '\n'
      if (~file == '\r') !file;
      file.reset();
      submitted = true;
    }
  }

  Token get_token()
  {
    if (cmd && submitted) return submitted = false, tok_invalid;
    while (escape(~file)) !file;

    if (isenter(~file)) {
      if (~file == '\r') !file;
      assert(~file == '\n');
      file.reset();
      // submitted = true;
      return tok_invalid;
    }

    if (isalpha(~file) || ~file == '_') {
      tmp_str = ~file;
      while (isalnum(!file) || ~file == '_')
        tmp_str += ~file;

      upd();

      if (tmp_str == "def") {
        return tok_kw_def;
      } else if (tmp_str == "extern") {
        return tok_kw_extern;
      }

      return { tok_id, tmp_str };
    }

    if (isdigit(~file) || ~file == '.') {
      tmp_str.clear();
      do tmp_str += ~file;
      while(isdigit(!file) || ~file == '.');

      upd();

      num = strtod(tmp_str.c_str(), nullptr);
      return { tok_lit_num, num };
    }

    if (~file == '#') {
      while (!file != EOF && ~file != '\n' && ~file != '\r');

      if (~file != EOF) return get_token();
    }

    if (~file == '\'') {
      char vch = !file, lch = !file;
      if (lch != '\'') throw zMile::syntax_error("no ending \' character.");
      !file;
      upd();
      return { tok_lit_char, vch };
    }

    if (~file == '\"') {
      tmp_str.clear();
      while (!file != '\n' && ~file != '\r' && ~file != '\"')
        tmp_str += ~file;
      upd();
      if (~file == '\"') { !file; return { tok_lit_string, tmp_str }; }
      throw zMile::syntax_error("no ending \" character.");
    }

    if (~file == EOF) return tok_eof;

    // other symbols
    char tmp_ch = ~file;
    !file;
    return { tok_other, tmp_ch };
  }

};

}
