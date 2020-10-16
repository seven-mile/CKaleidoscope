#pragma once

#include <cassert>
#include <cctype>
#include <functional>
#include <any>

#include "global.hpp"
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

    tok_kw_number,
    tok_kw_string,
    tok_kw_char,
    tok_kw_bool,

    tok_kw_true,
    tok_kw_false,

    tok_kw_if,
    tok_kw_else,

    tok_kw_for,
    tok_kw_in,

    tok_kw_return,

    // identifier
    tok_id,

    // literal
    tok_lit_num,
    tok_lit_char,
    tok_lit_string,

    // other tools
    tok_varargs,

    tok_other
};

const std::string map_tok[] = {
  "EOF", "Define", "Extern", "Number", "String", "Char", "Bool", "True", "False",
  "If", "Else", "For", "In", "Return", "Identity", "Literal Number",
  "Literal Char", "Literal String", "VarArgs Dots", "Other Lang Tool"
};

inline const bool tok_is_type(const tag_tok t) {
  return t == tok_kw_number ||
         t == tok_kw_string ||
         t == tok_kw_char   ||
         t == tok_kw_bool   ;
}

inline llvm::Type* get_type_of_tok(const tag_tok t) {
  switch (t) {
    case tok_kw_number:
      return llvm::Type::getDoubleTy(g_context);
    case tok_kw_string:
      return llvm::Type::getInt8PtrTy(g_context);
    case tok_kw_char:
      return llvm::Type::getInt8Ty(g_context);
    case tok_kw_bool:
      return llvm::Type::getInt1Ty(g_context);
    default:
      return nullptr;
  }
}

inline tag_tok get_tok_of_type(llvm::Type* t) {
  if (t->isFloatingPointTy()) return tok_kw_number;
  if (t->isArrayTy() && 
    (t->getArrayElementType()->isIntegerTy(8)))
      return tok_kw_string;
  if (t->isIntegerTy(8)) return tok_kw_char;
  if (t->isIntegerTy(1)) return tok_kw_bool;
  return tok_invalid;
}

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
  ~Token() {  }

  bool is_type() const {
    return tok_is_type(type);
  }

  bool is_tool(const char tool) const {
    return type == tok_other && any_eq_char(val, tool);
  }

  void print() const {
    std::cerr << "{ " << map_tok[type] << ", ";
    print_any(val);
    std::cerr << " }" << std::endl;
  }

};


class Lexer {
  FileSugar& file;
  std::string tmp_str;
  double num;
  bool submitted = false, cmd;

  inline bool isenter(char ch) { return ch == '\r' || ch == '\n'; }
  // Not EnTer But SPace
  inline bool netbsp(char ch) { return !isenter(ch) && isspace(ch); }
  inline bool escape(char ch) { return cmd ? netbsp(ch) : isspace(ch); }
  
public:
  Lexer(FileSugar& file, bool cmd = false) : file(file), cmd(cmd) {  }

  inline FileSugar& get_sugar() { return file; }

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

      tag_tok curx = tok_invalid;

      if (tmp_str == "def") {
        curx = tok_kw_def;
      } else if (tmp_str == "extern") {
        curx = tok_kw_extern;
      }
        else if (tmp_str == "number") {
        curx = tok_kw_number;
      } else if (tmp_str == "string") {
        curx = tok_kw_string;
      } else if (tmp_str == "char") {
        curx = tok_kw_char;
      } else if (tmp_str == "bool") {
        curx = tok_kw_bool;
      }
        else if (tmp_str == "true") {
        curx = tok_kw_true;
      } else if (tmp_str == "false") {
        curx = tok_kw_false;
      }
        else if (tmp_str == "if") {
        curx = tok_kw_if;
      } else if (tmp_str == "else") {
        curx = tok_kw_else;
      }
        else if (tmp_str == "for") {
        curx = tok_kw_for;
      } else if (tmp_str == "in") {
        curx = tok_kw_in;
      } 
        else if (tmp_str == "return") {
        curx = tok_kw_return;
      }
        else curx = tok_id;

      return { curx, tmp_str };
    }

    if (isdigit(~file) || ~file == '.') {
      tmp_str.clear();
      do tmp_str += ~file;
      while(isdigit(!file) || ~file == '.');

      upd();

      if (tmp_str == "...")
        return { tok_varargs, tmp_str };
      
      size_t verify_dot = 0;
      for (char ch : tmp_str) {
        if (ch == '.') if (++verify_dot > 1)
          throw syntax_error("two many dots in float number.");
        if (!isdigit(ch) && ch != '.')
          throw syntax_error("invalid literal float number.");
      }

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
      {
        char tch = ~file;
        if (tch == '\\')
        {
          tch = !file;
          // todo: \[number] ascii char
          switch (tch) {
            case 'n': tch = '\n'; break;
            case 'r': tch = '\r'; break;
            case 't': tch = '\t'; break;
            default: break;
          }
        }
        tmp_str += tch;

      }
      upd();
      if (~file == '\"') { !file; return { tok_lit_string, tmp_str }; }
      throw zMile::syntax_error("no ending \" character.");
    }

    if (~file == EOF) return tok_eof;

    // other symbols
    char tmp_ch = ~file;
    !file;

    if (tmp_ch == '<' || tmp_ch == '>') if (~file == '=')
      { !file; return { tok_other, char(-tmp_ch) }; }

    return { tok_other, tmp_ch };
  }

};

}
