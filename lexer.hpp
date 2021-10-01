#pragma once

#include "errdef.hpp"
#include "anyex.hpp"
#include "global.hpp"
#include "ioagent.hpp"

#include <cassert>
#include <cctype>
#include <cinttypes>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <any>
#include <llvm/IR/Type.h>
#include <string>


namespace zMile {

enum tag_tok : char {
    tok_invalid = -1,

    tok_eof,
    
    // keywords
    tok_kw_def,
    
    tok_kw_extern,
    tok_kw_const,

    tok_kw_void,
    tok_kw_int,
    tok_kw_uint,
    tok_kw_int64,
    tok_kw_uint64,
    tok_kw_number,
    tok_kw_string,
    tok_kw_char,
    tok_kw_bool,

    tok_kw_true,
    tok_kw_false,

    tok_kw_if,
    tok_kw_else,

    tok_kw_for,
    tok_kw_while,

    tok_kw_return,

    tok_kw_break,
    tok_kw_continue,

    // identifier
    tok_id,

    // literal
    tok_lit_num,
    tok_lit_char,
    tok_lit_string,

    // other tools
    tok_varargs,
    tok_addadd,
    tok_subsub,

    tok_other
};

const std::string map_tok[] = {
  "EOF", "Define", "Extern", "Const",
  "Void", "Int", "Uint", "Int64", "Uint64", "Number", "String", "Char",
  "Bool", "True", "False",
  "If", "Else", "For", "While", "Return", "Break", "Continue",
  "Identity",
  "Literal Number", "Literal Char", "Literal String",
  "VarArgs Dots", "Self Increment", "Self Decrement", "Other Lang Tool"
};

inline const bool tok_is_type(const tag_tok t) {
  return 
         t == tok_kw_void   ||

         t == tok_kw_int    ||
         t == tok_kw_uint   ||
         t == tok_kw_int64  ||
         t == tok_kw_uint64 ||
         
         t == tok_kw_number ||
         t == tok_kw_string ||
         t == tok_kw_char   ||
         t == tok_kw_bool   ||
         0;
}

inline llvm::Type* get_type_of_tok(const tag_tok t) {
  switch (t) {
    case tok_kw_number:
      return llvm::Type::getDoubleTy(*g_context);
    case tok_kw_string:
      return llvm::Type::getInt8PtrTy(*g_context);
    case tok_kw_char:
      return llvm::Type::getInt8Ty(*g_context);
    case tok_kw_bool:
      return llvm::Type::getInt1Ty(*g_context);
    case tok_kw_int:
    case tok_kw_uint:
      return llvm::Type::getInt32Ty(*g_context);
    case tok_kw_int64:
    case tok_kw_uint64:
      return llvm::Type::getInt64Ty(*g_context);
    case tok_kw_void:
      return llvm::Type::getVoidTy(*g_context);
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
  if (t->isIntegerTy(32)) return tok_kw_int;
  return tok_invalid;
}

struct Token {
  tag_tok type;
  std::any val;
  SourceLoc loc;

  Token(tag_tok type) : type(type),
    val(std::string("no value")), loc({0,0}) {  }
  template <class token_t>
  Token(tag_tok type, const token_t& cont, SourceLoc loc = {0,0})
    : type(type), val(token_t(cont)), loc(loc) {  }
  Token(const Token& rhs) = default;
  Token(Token&& rhs) = default;
  Token& operator=(Token &&rhs) = default;
  ~Token() {  }

  bool is_type() const {
    return tok_is_type(type);
  }

  bool is_spec() const {
    return type == tok_kw_const;// || type == tok_kw_extern;
  }

  bool is_tool(const std::string& tool) const {
    return type == tok_other && std::any_cast<const std::string&>(val) == tool;
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

  template<class err_t = syntax_error>
  inline auto log_err(std::string errinfo) {
    return log_err_with_loc<err_t>(errinfo, file.cur_loc);
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

    SourceLoc loc_start = file.cur_loc;

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
      } else if (tmp_str == "const") {
        curx = tok_kw_const;
      }
      else if (tmp_str == "void") {
        curx = tok_kw_void;
      } else if (tmp_str == "int") {
        curx = tok_kw_int;
      } else if (tmp_str == "int64") {
        curx = tok_kw_int;
      } else if (tmp_str == "uint") {
        curx = tok_kw_int;
      } else if (tmp_str == "uint64") {
        curx = tok_kw_int;
      } else if (tmp_str == "number") {
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
      } else if (tmp_str == "while") {
        curx = tok_kw_while;
      }
      else if (tmp_str == "return") {
        curx = tok_kw_return;
      } else if (tmp_str == "break") {
        curx = tok_kw_break;
      } else if (tmp_str == "continue") {
        curx = tok_kw_continue;
      } else curx = tok_id;

      return { curx, tmp_str, loc_start };
    }

    if (isdigit(~file) || ~file == '.') {
      tmp_str.clear();
      do tmp_str += ~file;
      while(isdigit(!file) || ~file == '.' || ~file == 'e'
            || (tmp_str.back() == 'e' && ~file == '-'));

      size_t cnt_ch[2] {};
      for (; ~file=='l' || ~file=='u'; !file)
        cnt_ch[~file=='l']++;
      if (cnt_ch[0] > 1) log_err("unsigned literal suffix shouldn't appear more than one time.");
      if (cnt_ch[1] > 2) log_err("long literal suffix shouldn't appear more than two times.");
      
      upd();

      if (tmp_str == "...")
        return { tok_varargs, tmp_str, loc_start };
      
      size_t verify_dot = 0, verify_e = 0;
      for (char ch : tmp_str) {
        if (ch == '.') {
          if (++verify_dot > 1)
            log_err("two many dots in float number.");
          if (verify_e)
            log_err("non integer exponent is invalid.");
        }
        if (ch == 'e') if (++verify_e   > 1)
          log_err("two many exp in float number.");
        if (!isdigit(ch) && ch != '.' && ch != 'e' && ch != '-')
          log_err("invalid literal float number.");
      }
      if (verify_dot || verify_e) return { tok_lit_num, std::strtod(tmp_str.c_str(), nullptr), loc_start };
      else {
        if (cnt_ch[0]) {
          if (cnt_ch[1] == 2) return { tok_lit_num, (uint64_t)std::strtoull(tmp_str.c_str(), nullptr, 10), loc_start };
          return { tok_lit_num, (uint)std::strtoul(tmp_str.c_str(), nullptr, 10), loc_start };
        } else {
          if (cnt_ch[1] == 2) return { tok_lit_num, (int64_t)atoll(tmp_str.c_str()), loc_start };
          return { tok_lit_num, (int)std::strtol(tmp_str.c_str(), nullptr, 10), loc_start };
        }
      }
    }
    if (~file == '/' && file.peek() == '/') {
      while (!file != EOF && ~file != '\n' && ~file != '\r');

      if (~file != EOF) return get_token();
    }

    if (~file == '\'') {
      SourceLoc loc_start = file.cur_loc;
      char vch = !file, lch = !file;
      if (lch != '\'') log_err("no ending \' character.");
      !file;
      upd();
      return { tok_lit_char, vch, loc_start };
    }

    if (~file == '\"') {
      SourceLoc loc_start = file.cur_loc;
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
      if (~file == '\"') { !file; return { tok_lit_string, tmp_str, loc_start }; }
      log_err("no ending \" character.");
    }

    if (~file == EOF) return { tok_eof, -1, file.cur_loc };

    // other symbols
    char tmp_ch = ~file;
    loc_start = file.cur_loc;
    !file;

    if (~file == tmp_ch) {
      if (tmp_ch == '+') { !file; return { tok_addadd, std::string("++"), loc_start }; }
      if (tmp_ch == '-') { !file; return { tok_subsub, std::string("--"), loc_start }; }
    }
    if ((tmp_ch == '+' || tmp_ch == '-') && ~file == '=')
      { !file; return { tok_other, tmp_ch + std::string("="), loc_start }; }
    
    // bit operators
    if (tmp_ch == '<') if (~file == '<') {
      if (!file == '=') { !file; return { tok_other, std::string("<<="), loc_start }; }
      else return { tok_other, std::string("<<"), loc_start };
    } if (tmp_ch == '>') if (~file == '>') {
      if (!file == '=') { !file; return { tok_other, std::string(">>="), loc_start }; }
      else return { tok_other, std::string(">>"), loc_start };
    } if (tmp_ch == '&') if (~file == '&') {
      if (!file == '=') { !file; return { tok_other, std::string("&&="), loc_start }; }
      else return { tok_other, std::string("&&"), loc_start };
    } if (tmp_ch == '|') if (~file == '|') {
      if (!file == '=') { !file; return { tok_other, std::string("||="), loc_start }; }
      else return { tok_other, std::string("||"), loc_start };
    }
    
    // comparison
    if (tmp_ch == '<' || tmp_ch == '>') if (~file == '=')
      { !file; return { tok_other, tmp_ch + std::string("="), loc_start }; }

    if (tmp_ch == '=') if (~file == '=')
      { !file; return { tok_other, std::string("=="), loc_start }; }
    if (tmp_ch == '!') if (~file == '=')
      { !file; return { tok_other, std::string("!="), loc_start }; }

    return { tok_other, std::string("") + tmp_ch, loc_start };
  }

};

}
