#pragma once
#include <string>
#include <llvm/IR/Module.h>

enum class EmitType {
  Executable,
  Object,
  IR,
};

enum class OptLevel
{
    O0,
    O1,
    O2,
};

enum class Model
{
    m32,
    m64,
};

enum class Standard
{
    none,
    iso7185,
    iso10206,
};

bool CreateBinary(llvm::Module *module, const std::string& fileName, EmitType emit);

llvm::Module* CreateModule();
