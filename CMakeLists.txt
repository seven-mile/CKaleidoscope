cmake_minimum_required(VERSION 3.0.0)
project(CKaleidoscope VERSION 0.1.0)

include(CTest)
enable_testing()

add_test(compl CKaleidoscope ../input.in)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(StdLib SHARED stdlib.cpp)
set_target_properties(StdLib PROPERTIES VERSION 1.0 SOVERSION 1)

add_definitions("-Wall -g")
add_executable(CKaleidoscope main.cpp)

llvm_map_components_to_libnames(llvm_libs interpreter
  support core irreader orcjit executionengine native
  analysis instcombine object runtimedyld scalaropts)

target_link_libraries(CKaleidoscope StdLib ${llvm_libs})

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
