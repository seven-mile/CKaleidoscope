cmake_minimum_required(VERSION 3.0.0)
project(CKaleidoscope VERSION 0.1.0)

include(CTest)
enable_testing()

# macro(add_test_crashed name command)
#   # Use generic flow of add_test() command for automatically recognize our executable target
#   add_test(NAME ${name} COMMAND test_that_crash ${command} ${ARGN})
# endmacro(add_test_crashed)

add_test(callclib CKaleidoscope ../test/callclib/input.in)
add_test(memfib CKaleidoscope ../test/memfib/input.in)
add_test(simpson CKaleidoscope ../test/simpson/input.in)
add_test(right_asso CKaleidoscope ../test/right_asso/input.in)
set_tests_properties(right_asso PROPERTIES PASS_REGULAR_EXPRESSION "x = 5, y = 5, z = 5")
add_test(new_comma_test CKaleidoscope ../test/new_comma_test/input.in)
set_tests_properties(new_comma_test PROPERTIES PASS_REGULAR_EXPRESSION "now a is -1.\nplus returned 7.")
add_test(new_decl_test_correct CKaleidoscope ../test/new_decl_test/input.in)
add_test(new_decl_test_error CKaleidoscope ../test/new_decl_test/error.in)
set_tests_properties(new_decl_test_error PROPERTIES WILL_FAIL true)
# add_test_crashed(new_decl_test CKaleidoscope ../test/new_decl_test/input.in)
# add_test(segtree CKaleidoscope ../test/segtree/input.in)
# add_test(fft CKaleidoscope ../test/fft/input.in)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")

find_package(LLVM 12 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(CKaleidoscope main.cpp binary.cpp)
add_executable(Sample test/sample.cpp)

#llvm_map_components_to_libnames(llvm_libs interpreter mcjit
#  support core irreader orcjit executionengine native
#  analysis instcombine object runtimedyld scalaropts
#  all-targets)
execute_process(COMMAND llvm-config-12 --libs all
                OUTPUT_VARIABLE llvm_libs)

string(STRIP ${llvm_libs} llvm_libs)

message(STATUS "Use llvm_libs: ${llvm_libs}")

target_link_libraries(CKaleidoscope ${llvm_libs})
target_link_libraries(Sample ${llvm_libs})

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
