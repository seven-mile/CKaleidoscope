cmake_minimum_required(VERSION 3.0.0)
project(CKaleidoscope VERSION 0.1.0)

include(CTest)
enable_testing()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(CKaleidoscope main.cpp)

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(CKaleidoscope ${llvm_libs})

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
