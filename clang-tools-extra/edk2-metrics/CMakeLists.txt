set(LLVM_LINK_COMPONENTS support)

add_clang_executable(edk2-ast-parser
  Edk2Metrics.cpp
  )

target_link_libraries(edk2-ast-parser
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
