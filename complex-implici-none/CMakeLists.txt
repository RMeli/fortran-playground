cmake_minimum_required(VERSION 3.16)
project(test_implicit_typing LANGUAGES Fortran)

# Ensures that the compiler has no visibility on the types of zdotc/cdotc
# by compiling it as a separate library.
add_library(dotc_fake STATIC dotc_fake.f90)

add_executable(test test.f90)
target_link_libraries(test PRIVATE dotc_fake)

add_custom_target(asm
    COMMAND objdump -d -M intel --no-show-raw-insn
            $<TARGET_FILE:test>
            > ${CMAKE_BINARY_DIR}/test.asm
    COMMAND echo "=== test.asm ==="
    COMMAND cat ${CMAKE_BINARY_DIR}/test.asm
    DEPENDS test
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Disassembling linked executable (Intel syntax)"
)
