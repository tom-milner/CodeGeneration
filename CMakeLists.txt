cmake_minimum_required(VERSION 2.8.12)
project(Code_Generation_Self_Grader C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED TRUE)

include_directories(.)

add_executable(Code_Generation_Self_Grader
        CodeGrader.c
        compiler.c
        lexer.c
        lexer.h
        parser.c
        parser.h
        symbols.c
        symbols.h
        vm_writer.c
        vm_writer.h)


add_custom_target(jack_lib
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/lib ${CMAKE_CURRENT_BINARY_DIR}/
        )

add_custom_target(test_programs
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/TestPrograms ${CMAKE_CURRENT_BINARY_DIR}/
        )

add_custom_target(test_programs_compiled
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/TestProgramsCompiled ${CMAKE_CURRENT_BINARY_DIR}/
        )

add_dependencies(Code_Generation_Self_Grader jack_lib test_programs test_programs_compiled)

set_property(TARGET Code_Generation_Self_Grader PROPERTY C_STANDARD 99)

