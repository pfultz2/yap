include_directories(${CMAKE_HOME_DIRECTORY})

include(CTest)
enable_testing()

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} -VV -C ${CMAKE_CFG_INTDIR})

macro(add_test_executable name)
    add_executable(${name} ${name}.cpp)
    target_link_libraries(${name} yap gtest gtest_main)
    add_test(${name} ${CMAKE_CURRENT_BINARY_DIR}/${name} --gtest_catch_exceptions=1)
    if (clang_on_linux)
        target_link_libraries(${name} c++)
    endif ()
endmacro()


# include_directories(SYSTEM ${CMAKE_SOURCE_DIR}/googletest-release-1.8.0/googletest/include)
# add_compile_options(-Weverything
#                 -Wno-c++98-compat
#                 -Wno-c++98-compat-pedantic
#                 -Wno-conversion
#                 -Wno-double-promotion
#                 -Wno-exit-time-destructors
#                 -Wno-extra-semi
#                 -Wno-float-conversion
#                 -Wno-gnu-anonymous-struct
#                 -Wno-gnu-zero-variadic-macro-arguments
#                 -Wno-missing-prototypes
#                 -Wno-nested-anon-types
#                 -Wno-padded
#                 -Wno-shorten-64-to-32
#                 -Wno-sign-conversion
#                 -Wno-unused-command-line-argument
#                 -Wno-weak-vtables
#                 -Wno-shadow
#                 -Wno-covered-switch-default
#                 -Wno-unused-parameter
#                 -Wno-old-style-cast
# )

add_test_executable(deref)
add_test_executable(value)
add_test_executable(left)
add_test_executable(right)
add_test_executable(print)
add_test_executable(default_eval)
add_test_executable(user_operator_eval)
add_test_executable(user_eval_expression_as)
add_test_executable(user_operator_and_eval_expression_as)
add_test_executable(user_expression_transform)
add_test_executable(user_expression_transform_2)
add_test_executable(user_expression_transform_3)
add_test_executable(user_expression_transform_4)
add_test_executable(placeholder_eval)
add_test_executable(call_expr)
add_test_executable(call_expr_implicit)
add_test_executable(reference_returns)
add_test_executable(depth_stress_test_left)
add_test_executable(depth_stress_test_right)
add_test_executable(lazy_vector_alloc_test)
add_test_executable(vector_alloc_test)
add_test_executable(operators_unary)
add_test_executable(expression_function)

add_executable(
    compile_tests
    compile_tests_main.cpp
    compile_is_expr.cpp
    compile_const_term.cpp
    compile_placeholders.cpp
    compile_term_plus_expr.cpp
    compile_term_plus_term.cpp
    compile_term_plus_x.cpp
    compile_x_plus_term.cpp
    compile_term_plus_x_this_ref_overloads.cpp
    compile_const_term.cpp
    compile_move_only_types.cpp
    compile_copy_only_types.cpp
)
target_link_libraries(compile_tests yap)
if (clang_on_linux)
    target_link_libraries(compile_tests c++)
endif ()
