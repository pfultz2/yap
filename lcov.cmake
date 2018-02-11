# lcov.cmake
# `ctest -S lcov.cmake`

if(NOT ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  message(FATAL_ERROR "LCOV is Linux-only")
endif(NOT ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")

include(ProcessorCount)
ProcessorCount(N)

set(CTEST_PROJECT_NAME yap)
set(CTEST_SOURCE_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
set(CTEST_BINARY_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/build_lcov)
set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
set(CTEST_MEMORYCHECK_COMMAND /usr/bin/valgrind)

set(LCOV_OUT "${CTEST_BINARY_DIRECTORY}/lcov_html")

ctest_start(lcov)
ctest_empty_binary_directory(${CTEST_BINARY_DIRECTORY})
message("configuring...")
ctest_configure(BUILD "${CTEST_BINARY_DIRECTORY}" OPTIONS "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_CURRENT_LIST_DIR}/cget/cget/cget.cmake;-DCMAKE_CXX_FLAGS=-fprofile-arcs -ftest-coverage")
message("lcov: resetting counters...")
execute_process(COMMAND lcov -z -d ${CTEST_BINARY_DIRECTORY}
  WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} OUTPUT_QUIET)

message("building...")
ctest_build(BUILD "${CTEST_BINARY_DIRECTORY}" FLAGS -j${N})

message("running tests...")
ctest_test(BUILD "${CTEST_BINARY_DIRECTORY}/test" PARALLEL_LEVEL ${N})

message("analyzing profiling data using lcov...")
execute_process(COMMAND lcov -c -d ${CTEST_BINARY_DIRECTORY} -o ${CTEST_BINARY_DIRECTORY}/stepcode.lcov
  WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} OUTPUT_QUIET)

message("removing system headers...")
execute_process(COMMAND lcov -r ${CTEST_BINARY_DIRECTORY}/stepcode.lcov "/usr/include/*" -o ${CTEST_BINARY_DIRECTORY}/stepcode_no_usr.lcov
  WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} OUTPUT_QUIET)
execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${LCOV_OUT})

message("creating html files...")
execute_process(COMMAND genhtml ${CTEST_BINARY_DIRECTORY}/stepcode_no_usr.lcov
  WORKING_DIRECTORY ${LCOV_OUT} OUTPUT_QUIET)

message("html files are located in ${LCOV_OUT}")

message("================================================ Success! ================================================")

