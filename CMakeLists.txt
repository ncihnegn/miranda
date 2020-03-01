cmake_minimum_required(VERSION 3.5)

project(
  miranda
  VERSION 2.066
  LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(READ "miralib/.version" VERS)
execute_process(COMMAND "./revdate"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  OUTPUT_VARIABLE VDATE)
string(STRIP ${VDATE} VDATE)
execute_process(COMMAND "./quotehostinfo"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  OUTPUT_VARIABLE HOST)
string(STRIP ${HOST} HOST)

add_executable(
  mira
  big.c
  cmbnms.c
  data.c
  lex.c
  reduce.c
  steer.c
  trans.c
  types.c
  utf8.c
  version.c
  y.tab.c)

target_compile_definitions(mira PRIVATE VERS=${VERS} VDATE="${VDATE}" HOST=${HOST})
target_compile_options(mira PRIVATE -w)
