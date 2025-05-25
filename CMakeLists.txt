cmake_minimum_required(VERSION 3.5)

project(
  miranda
  VERSION 2.066
  LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_program(clangtidy NAMES clang-tidy)
if(${clangtidy})
  set(CMAKE_C_CLANG_TIDY ${clangtidy} -fix)
endif()

add_executable(just just.c)

add_executable(menudriver menudriver.c)
set_target_properties(menudriver PROPERTIES RUNTIME_OUTPUT_DIRECTORY miralib)

execute_process(
  COMMAND byacc -d rules.y
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE byacc)
if(byacc)
  message(FATAL_ERROR "Fail to run byacc")
endif()

execute_process(
  COMMAND ./gencdecs
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE gencdecs)
if(gencdecs)
  message(FATAL_ERROR "Fail to run gencdecs")
endif()

execute_process(
  COMMAND sh -c "echo compiled: `date` && ${CMAKE_C_COMPILER} -v 2>&1"
  OUTPUT_VARIABLE host)
string(REPLACE "\n" "\\n" host ${host})

execute_process(COMMAND git show -s --format=%cd --date=format:"%d %b %Y"
                OUTPUT_VARIABLE vdate)
string(STRIP ${vdate} vdate)

if(NOT (${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR}))
  execute_process(COMMAND ln -sf ${CMAKE_SOURCE_DIR}/miralib ${CMAKE_BINARY_DIR}/miralib)
endif()

file(READ miralib/.version vers)

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
  version.c
  y.tab.c)
target_compile_definitions(mira PRIVATE VERS=${vers} VDATE=${vdate}
                                        HOST="${host}")
target_compile_options(mira PRIVATE -O2 -Wno-deprecated-non-prototype -fcommon)
target_link_libraries(mira m)

add_custom_target(
  examples
  COMMAND ./mira -make -lib miralib ex/*.m
  DEPENDS mira)
