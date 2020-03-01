cmake_minimum_required(VERSION 3.5)

project(
  miranda
  VERSION 2.066
  LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

execute_process(
  COMMAND byacc -d rules.y
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE byacc)
if(byacc)
  message(STATUS "byacc fails")
endif()

execute_process(
  COMMAND cc fdate.c -o fdate
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE fdate)
if(${fdate})
  message(STATUS "Fail to build fdate")
endif()

execute_process(COMMAND ./hostinfo WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                                                     OUTPUT_FILE .host)

execute_process(
  COMMAND ./quotehostinfo
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE host)
string(STRIP ${host} host)

execute_process(
  COMMAND ./revdate
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE vdate)
string(STRIP ${vdate} vdate)

execute_process(
  COMMAND ln -sf ${CMAKE_SOURCE_DIR}/miralib)

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
  utf8.c
  version.c
  y.tab.c)
target_compile_definitions(mira PRIVATE VERS=${vers} VDATE="${vdate}"
                                        HOST=${host})
target_compile_options(mira PRIVATE -O2)

add_executable(menudriver menudriver.c)
set_target_properties(menudriver PROPERTIES RUNTIME_OUTPUT_DIRECTORY miralib)

add_custom_target(examples
  COMMAND ./mira -make -lib miralib ex/*.m
  DEPENDS mira)
