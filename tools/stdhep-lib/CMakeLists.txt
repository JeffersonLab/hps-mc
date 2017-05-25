cmake_minimum_required(VERSION 3.0)

project(stdhep-lib VERSION 5.6.1)

message(STATUS "StdHep library version: ${stdhep-lib_VERSION}")

file(COPY ${PROJECT_SOURCE_DIR}/src/ DESTINATION ${CMAKE_BINARY_DIR}/stdhep-src/)

set(STDHEP_DIR ${CMAKE_BINARY_DIR}/stdhep-src)
set(STDHEP_LIBRARY_DIR ${STDHEP_DIR}/lib)
set(STDHEP_LIBRARIES ${STDHEP_LIBRARY_DIR}/libstdhep.a ${STDHEP_LIBRARY_DIR}/libstdhepC.a ${STDHEP_LIBRARY_DIR}/libFmcfio.a)

set(STDHEP_DIR ${STDHEP_DIR} PARENT_SCOPE)
set(STDHEP_LIBRARY_DIR ${STDHEP_LIBRARY_DIR} PARENT_SCOPE)
set(STDHEP_LIBRARIES ${STDHEP_LIBRARIES} PARENT_SCOPE)

add_custom_command(OUTPUT ${STDHEP_LIBRARIES} 
    COMMAND make
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/stdhep-src"
    DEPENDS "${CMAKE_BINARY_DIR}/stdhep-src")

add_custom_target(stdhep_lib_build ALL DEPENDS ${STDHEP_LIBRARIES})
