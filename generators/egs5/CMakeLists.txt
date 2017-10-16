cmake_minimum_required(VERSION 3.0)

project(egs5)

add_custom_target(egs5 all)

set(EGS5_DIR ${PROJECT_SOURCE_DIR}/src)
set(EGS5_DATA_DIR ${PROJECT_SOURCE_DIR}/data)
set(EGS5_CONFIG_DIR ${PROJECT_SOURCE_DIR}/config)

set(EGS5_DATA_DIR ${EGS5_DATA_DIR} PARENT_SCOPE)
set(EGS5_CONFIG_DIR ${EGS5_CONFIG_DIR} PARENT_SCOPE)

if (NOT STDHEP_DIR)
  message(FATAL_ERROR "The variable STDHEP_DIR pointing to a valid stdhep installation is not defined.")
endif()

file(COPY ${PROJECT_SOURCE_DIR}/config/ DESTINATION ${CMAKE_BINARY_DIR}/egs5/config/)

configure_file(${PROJECT_SOURCE_DIR}/config/src/egs5run.in ${CMAKE_BINARY_DIR}/egs5/config/src/egs5run)

add_custom_command(TARGET egs5 PRE_BUILD COMMAND ln ARGS -f -s ${PROJECT_SOURCE_DIR}/data . WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/egs5/config)

file(GLOB egs_program_sources ${PROJECT_SOURCE_DIR}/config/src/*.f)
list(REMOVE_ITEM egs_program_sources "${PROJECT_SOURCE_DIR}/config/src/singlePrecision_randomset.f")
foreach (egs_program_source ${egs_program_sources})
  get_filename_component(egs_program ${egs_program_source} NAME_WE)
  message(STATUS "Configuring EGS5 program: ${egs_program}")
  add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/egs5/config/${egs_program}.exe PRE_BUILD 
          COMMAND src/build.sh ARGS ${egs_program} WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/egs5/config)
  add_custom_target(${egs_program}_build ALL DEPENDS ${CMAKE_BINARY_DIR}/egs5/config/${egs_program}.exe)
  add_dependencies(egs5 ${egs_program}_build)
  install(FILES ${CMAKE_BINARY_DIR}/egs5/config/${egs_program}.exe DESTINATION bin RENAME egs5_${egs_program}
          PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endforeach()
