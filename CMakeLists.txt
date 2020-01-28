cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

# build stdhep tools
add_subdirectory(tools/stdhep-lib)
add_subdirectory(tools/stdhep-tools)

# build generators
add_subdirectory(generators/egs5)
add_subdirectory(generators/madgraph4)
add_subdirectory(generators/madgraph5)

# config and install bash env script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# config and install csh env script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.csh.in ${CMAKE_BINARY_DIR}/hps-mc-env.csh)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.csh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# config and install workflow script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-workflow.in ${CMAKE_BINARY_DIR}/hps-mc-workflow)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-workflow DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# config and install bsub script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-bsub.in ${CMAKE_BINARY_DIR}/hps-mc-bsub)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-bsub DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# config and install jsub script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-jsub.in ${CMAKE_BINARY_DIR}/hps-mc-jsub)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-jsub DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# install job script
install(FILES ${PROJECT_SOURCE_DIR}/scripts/hps-mc-job DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# install stdhep script
install(FILES scripts/stdhep_count.sh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# install python scripts
install(DIRECTORY python DESTINATION lib)
