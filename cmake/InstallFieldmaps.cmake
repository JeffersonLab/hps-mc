set(FIELDMAP_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/share/fieldmap")

externalproject_add(
    Fieldmaps
    GIT_REPOSITORY    "https://github.com/JeffersonLab/hps-fieldmaps"
    GIT_SHALLOW       ON
    SOURCE_DIR        ${CMAKE_BINARY_DIR}/Fieldmaps
    BUILD_COMMAND     cd ${CMAKE_BINARY_DIR}/Fieldmaps && ./unzip.sh
    UPDATE_COMMAND    git pull
    INSTALL_COMMAND   ${CMAKE_COMMAND} -E make_directory ${FIELDMAP_INSTALL_DIR}
              COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/Fieldmaps ${FIELDMAP_INSTALL_DIR}
    CONFIGURE_COMMAND ""
)

add_dependencies(external Fieldmaps)

message(STATUS "Fieldmaps will be installed to: ${FIELDMAP_INSTALL_DIR}")