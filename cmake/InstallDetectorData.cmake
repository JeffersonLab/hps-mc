set(DETECTOR_DATA "DetectorData")

externalproject_add(
    ${DETECTOR_DATA}
    INSTALL_DIR       "${CMAKE_INSTALL_PREFIX}/share"
    GIT_REPOSITORY    "https://github.com/JeffersonLab/hps-java"
    GIT_SHALLOW       ON
    SOURCE_DIR        ${CMAKE_BINARY_DIR}/${DETECTOR_DATA}
    BUILD_COMMAND     ""
    INSTALL_COMMAND   ""
    CONFIGURE_COMMAND ""
)

add_dependencies(external ${DETECTOR_DATA})

externalproject_get_property(${DETECTOR_DATA} SOURCE_DIR)
externalproject_get_property(${DETECTOR_DATA} INSTALL_DIR)

install(DIRECTORY ${SOURCE_DIR}/detector-data/detectors DESTINATION ${INSTALL_DIR}
        FILES_MATCHING PATTERN "*.lcdd"
        PATTERN "SamplingFractions" EXCLUDE)

message(STATUS "${DETECTOR_DATA} will be installed to: ${INSTALL_DIR}/detectors")
