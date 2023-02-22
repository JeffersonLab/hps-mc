externalproject_add("pede"
    PREFIX            "pede"
    # download MillepedeII tar-ball from host at JLab
    #   this also unpacks the tar archive by default
    URL               "https://hpsweb.jlab.org/test/hps-mc/tars/MillepedeII.tar.gz"
    URL_MD5           ab5de8ae6b1b6494899510a6cbc155cd
    # have the build commands run from inside source directory
    BUILD_IN_SOURCE   ON
    # update install prefix during configure command by editing the Makefile
    CONFIGURE_COMMAND sed -i "s|^PREFIX.*$|PREFIX = ${CMAKE_INSTALL_PREFIX}|g" Makefile
    # simply run make to build it
    BUILD_COMMAND     make
    # simply run make to install it
    INSTALL_COMMAND   make install
)

add_dependencies(external "pede")
message(STATUS "pede will be installed to: ${CMAKE_INSTALL_PREFIX}")
