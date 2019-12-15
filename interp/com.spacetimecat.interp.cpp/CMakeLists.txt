cmake_minimum_required(VERSION 3.7)

project(interp VERSION 0.0.0 LANGUAGES C CXX)

    ##  target_compile_features does not accept cxx_std_11 in CMake 3.7.
    set(CMAKE_CXX_STANDARD 11)
    set(CMAKE_CXX_STANDARD_REQUIRED True)
    set(CMAKE_CXX_EXTENSIONS False)

    configure_file(src/config.h.in config.h)

    add_executable(interp
        src/main1.cpp
        src/rts.cpp
    )

        set(USE_LIBSTDCXX True CACHE BOOL "Use libstdc++?")

        if(USE_LIBSTDCXX)
        else()
            ##  https://stackoverflow.com/questions/15058403/how-to-stop-cmake-from-linking-against-libstdc
            set(CMAKE_CXX_IMPLICIT_LINK_LIBRARIES "")
            set(CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES "")
            set_target_properties(interp PROPERTIES LINKER_LANGUAGE C)

            target_compile_options(interp PUBLIC
                -fno-rtti
                -fno-exceptions
            )
            target_sources(interp PUBLIC
                src/rts_no_libstdcxx.cpp
            )
        endif()

        target_include_directories(interp
            PRIVATE
                "${PROJECT_BINARY_DIR}"
                src
        )

        target_compile_options(interp
            PRIVATE
                -Wall
        )
