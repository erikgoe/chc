cmake_minimum_required (VERSION 3.21)

# add files
add_executable(${PROJECT_NAME}
    Main.cpp
)

target_precompile_headers(${PROJECT_NAME}
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../../core/include/chc/pch.hpp
)

# includes
target_include_directories(${PROJECT_NAME}
    PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${CMAKE_CURRENT_SOURCE_DIR}/../../core
)

# linking
target_link_libraries(${PROJECT_NAME}
)
