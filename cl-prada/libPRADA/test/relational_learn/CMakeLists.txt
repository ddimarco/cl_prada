FIND_PACKAGE( Boost 1.40 COMPONENTS program_options REQUIRED )
INCLUDE_DIRECTORIES( ${Boost_INCLUDE_DIR} )

add_executable(relational_learn main.cpp)
target_link_libraries(relational_learn PRADA ${Boost_LIBRARIES})

add_executable(rprop_test rprop_test.cpp)
target_link_libraries(rprop_test PRADA)
