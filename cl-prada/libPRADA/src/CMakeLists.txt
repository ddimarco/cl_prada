set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall")
add_definitions(-DMT_IMPLEMENT_TEMPLATES)

set (MT_srcs 
MT/array.cpp
MT/util.cpp)

set (relational_srcs
relational/learn.cpp
relational/learn_searchOperators.cpp
relational/literals.cpp
relational/plan.cpp
relational/prada.cpp
relational/reason.cpp
relational/reason_rules.cpp
relational/robotManipulationSymbols.cpp
relational/rules.cpp
relational/symbols.cpp
relational/utilTL.cpp)

add_library(PRADA SHARED ${MT_srcs} ${relational_srcs})
target_link_libraries(PRADA rt)
