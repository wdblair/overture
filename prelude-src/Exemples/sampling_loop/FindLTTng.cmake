# - Find LTTng
# Find the Linux Trace Toolkit - next generation with associated includes path.
# See http://lttng.org/
# This module defines
#  LTTNG_FOUND, If false, LTTng was not found.
# On can set LTTNG_PATH_HINT before using find_package(LTTng) and the
# module with use the PATH as a hint to find LTTng.
#
# The hint can be given on the command line too:
#   cmake -DLTTNG_PATH_HINT=/DATA/ERIC/LTTng /path/to/source


if(LTTNG_PATH_HINT)
  message(STATUS "FindLTTng: using PATH HINT: ${LTTNG_PATH_HINT}")
else()
  set(LTTNG_PATH_HINT)
endif()

#One can add his/her own builtin PATH.
#FILE(TO_CMAKE_PATH "/DATA/ERIC/LTTng" MYPATH)
#list(APPEND LTTNG_PATH_HINT ${MYPATH})

find_path(LTTNG_INCLUDE_DIR 
          NAMES lttng/tracepoint.h
          PATHS ${LTTNG_PATH_HINT}
          PATH_SUFFIXES include
          DOC "The LTTng include headers")

find_path(LTTNG_LIBRARY_DIR 
          NAMES liblttng-ust.so
          PATHS ${LTTNG_PATH_HINT}
          PATH_SUFFIXES lib
          DOC "The LTTng libraries")

find_library(LTTNG_UST_LIBRARY lttng-ust PATHS ${LTTNG_LIBRARY_DIR})
find_library(URCU_LIBRARY urcu-bp PATHS ${LTTNG_LIBRARY_DIR})
find_library(UUID_LIBRARY uuid)

set(LTTNG_LIBRARIES ${LTTNG_UST_LIBRARY} ${URCU_LIBRARY} ${UUID_LIBRARY}) 

# handle the QUIETLY and REQUIRED arguments and set PRELUDE_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(LTTNG 
                                  REQUIRED_VARS LTTNG_INCLUDE_DIR LTTNG_LIBRARY_DIR)
# VERSION FPHSA options not handled by CMake version < 2.8.2)                                  
#                                  VERSION_VAR PRELUDE_COMPILER_VERSION)
mark_as_advanced(LTTNG_INCLUDE_DIR)
mark_as_advanced(LTTNG_LIBRARY_DIR)