#
# This is a sample project that can 
# be used to compile a shared lib out of a prelude
# program. This shared lib will be usable by SchedMCore. 
#

cmake_minimum_required(VERSION 2.6.2)
project(sampling_loop C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
find_package(Prelude REQUIRED)

Prelude_Compile(NODE sampling 
                PLU_FILES sampling.plu)
                
# Or without precedence encoding (needed for multicore execution)
#Prelude_Compile(NODE sampling 
#                PLU_FILES sampling.plu
#                NOENCODING)

