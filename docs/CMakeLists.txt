#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013--2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

cmake_minimum_required(VERSION 3.9)

cmake_policy(SET CMP0057 NEW)
cmake_policy(SET CMP0054 NEW)

find_package(Doxygen)

set(DOXYGEN_PROJECT_NAME         "${PROJECT_NAME}")
set(DOXYGEN_STRIP_FROM_PATH      "../../")
set(DOXYGEN_TAB_SIZE             2)
set(DOXYGEN_EXTRACT_ALL          YES)
set(DOXYGEN_EXTRACT_STATIC       YES)
set(DOXYGEN_EXTRACT_ANON_NSPACES YES)
set(DOXYGEN_FILE_PATTERNS
  "*.c" "*.cc" "*.cxx" "*.cpp" "*.c++" "*.java" "*.ii" "*.ixx" "*.ipp"
  "*.i++" "*.inl" "*.idl" "*.ddl" "*.odl" "*.h" "*.hh" "*.hxx" "*.hpp" "*.h++"
  "*.cs" "*.d" "*.php" "*.php4" "*.php5" "*.phtml" "*.inc" "*.m" "*.markdown"
  "*.md" "*.mm" "*.dox" "*.py" "*.pyw" "*.f90" "*.f95" "*.f03" "*.f08" "*.f"
  "*.for" "*.tcl" "*.vhd" "*.vhdl" "*.ucf" "*.qsf" "Makefile" "CMakeLists.txt"
  "*.inc" "*.tpl" "*.sh" "*.fd" "*.params" "README"
)


set(DOXYGEN_RECURSIVE            YES)
set(DOXYGEN_EXAMPLE_PATH         "src/porting-scripts")
set(DOXYGEN_IMAGE_PATH           "src/asymptote-figures")
set(DOXYGEN_SOURCE_BROWSER       YES)
set(DOXYGEN_USE_MATHJAX          YES)
set(DOXYGEN_MATHJAX_RELPATH      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3")
set(DOXYGEN_EXTRA_PACKAGES       "amsmath")
set(DOXYGEN_GENERATE_LATEX       YES)

set(SOURCES
  "src/introduction.txt"
  "src/features.txt"
  "src/theory.txt"
  "src/implementation.txt"
  "src/version2-differences.txt"
  "src/porting-content-from-v1-to-v2.txt"
  "porting-content-from-v1-to-v2-examples/"
  "../cpp/include/"
  "../c/include/"
  "../fortran/include/"
  "../examples/model-drivers/"
  "../examples/models/"
  "../examples/simulators/"
  )

if(DOXYGEN_FOUND)
  doxygen_add_docs(
    docs
    ${SOURCES}
    COMMENT "Generate documentation"
    )

  add_custom_command(OUTPUT refman.pdf
    COMMAND make
    COMMAND ${CMAKE_COMMAND} -E copy refman.pdf ${CMAKE_CURRENT_BINARY_DIR}/refman.pdf
    WORKING_DIRECTORY latex
    DEPENDS docs
    )

  # TODO fix dependencies
  add_custom_target(pdf DEPENDS docs refman.pdf SOURCES ${SOURCES})
endif(DOXYGEN_FOUND)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html DESTINATION ${CMAKE_INSTALL_DOCDIR} OPTIONAL)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/refman.pdf DESTINATION ${CMAKE_INSTALL_DOCDIR} OPTIONAL)
