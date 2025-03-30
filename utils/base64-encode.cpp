// from base64.cc - c++ source to a base64 reference encoder and decoder

//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Navneeth Mohan
//    Amit Gupta
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api.git repository.
//


#include "KIM_Version.hpp"

#include <b64/encode.h>

#include <fstream>
#include <iostream>
#include <string>

#include <algorithm>
#include <cstdlib>
#include <sstream>

// Function which prints the usage of this executable
void usage(std::string name)
{
  size_t beg = name.find_last_of("/\\");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);
  // Follows docopt.org format
  std::cerr << "Usage:\n"
            << "  " << name << " -i <file-in> <file-out>\n"
            << "  " << name << " --version\n";
  // note: this interface is likely to change in future kim-api releases
}

void usage(std::string name, std::string message)
{
  std::cerr << message << std::endl;
  usage(name);
}

// quick replacement of to_string for C++98
inline std::string int_to_string(int value)
{
  std::ostringstream oss;
  oss << value;
  return oss.str();
}

// A C++ implementation of the CMAKE string-operation MAKE_C_IDENTIFIER
std::string make_c_identifier(const std::string& input) 
{
  std::string output;
  
  // If the first character is a digit, prepend an underscore
  if (!input.empty() && std::isdigit(input[0])) 
    output += '_';

  // Process each character
  for(int i = 0; i < input.size(); i++)
  {
    const char ch = input[i];
    if (std::isalnum(ch) || ch == '_') 
      output += ch;  // Keep letters, numbers, and underscores
    else 
      output += '_';  // Replace other characters with underscore
  }

  return output;
}

int main(int argc, char ** argv)
{
  if ((argc < 2) || (argc > 4))
  {
    usage(argv[0]);
    return 1;
  }

  std::string option = argv[1];
  if (option == "--version")
  {
    std::cout << KIM_VERSION_STRING << std::endl;
    return 0;
  }
  else if (option != "-i" || argc != 4)
  {
    usage(argv[0]);
    exit(-1);
  }

  // open the input file in binary-mode
  std::string input = argv[2];
  std::ifstream instream(input.c_str(),
                         std::ios_base::in | std::ios_base::binary);
  if (!instream.is_open())
  {
    usage(argv[0], "Could not open input file " + input);
    exit(-1);
  }

  // open the output file in binary-mode
  std::string output = argv[3];
  std::ofstream outstream(output.c_str(), std::ios_base::out);
  if (!outstream.is_open())
  {
    usage(argv[0], "Could not open output file " + output);
    exit(-1);
  }

  std::string encodeFormatFileName = make_c_identifier(input);

  std::string header
      = "extern unsigned char " + encodeFormatFileName + "[] = \n\"";
  outstream.write(header.data(), header.length());

  base64::encoder E;

  E.encode(instream, outstream);
  std::string footer = "\";\nextern unsigned int " + encodeFormatFileName
                       + "_len = sizeof(" + encodeFormatFileName + ") - 1;\n";

  outstream.write(footer.data(), footer.length());

  return 0;
}
