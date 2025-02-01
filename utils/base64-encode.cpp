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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <b64/encode.h>

#include <fstream>
#include <iostream>
#include <string>

#include <algorithm>
#include <sstream>
#include <cstdlib>

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
    printf("VERSION\n");
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

  std::string encodeFormatFileName = input;
  std::replace(
      encodeFormatFileName.begin(), encodeFormatFileName.end(), '.', '_');
  std::replace(
      encodeFormatFileName.begin(), encodeFormatFileName.end(), '-', '_');

  std::string header
      = "extern unsigned char " + encodeFormatFileName + "[] = \n\"";
  outstream.write(header.data(), header.length());

//  std::streampos startPos = outstream.tellp();
  base64::encoder E;

  E.encode(instream, outstream);
  outstream.seekp(-1, std::ios::cur);  // rewind by 1 character to delete the
                                       // '\n' written by libb64
//  std::streampos endPos = outstream.tellp();
  // size_t len = endPos - startPos;
  std::string footer = "\";\nextern unsigned int " + encodeFormatFileName
                       + "_len = sizeof(" + encodeFormatFileName + ") - 1;\n";

  outstream.write(footer.data(), footer.length());

  return 0;
}
