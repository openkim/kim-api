//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Alexander Stukowski
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
#include <cstddef>
#ifndef _WIN32
#include <dlfcn.h>
#else
#include <libloaderapi.h>
#endif
#include <iostream>
#include <string>

void usage(std::string name)
{
  size_t beg = name.find_last_of("/\\");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

  // Follows docopt.org format
  std::cerr << "Usage:\n"
            << "  " << name << " <shared-library-name>\n"
            << "  " << name << " --version" << std::endl;
  // note: this interface is likely to change in future kim-api releases
}

int IsFilePath(std::string const & filePath)
{
  // not very comperhensive but can be improved as needed
  if (filePath.length() == 0) return false;
  return true;
}

int main(int argc, char * argv[])
{
  if (argc != 2)
  {
    usage(argv[0]);
    return 1;
  }
  else if (std::string(argv[1]) == "--version")
  {
    std::cout << KIM_VERSION_STRING << std::endl;
    return 0;
  }
  else
  {
    std::string libFilePath(argv[1]);
    if (!IsFilePath(libFilePath))  // validate lib path
    {
      std::cout << "Invalid <shared-library-name>.\n" << std::endl;
      return 2;
    }

#ifndef _WIN32
    void * sharedLibraryHandle = dlopen(libFilePath.c_str(), RTLD_NOW);
#else
    HMODULE sharedLibraryHandle = LoadLibraryExA(
        libFilePath.c_str(), NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
#endif
    if (sharedLibraryHandle == NULL)
    {
#ifndef _WIN32
      std::cout << "Unable to open shared library.\n" << dlerror() << std::endl;
#else
      std::cout << "Unable to open shared library." << std::endl;
#endif
      return 3;
    }
    else
    {
      std::cout << "Successfully opened shared library." << std::endl;
#ifndef _WIN32
      dlclose(sharedLibraryHandle);
#else
      FreeLibrary(sharedLibraryHandle);
#endif
      return 0;
    }
  }
}
