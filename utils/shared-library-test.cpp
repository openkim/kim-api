//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2013--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Alexander Stukowski
//

//
// Release: This file is part of the kim-api-2.2.0 package.
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
