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
// Copyright (c) 2013--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include "KIM_Configuration.hpp"
#include "KIM_SharedLibrary.hpp"
#include "KIM_Version.hpp"
#include "old_KIM_API_DIRS.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <string>
#include <vector>
using namespace OLD_KIM;

void usage(std::string name)
{
  size_t beg = name.find_last_of("/");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

  // Follows docopt.org format
  std::cerr << "Usage:\n"
            << "  " << name << " "
            << "<simulator-model-name>\n"
            << "  " << name << " "
            << "<simulator-model-name> "
            << "number-of-parameter-files\n"
            << "  " << name << " "
            << "<simulator-model-name> "
            << "(metadata-file | <one-based-parameter-file-index>) "
            << "(data | name)\n"
            << "  " << name << " "
            << "--version\n";
  // note: this interface is likely to change in future kim-api releases
}


int main(int argc, char * argv[])
{
  if ((argc < 2) || (argc >= 5))
  {
    usage(argv[0]);
    return -1;
  }

  if ((argc == 2) && (std::string(argv[1]) == "--version"))
  {
    std::cout << KIM_VERSION_STRING << std::endl;
    return 0;
  }

  char const * modelname = argv[1];

  int error;
  KIM::SharedLibrary sharedLib(NULL);

  // get item shared library file name
  std::vector<std::string> item;
  bool accessible = findItem(KIM_SIMULATOR_MODELS, modelname, &item, NULL);
  if (accessible)
  {
    error = sharedLib.Open(item[OLD_KIM::IE_FULLPATH]);
    if (error)
    {
      std::cout << "* Error: A problem occurred with the Simulator Model "
                   "shared library file for name: '"
                << modelname << "'" << std::endl;
      return 1;
    }

    if (argc == 2)
    {
      std::cout << "SIMULATOR_MODEL";
      return 0;
    }
  }
  else
  {
    if (argc == 2)
    {
      std::cout << "NOT_A_SIMULATOR_MODEL";
      return 4;
    }
    else
    {
      std::cout << "* Error: The Simulator Model shared library file is not "
                   "readable for name: '"
                << modelname << "'" << std::endl;
      return 2;
    }
  }

  if (std::string(argv[2]) == "number-of-parameter-files")
  {
    int number;
    error = sharedLib.GetNumberOfParameterFiles(&number);
    if (error)
    {
      std::cout << "* Error: cannot get number of parameter files."
                << std::endl;
      sharedLib.Close();
      return 5;
    }
    else
    {
      std::cout << number << std::endl;
      sharedLib.Close();
      return 0;
    }
  }
  else if (argc < 4)
  {
    usage(argv[0]);
    return -2;
  }
  else
  {
    std::string name;
    unsigned int len;
    unsigned char const * data;
    if (std::string(argv[2]) == "metadata-file")
    {
      error = sharedLib.GetMetadataFile(&name, &len, &data);
      if (error)
      {
        std::cout << "* Error: unable to get metadata file." << std::endl;
        return 6;
      }
    }
    else  // one-based parameter file index provided
    {
      int number;
      error = sharedLib.GetNumberOfParameterFiles(&number);
      int index = atol(argv[2]);
      if ((index < 1) || (index > number))
      {
        std::cout << "* Error: invalid index provided." << std::endl;
        return 7;
      }
      else
      {
        // The command line interface uses a one-based index for compatibility
        // with external scripts.  The SharedLibrary API uses a zero-based
        // index consistent with standard C++ convention.
        error = sharedLib.GetParameterFile(index - 1, &name, &len, &data);
        if (error)
        {
          std::cout << "* Error: unable to get parameter file." << std::endl;
          return 8;
        }
      }
    }

    if (std::string(argv[3]) == "name") { std::cout << name << std::endl; }
    else if (std::string(argv[3]) == "data")
    {
      fwrite(data, sizeof(unsigned char), len, stdout);
    }

    sharedLib.Close();
    return 0;
  }

  // something is wrong...
  return 9;
}
