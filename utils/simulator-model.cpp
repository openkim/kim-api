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
// Copyright (c) 2013--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2-2.0.0-beta.3 package.
//

#include "KIM_Configuration.hpp"
#include "KIM_SharedLibrary.hpp"
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
            << "(metadata-file | <parameter-file-index>) "
            << "(data | name)\n";
  // note: this interface is likely to change in future kim-api releases
}


int main(int argc, char * argv[])
{
  if ((argc < 2) || (argc >= 5))
  {
    usage(argv[0]);
    return -1;
  }

  char const * modelname = argv[1];

  int error;
  KIM::SharedLibrary sharedLib(NULL);

  // get item shared library file name
  std::vector<std::string> item;
  bool accessible = findItem(KIM_MODELS_DIR, modelname, &item, NULL);
  if (accessible)
  {
    error = sharedLib.Open(item[OLD_KIM::IE_FULLPATH]);
    if (error)
    {
      std::cout << "* Error: A problem occurred with the Model shared library "
                   "file for Model name: '"
                << modelname << "'" << std::endl;
      return 1;
    }
  }
  else
  {
    std::cout << "* Error: The Model shared library file is not readable for "
                 "Model name: '"
              << modelname << "'" << std::endl;
    return 2;
  }

  if (2 == argc)  // Is item a simulator model
  {
    KIM::SharedLibrary::ITEM_TYPE itemType;
    error = sharedLib.GetType(&itemType);
    if (error)
    {
      std::cout << "* Error getting itemType" << std::endl;
      sharedLib.Close();
      return 3;
    }

    std::cout << "Item is a ";
    switch (itemType)
    {
      case KIM::SharedLibrary::STAND_ALONE_MODEL:
        std::cout << "STAND_ALONE_MODEL";
        break;
      case KIM::SharedLibrary::PARAMETERIZED_MODEL:
        std::cout << "PARAMETERIZED_MODEL";
        break;
      case KIM::SharedLibrary::SIMULATOR_MODEL:
        std::cout << "SIMULATOR_MODEL";
        break;
      case KIM::SharedLibrary::MODEL_DRIVER: std::cout << "MODEL_DRIVER"; break;
    };
    std::cout << std::endl;

    int returnCode;
    if (itemType != KIM::SharedLibrary::SIMULATOR_MODEL)
      returnCode = 4;
    else
      returnCode = 0;

    sharedLib.Close();
    return returnCode;
  }
  else if (std::string(argv[2]) == "number-of-parameter-files")
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
    else  // parameter file index provided
    {
      int number;
      error = sharedLib.GetNumberOfParameterFiles(&number);
      int index = atol(argv[2]);
      if ((index < 0) || (index >= number))
      {
        std::cout << "* Error: invalid index provided." << std::endl;
        return 7;
      }
      else
      {
        error = sharedLib.GetParameterFile(index, &name, &len, &data);
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
