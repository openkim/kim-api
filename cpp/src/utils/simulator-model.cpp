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
// Copyright (c) 2013--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <iostream>
#include <string>
#include <list>
#include <vector>
#include <cstring>
#include <dlfcn.h>
#include <cstdio>
#include "old_KIM_API_DIRS.h"
using namespace OLD_KIM;

#ifndef MODELLIBFILE
#error
#endif


void usage(char const* const name)
{
  std::cerr << "usage: "
            << name << " "
            << "<simulator model name> "
            << "<metadata_file | number_of_parameter_files "
            << "| parameter file index>\n";
      // note: this interface is likely to change in future kim-api releases
      }


int main(int argc, char* argv[])
{
  if ((argc < 3) || (argc >= 4))
  {
    usage(argv[0]);
    return -1;
  }

  char const * modelname = argv[1];

  std::string symbol;
  int argFlag;
  if (std::string(argv[2]) == "number_of_parameter_files")
  {
    argFlag = 0;
    symbol = "number_of_parameter_files";
  }
  else if (std::string(argv[2]) == "metadata_file")
  {
    argFlag = 1;
    symbol = "metadata_file";
  }
  else
  {
    argFlag = 1;
    symbol = "parameter_file_" + std::string(argv[2]);
  }

  std::vector<std::string> item;
  bool accessible = findItem(KIM_MODELS_DIR, modelname, &item);
  void * model_lib_handle;
  if (accessible)
  {
    std::string libFileName
        = item[IE_DIR] + "/" + item[IE_NAME] + "/" + MODELLIBFILE + ".so";
    model_lib_handle = dlopen(libFileName.c_str(), RTLD_NOW);
  }
  if(!accessible)
  {
    std::cout<< "* Error: The Model shared library file is not readable for Model name: '" << modelname << "'" <<std::endl;
    std::cout<<dlerror()<<std::endl;
    return 1;
  }
  else if(NULL == model_lib_handle) {
    std::cout<< "* Error: A problem occurred with the Model shared library file for Model name: '" << modelname << "'" <<std::endl;
    std::cout<<dlerror()<<std::endl;
    return 2;
  }

  char const * itemType
      = (char const *) dlsym(model_lib_handle,"kim_item_type");
  const char *dlsym_error = dlerror();
  if (dlsym_error) {
    std::cout << "* Error: Cannot load symbol: " << dlsym_error <<std::endl;
    dlclose(model_lib_handle);
    return 3;
  }

  if (std::string(itemType) != "simulator-model")
  {
    std::cout << "* Error: not an simulator model" <<std::endl;
    dlclose(model_lib_handle);
    return 4;
  }

  if (argFlag)
  {
    unsigned char const * filePointer
        = (unsigned char const *) dlsym(model_lib_handle, symbol.c_str());
    dlsym_error = dlerror();
    if (dlsym_error) {
      std::cout << "* Error: Cannot load symbol: " << dlsym_error <<std::endl;
      dlclose(model_lib_handle);
      return 5;
    }
    else
    {
      symbol += "_len";
      unsigned int const * fileLength
          = (unsigned int const *) dlsym(model_lib_handle, symbol.c_str());
      dlsym_error = dlerror();
      if (dlsym_error) {
        std::cout << "* Error: Cannot load symbol: " << dlsym_error <<std::endl;
        dlclose(model_lib_handle);
        return 6;
      }
      else
      {
        fwrite(filePointer, sizeof(unsigned char), *fileLength, stdout);
      }
    }
  }
  else
  {
    int const * number
        = (int const *) dlsym(model_lib_handle, symbol.c_str());
    dlsym_error = dlerror();
    if (dlsym_error) {
      std::cout << "* Error: Cannot load symbol: " << dlsym_error <<std::endl;
      dlclose(model_lib_handle);
      return 6;
    }
    else
      std::cout << *number << std::endl;
  }

  dlclose(model_lib_handle);
  return 0;
}
