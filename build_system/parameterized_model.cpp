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


#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>

#ifndef KIM_SIMULATOR_HPP_
#include "KIM_Simulator.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

extern "C"
{
#ifndef KIM_SIMULATOR_H_
#include "KIM_Simulator.h"
#endif
}

#include "old_KIM_API_status.h"
#include "old_KIM_API_DIRS.h"

// define number of X's to use in mkstemp call
#define NUM_XS 12
#define FL_NAME_LEN (L_tmpnam+25+NUM_XS+1)
#define FL_STRING "kim-model-parameter-file-XXXXXXXXXXXX"

static int process_paramfiles(char* parameterFileNames, int* nameStringLength);

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
static void* driver_lib_handle = NULL;
static KIM::LanguageName driver_destroy_lang;
static KIM::func * driver_destroy;
#endif

namespace KIM
{
int get_destroy_helper(KIM::Simulator const * const simulator,
                       KIM::LanguageName * const languageName,
                       KIM::func ** const destroyFunctionPointer);
}

extern "C" {

char MODEL_NAME_STR_compiled_with_version[] =
    "VERSION_FULL_STR";

int MODEL_NAME_STR_language = 1;

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
#include <unistd.h>
#include <dlfcn.h>
static int model_destroy(KIM::Simulator * const simulator);
#else
extern int (* MODEL_DRIVER_NAME_STR_init_pointer)(void*, char*, int*, int*);
#endif


#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
static int model_destroy(KIM::Simulator * const simulator) {
  typedef int (*Driver_Destroy_cpp)(KIM::Simulator * const simulator);//prototype for c++
  typedef int (*Driver_Destroy_c)(KIM_Simulator * const simulator);//prototype for c
  typedef void (*Driver_Destroy_Fortran)(KIM_Simulator * const simulator, int * const ierr);//prototype for fortran
  Driver_Destroy_cpp drvr_destroy_cpp = (Driver_Destroy_cpp) driver_destroy;
  Driver_Destroy_c drvr_destroy_c = (Driver_Destroy_c) driver_destroy;
  Driver_Destroy_Fortran drvr_destroy_fortran = (Driver_Destroy_Fortran) driver_destroy;
  int ier = KIM_STATUS_FAIL;
  //call driver_destroy
  if (drvr_destroy_cpp != NULL) {
    if(driver_destroy_lang == KIM::LANGUAGE_NAME::Cpp){
      ier = (*drvr_destroy_cpp)(simulator);
    }
    else if (driver_destroy_lang == KIM::LANGUAGE_NAME::C){
      KIM_Simulator cSim;
      cSim.p = (void *) simulator;
      ier = (*drvr_destroy_c)(&cSim);
    }
    else if (driver_destroy_lang == KIM::LANGUAGE_NAME::Fortran){
      KIM_Simulator cSim;
      cSim.p = (void *) simulator;
      (*drvr_destroy_fortran)(&cSim, &ier);
    }
    else {
      std::cout << "* Error (MODEL_NAME_STR_init()):" << std::endl
                << "Unknown destroy language type" << std::endl;
      return KIM_STATUS_FAIL;
    }
  }

  // close driver library
  dlclose(driver_lib_handle);
  return ier;
}
#endif

int MODEL_NAME_STR_init(KIM::Simulator * const simulator) {
  int numparamfiles = NUM_PARAMFILES;
  int nameStringLength;
  char* parameterFileNames = new char[NUM_PARAMFILES*(FL_NAME_LEN)];
  char* parameterFileNames_copy = new char[NUM_PARAMFILES*(FL_NAME_LEN)];

  if (KIM_STATUS_OK != process_paramfiles(parameterFileNames, &nameStringLength)) {
    delete [] parameterFileNames;
    delete [] parameterFileNames_copy;
    return KIM_STATUS_FAIL;
  }
  for (int i=0; i<NUM_PARAMFILES; ++i)
  {
    strncpy(&(parameterFileNames_copy[i*(FL_NAME_LEN)]),
            &(parameterFileNames[i*(FL_NAME_LEN)]),
            FL_NAME_LEN);
  }
#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
  void* tmp_driver_lib_handle = NULL;
  std::stringstream messageText;
  std::vector<std::string> item;
  bool accessible
      = findItem(OLD_KIM::KIM_MODEL_DRIVERS_DIR, "MODEL_DRIVER_NAME_STR", &item);
  if (accessible)
  {
    std::string libFileName
        = item[1] + "/" + item[0] + "/" + "MODEL_DRIVER_LIBNAME_STR" + ".so";
    tmp_driver_lib_handle = dlopen(libFileName.c_str(), RTLD_NOW);
  }
  if(tmp_driver_lib_handle == NULL) {
    std::cout << "* Error (MODEL_NAME_STR_init()):" << std::endl
              << messageText.str()
              <<
        " A problem occurred with the MODEL_DRIVER_NAME_STR shared library"
        " for MODEL_NAME_STR" << std::endl;
    std::cout << dlerror() << std::endl;
    delete [] parameterFileNames;
    delete [] parameterFileNames_copy;
    return KIM_STATUS_FAIL;
  }
  else
  {
    driver_lib_handle = tmp_driver_lib_handle;
  }

  std::stringstream driver_lang_name;
  driver_lang_name << "MODEL_DRIVER_NAME_STR" << "_language";
  int driver_language = * (int*) dlsym(driver_lib_handle,driver_lang_name.str().c_str());

  typedef int (*Driver_Init_cpp)(KIM::Simulator * const simulator, char const * const parameterFileNames,
                                 int const nameStringLength, int const numberOfParameterFiles);
  typedef int (*Driver_Init_c)(KIM_Simulator * const simulator, char const * const parameterFileNames,
                               int const nameStringLength, int const numberOfParameterFiles);
  typedef void (*Driver_Init_Fortran)(KIM_Simulator * const simulator, char const * const parameterFileNames,
                                      int const nameStringLength, int const numberOfParameterFiles,
                                      int * const ierr);
  Driver_Init_cpp drvr_init_cpp =
      *((Driver_Init_cpp*)dlsym(driver_lib_handle,
                                "MODEL_DRIVER_NAME_STR_init_pointer"));
  Driver_Init_c drvr_init_c = ((Driver_Init_c) drvr_init_cpp);
  Driver_Init_Fortran drvr_init_fortran = ((Driver_Init_Fortran) drvr_init_cpp);
  const char *dlsym_error = dlerror();
  if (dlsym_error)
  {
    std::cout << "* Error (MODEL_NAME_STR_init()): Cannot load symbol: "
              << dlsym_error << std::endl;
    dlclose(driver_lib_handle);
    delete [] parameterFileNames;
    delete [] parameterFileNames_copy;
    return KIM_STATUS_FAIL;
  }
  int ier = 0;

  if (driver_language == 1)
  {
    ier = (*drvr_init_cpp)(simulator, parameterFileNames_copy, nameStringLength, numparamfiles);
  }
  else if (driver_language == 2)
  {
    KIM_Simulator cSim;
    cSim.p = simulator;
    ier = (*drvr_init_c)(&cSim, parameterFileNames_copy, nameStringLength, numparamfiles);
  }
  else if (driver_language == 3)
  {
    KIM_Simulator cSim;
    cSim.p = simulator;
    (*drvr_init_fortran)(&cSim, parameterFileNames_copy, nameStringLength, numparamfiles, &ier);
  }
  else
  {
    std::cout << "* Error (MODEL_NAME_STR_init()): Unknown MODEL_DRIVER_INIT_FUNCTION_LANG value"
              << std::endl;
    return KIM_STATUS_FAIL;
  }

  for (int i=0; i<numparamfiles; ++i) {
    remove(&(parameterFileNames[i*(FL_NAME_LEN)]));
  }
  delete [] parameterFileNames;
  delete [] parameterFileNames_copy;
  parameterFileNames = NULL;
  parameterFileNames_copy = NULL;
  if (ier) return ier;  // driver init functions should return 2.0 codes

  ier = get_destroy_helper(simulator, &driver_destroy_lang, (KIM::func **) &driver_destroy);
  simulator->set_destroy(KIM::LANGUAGE_NAME::Cpp, (KIM::func *) model_destroy);
#else
  int ier = (*MODEL_DRIVER_NAME_STR_init_pointer)(km, parameterFileNames_copy,
                                                  &nameStringLength, &numparamfiles);
  for (int i=0; i<numparamfiles; ++i) {
    remove(&(parameterFileNames[i*(FL_NAME_LEN)]));
  }
  delete [] parameterFileNames;
  delete [] parameterFileNames_copy;
  parameterFileNames = NULL;
  parameterFileNames_copy = NULL;
  if (KIM_STATUS_OK > ier) return ier;
#endif
  return false;  // Models should return 2.0 codes
}
}

int (* MODEL_NAME_STR_init_pointer)(KIM::Simulator * const simulator) = MODEL_NAME_STR_init;

static int process_paramfiles(char* parameterFileNames, int* nameStringLength)
{
  *nameStringLength = FL_NAME_LEN;

  const unsigned char* paramfile_strings[NUM_PARAMFILES];
  PARAMFILE_POINTERS_GO_HERE;
  unsigned int paramfile_strings_len[NUM_PARAMFILES];
  PARAMFILE_LENS_GO_HERE;

  for (int i=0; i<NUM_PARAMFILES; ++i)
  {
    int ret;
    ret = snprintf(&(parameterFileNames[i*(FL_NAME_LEN)]),
                   FL_NAME_LEN, "%s/" FL_STRING, P_tmpdir);
    if (ret >= FL_NAME_LEN)
    {
      std::cerr
          << "FL_NAME_LEN too short for this system: Failed in process_paramfiles()."
          << std::endl;
      return KIM_STATUS_FAIL;
    }
    int fileid = mkstemp(&(parameterFileNames[i*(FL_NAME_LEN)]));
    if (fileid == -1)
    {
      std::cerr << "Cannot open temporary file: mkstemp() failed."
                << std::endl;
      return KIM_STATUS_FAIL;
    }

    FILE* fl = fdopen(fileid,"w");
    fwrite(paramfile_strings[i], paramfile_strings_len[i], 1, fl);
    fclose(fl);  // also closed the fileid
  }

  return KIM_STATUS_OK;
}
