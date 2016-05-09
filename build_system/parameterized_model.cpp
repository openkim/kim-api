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
// Copyright (c) 2013--2015, Regents of the University of Minnesota.
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
#include "KIM_API.h"
#include "KIM_API_status.h"
#include "KIM_API_DIRS.h"

// define number of X's to use in mkstemp call
#define NUM_XS 12
#define FL_NAME_LEN (L_tmpnam+25+NUM_XS+1)
#define FL_STRING "kim-model-parameter-file-XXXXXXXXXXXX"

static int process_paramfiles(char* param_file_names, int* nmstrlen);

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
static void* driver_lib_handle = NULL;
static func_ptr driver_destroy;
#endif

extern "C" {

  char MODEL_NAME_STR_compiled_with_version[] =
      "VERSION_FULL_STR";

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
#include <unistd.h>
#include <dlfcn.h>
  static int model_destroy(void* km);
#else
  extern int (* MODEL_DRIVER_NAME_STR_init_pointer)(void*, char*, int*, int*);
#endif


#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
  static int model_destroy(void* km) {
    typedef int (*Driver_Destroy)(void *);//prototype for driver_destroy
    Driver_Destroy drvr_destroy = (Driver_Destroy) driver_destroy;
    int ier = KIM_STATUS_FAIL;
    //call driver_destroy
    if (drvr_destroy != NULL) {
      ier = (*drvr_destroy)(km);
    }

    // close driver library
    dlclose(driver_lib_handle);
    return ier;
  }
#endif

  int MODEL_NAME_STR_init(void* km) {
    int numparamfiles = NUM_PARAMFILES;
    int nmstrlen;
    char* param_file_names = new char[NUM_PARAMFILES*(FL_NAME_LEN)];
    char* param_file_names_copy = new char[NUM_PARAMFILES*(FL_NAME_LEN)];

    if (KIM_STATUS_OK != process_paramfiles(param_file_names, &nmstrlen)) {
      delete [] param_file_names;
      delete [] param_file_names_copy;
      return KIM_STATUS_FAIL;
    }
    for (int i=0; i<NUM_PARAMFILES; ++i)
    {
      strncpy(&(param_file_names_copy[i*(FL_NAME_LEN)]),
              &(param_file_names[i*(FL_NAME_LEN)]),
              FL_NAME_LEN);
    }
#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
    void* tmp_driver_lib_handle = NULL;
    std::stringstream messageText;
    std::list<std::string> lst;
    searchPaths(KIM_MODEL_DRIVERS_DIR, &lst);
    std::list<std::string>::iterator itr;
    for (itr = lst.begin(); itr != lst.end(); ++itr)
    {
      itr->append("/");
      itr->append("MODEL_DRIVER_NAME_STR"); itr->append("/");
      itr->append("MODEL_DRIVER_LIBNAME_STR"); itr->append(".so");
      if (0 == access(itr->c_str(), F_OK))
      {
        tmp_driver_lib_handle = dlopen(itr->c_str(), RTLD_NOW);
      }
      if (tmp_driver_lib_handle != NULL)
      {
        messageText << "  * Found Model Driver shared library file "
                    << itr->c_str() <<std::endl;
        break;
      }
      else
      {
        messageText << "  * Did not find Model Driver shared library file "
                    << itr->c_str() <<std::endl;
      }
    }
    if(tmp_driver_lib_handle == NULL) {
      std::cout << "* Error (MODEL_NAME_STR_init()):" << std::endl
                << messageText.str()
                <<
          " A problem occurred with the MODEL_DRIVER_NAME_STR shared library"
          " for MODEL_NAME_STR" << std::endl;
      std::cout << dlerror() << std::endl;
      delete [] param_file_names;
      delete [] param_file_names_copy;
      return KIM_STATUS_FAIL;
    }
    else
    {
      driver_lib_handle = tmp_driver_lib_handle;
    }

    typedef int (*Driver_Init)(void *km, char* paramfilenames,
                               int* nmstrlen, int* numparamfiles);
    Driver_Init drvr_init =
        *((Driver_Init*)dlsym(driver_lib_handle,
                              "MODEL_DRIVER_NAME_STR_init_pointer"));
    const char *dlsym_error = dlerror();
    if (dlsym_error)
    {
      std::cout << "* Error (MODEL_NAME_STR_init()): Cannot load symbol: "
                << dlsym_error << std::endl;
      dlclose(driver_lib_handle);
      delete [] param_file_names;
      delete [] param_file_names_copy;
      return KIM_STATUS_FAIL;
    }
    int ier = 0;
    ier = (*drvr_init)(km, param_file_names_copy, &nmstrlen, &numparamfiles);
    for (int i=0; i<numparamfiles; ++i) {
      remove(&(param_file_names[i*(FL_NAME_LEN)]));
    }
    delete [] param_file_names;
    delete [] param_file_names_copy;
    param_file_names = NULL;
    param_file_names_copy = NULL;
    if (KIM_STATUS_OK > ier) return ier;

    driver_destroy = (*((KIM_API_model**)km))
        ->get_method((char*) "destroy", &ier);
    (*((KIM_API_model**)km))->set_method((char*) "destroy",1,
                                         (func_ptr) model_destroy);
#else
    int ier = (*MODEL_DRIVER_NAME_STR_init_pointer)(km, param_file_names_copy,
                                                    &nmstrlen, &numparamfiles);
    for (int i=0; i<numparamfiles; ++i) {
      remove(&(param_file_names[i*(FL_NAME_LEN)]));
    }
    delete [] param_file_names;
    delete [] param_file_names_copy;
    param_file_names = NULL;
    param_file_names_copy = NULL;
    if (KIM_STATUS_OK > ier) return ier;
#endif

    return KIM_STATUS_OK;
  }
}

int (* MODEL_NAME_STR_init_pointer)(void*) = MODEL_NAME_STR_init;

static int process_paramfiles(char* param_file_names, int* nmstrlen)
{
  *nmstrlen = FL_NAME_LEN;

  const unsigned char* paramfile_strings[NUM_PARAMFILES];
  PARAMFILE_POINTERS_GO_HERE;
  unsigned int paramfile_strings_len[NUM_PARAMFILES];
  PARAMFILE_LENS_GO_HERE;

  for (int i=0; i<NUM_PARAMFILES; ++i)
  {
    int ret;
    ret = snprintf(&(param_file_names[i*(FL_NAME_LEN)]),
                   FL_NAME_LEN, "%s/" FL_STRING, P_tmpdir);
    if (ret >= FL_NAME_LEN)
    {
      std::cerr
          << "FL_NAME_LEN too short for this system: Failed in process_paramfiles()."
          << std::endl;
      return KIM_STATUS_FAIL;
    }
    int fileid = mkstemp(&(param_file_names[i*(FL_NAME_LEN)]));
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
