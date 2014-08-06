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
// Copyright (c) 2013--2014, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstdlib>
#include <iostream>
#include <fstream>
#include "KIM_API.h"
#include "KIM_API_status.h"
#include "KIM_API_DIRS.h"

static int process_paramfiles(char* param_file_names, int* nmstrlen);

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
static void* driver_lib_handle;
static func_ptr driver_destroy;
#endif

extern "C" {

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
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
    char* param_file_names = new char[NUM_PARAMFILES*(L_tmpnam+1)];

    if (KIM_STATUS_OK != process_paramfiles(param_file_names, &nmstrlen)) {
      return KIM_STATUS_FAIL;
    }
#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
    std::list<std::string> lst;
    directoryPath(KIM_MODEL_DRIVERS_DIR, &lst);
    std::list<std::string>::iterator itr;
    for (itr = lst.begin(); itr != lst.end(); ++itr)
    {
      itr->append("/");
      itr->append("MODEL_DRIVER_NAME_STR"); itr->append("/");
      itr->append("MODEL_DRIVER_LIBNAME_STR"); itr->append(".so");
      //std::cout<< "* Info (MODEL_NAME_STR_init): Looking for Model Driver"
      //    " shared library file " << itr->c_str() <<std::endl;
      driver_lib_handle = dlopen(itr->c_str(), RTLD_NOW);
      if (driver_lib_handle != NULL) break;
      //std::cout<< "* Error (MODEL_NAME_STR_init): Cannot find Model Driver"
      //    " shared library file for Model Driver name: ";
      //std::cout<< "MODEL_DRIVER_NAME_STR" <<std::endl<<dlerror()<<std::endl;
    }
    if(driver_lib_handle == NULL) {
      std::cerr << "Cannot load MODEL_DRIVER_NAME_STR shared library for"
          " MODEL_NAME_STR" << std::endl;
      return KIM_STATUS_FAIL;
    }
    else
    {
      //std::cout<< "* Info (MODEL_NAME_STR_init): Found Model Driver shared"
      //    " library file for Model Driver name: "
      //         << "MODEL_DRIVER_NAME_STR" << std::endl;
    }

    typedef int (*Driver_Init)(void *km, char* paramfilenames,
                               int* nmstrlen, int* numparamfiles);
    Driver_Init drvr_init =
        *((Driver_Init*)dlsym(driver_lib_handle,
                              "MODEL_DRIVER_NAME_STR_init_pointer"));
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
      std::cerr << "Cannot load symbol: " << dlsym_error << std::endl;
      dlclose(driver_lib_handle);
      return KIM_STATUS_FAIL;
    }
    int ier = 0;
    ier = (*drvr_init)(km, param_file_names, &nmstrlen, &numparamfiles);
    for (int i=0; i<numparamfiles; ++i) {
      remove(&(param_file_names[i*(L_tmpnam+1)]));
    }
    delete [] param_file_names;
    param_file_names = NULL;
    if (KIM_STATUS_OK > ier) return ier;

    driver_destroy = (*((KIM_API_model**)km))
        ->get_method((char*) "destroy", &ier);
    (*((KIM_API_model**)km))->set_method((char*) "destroy",1,
                                         (func_ptr) model_destroy);
#else
    int ier = (*MODEL_DRIVER_NAME_STR_init_pointer)(km, param_file_names,
                                                    &nmstrlen, &numparamfiles);
    delete [] param_file_names;
    param_file_names = NULL;
    if (KIM_STATUS_OK > ier) return ier;
#endif

    return KIM_STATUS_OK;
  }
}

int (* MODEL_NAME_STR_init_pointer)(void*) = MODEL_NAME_STR_init;

static int process_paramfiles(char* param_file_names, int* nmstrlen)
{
  *nmstrlen = L_tmpnam+1;

  const char** paramfile_strings[NUM_PARAMFILES];
  PARAMFILE_POINTERS_GO_HERE;
  int paramfile_strings_chunks[NUM_PARAMFILES];
  PARAMFILE_CHUNKS_GO_HERE;

  char* ret;
  std::fstream fl;
  for (int i=0; i<NUM_PARAMFILES; ++i)
  {
    // Note: the use of tmpnam() below may create a security hole.  Users should
    //       avoid running KIM Models with root (or other special) previlages.
    ret=tmpnam(&(param_file_names[i*(L_tmpnam+1)]));

    if (ret == NULL)
    {
      std::cerr << "Cannot obtain unique temporary file name: tmpnam() failed."
                << std::endl;
      return KIM_STATUS_FAIL;
    }

    fl.open(&(param_file_names[i*(L_tmpnam+1)]),std::fstream::out);
    if (fl.fail())
    {
      std::cerr << "Unable to open temporary file." << std::endl;
      return KIM_STATUS_FAIL;
    }

    for (int j=0; j<paramfile_strings_chunks[i]; ++j)
    {
      fl << paramfile_strings[i][j];
    }
    fl.close();
  }

  return KIM_STATUS_OK;
}
