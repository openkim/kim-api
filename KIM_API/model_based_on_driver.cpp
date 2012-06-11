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
// Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//    Ryan S. Elliott
//    Ellad B. Tadmor
//

//
// Release: This file is part of the openkim-api.git repository.
//


#include <stdlib.h>
#include <iostream>
#include <string.h>
#include "KIM_API_C.h"
#include "KIM_API.h"

using namespace std;

static void process_paramfiles(char* param_file_names, int* nmstrlen);

#ifdef KIM_DYNAMIC
   static void* driver_lib_handle;
   static void* driver_destroy;
#endif

extern "C" {

#ifdef KIM_DYNAMIC
   #include <dlfcn.h>
   static void model_destroy(void* km, int* ier);
#else
   int MODEL_DRIVER_NAME_LC_STR_init_(void* km, char* paramfilenames, int* nmstrlen, int* numparamfiles);
#endif


#ifdef KIM_DYNAMIC
   static void model_destroy(void* km, int* ier) {
      typedef void (*Driver_Destroy)(void *,int *);//prototype for driver_destroy
      Driver_Destroy drvr_destroy = (Driver_Destroy) driver_destroy;
      //call driver_destroy
      if (drvr_destroy != NULL) {
         (*drvr_destroy)(km, ier);
      }

      // close driver library
      dlclose(driver_lib_handle);
   }
#endif

   int MODEL_NAME_LC_STR_init_(void* km) {
      int numparamfiles = NUM_PARAMFILES;
      int nmstrlen;
      char* param_file_names = new char[NUM_PARAMFILES*(L_tmpnam+1)];

      process_paramfiles(param_file_names, &nmstrlen);
#ifdef KIM_DYNAMIC
      driver_lib_handle = dlopen("MODEL_DRIVER_SO_NAME_STR",RTLD_NOW);
      if (!driver_lib_handle) {
         cout << "Error at " << __LINE__ << " of file " << __FILE__ << endl;
         cout << dlerror() << endl;
         return KIM_STATUS_FAIL;
      }
      typedef int (*Driver_Init)(void *km, char* paramfilenames, int* nmstrlen, int* numparamfiles);
      Driver_Init drvr_init = (Driver_Init)dlsym(driver_lib_handle,"MODEL_DRIVER_NAME_LC_STR_init_");
      const char *dlsym_error = dlerror();
      if (dlsym_error) {
         cerr << "Cannot load symbol: " << dlsym_error << endl;
         dlclose(driver_lib_handle);
         return KIM_STATUS_FAIL;
      }
      int ier = 0;
      ier = (*drvr_init)(km, param_file_names, &nmstrlen, &numparamfiles);
      delete [] param_file_names;
      param_file_names = NULL;
      if (KIM_STATUS_OK > ier) return ier;

      driver_destroy = KIM_API_get_data((void *) *((KIM_API_model**)km), "destroy", &ier);
      KIM_API_set_data((void *) *((KIM_API_model**)km), "destroy",1,(void*) &model_destroy);
#else
      int ier = MODEL_DRIVER_NAME_LC_STR_init_(km, param_file_names, &nmstrlen, &numparamfiles);
      delete [] param_file_names;
      param_file_names = NULL;
      if (KIM_STATUS_OK > ier) return ier;
#endif

      return KIM_STATUS_OK;
   }
}

static void process_paramfiles(char* param_file_names, int* nmstrlen)
{
   *nmstrlen = L_tmpnam+1;

   char* paramfile_strings[NUM_PARAMFILES];
   PARAMFILE_POINTERS_GO_HERE;

   char* ret;
   fstream fl;
   for (int i=0; i<NUM_PARAMFILES; ++i)
   {
      ret=tmpnam(&(param_file_names[i*(L_tmpnam+1)]));

      if (ret == NULL)
      {
         cerr << "Cannot obtain unique temporary file name: tmpnam() failed." << endl;
         exit(-1);
      }

      fl.open(&(param_file_names[i*(L_tmpnam+1)]),fstream::out);
      if (fl.fail())
      {
         cerr << "Unable to open temporary file." << endl;
         exit(-1);
      }

      fl << paramfile_strings[i];
      fl.close();
   }
}
