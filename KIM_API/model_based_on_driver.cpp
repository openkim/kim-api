
//
// Release: This file is part of the openkim-api.git repository.
//
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
// All rights reserved.
//
// Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
//


#include <stdlib.h>
#include <iostream>
#include <string.h>
#include "KIM_API_C.h"
#include "KIM_API.h"

using namespace std;

static char* param_string();

#ifdef KIM_DYNAMIC
   void* driver_lib_handle;
   void* driver_destroy;
#endif

extern "C" {

#ifdef KIM_DYNAMIC
#include <dlfcn.h>
#else
   void MODEL_DRIVER_NAME_LC_STR_init_(void* km, char* paramfile, int* length);
   static void model_destroy(void* km, int* ier);
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

   void MODEL_NAME_LC_STR_init_(void* km) {
      char* param_str = param_string();
      int length = strlen(param_str);
#ifdef KIM_DYNAMIC
      driver_lib_handle = dlopen("MODEL_DRIVER_SO_NAME_STR",RTLD_NOW);
      if (!driver_lib_handle) {
         cout << "Error at " << __LINE__ << " of file " << __FILE__ << endl;
         cout << dlerror() << endl;
      exit(-1);
      }
      typedef void (*Driver_Init)(void *km, char* paramfile, int* length);
      Driver_Init drvr_init = (Driver_Init)dlsym(driver_lib_handle,"MODEL_DRIVER_NAME_LC_STR_init_");
      const char *dlsym_error = dlerror();
      if (dlsym_error) {
         cerr << "Cannot load symbol: " << dlsym_error << endl;
         dlclose(driver_lib_handle);
         exit(-1);
      }
      (*drvr_init)(km, param_str, &length);

      int ier = 0;
      driver_destroy = KIM_API_get_data((void *) *((KIM_API_model**)km), "destroy", &ier);
      KIM_API_set_data((void *) *((KIM_API_model**)km), "destroy",1,(void*) &model_destroy);
#else
      MODEL_DRIVER_NAME_LC_STR_init_(km, param_str, &length);
#endif

   }

}

static char* param_string() {
   return params;
}
