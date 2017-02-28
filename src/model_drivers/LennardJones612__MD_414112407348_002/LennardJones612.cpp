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
// Copyright (c) 2015, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Andrew Akerson
//


#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>

#include "LennardJones612.hpp"
#include "LennardJones612Implementation.hpp"
#include "KIM_API_status.h"

//==============================================================================
//
// This is the standard interface to KIM Model Drivers
//
//==============================================================================

//******************************************************************************
int model_driver_init(void* km, char* paramfile_names, int* nmstrlen,
                      int* numparamfiles)
{
  KIM_API_model* const pkim = *static_cast<KIM_API_model**>(km);
  int ier;

  // read input files, convert units if needed, compute
  // interpolation coefficients, set cutoff, and publish parameters
  LennardJones612* const modelObject
      = new LennardJones612(pkim, paramfile_names, *nmstrlen,
                            *numparamfiles, &ier);
  if (ier < KIM_STATUS_OK)
  {
    // constructor already reported the error
    delete modelObject;
    return ier;
  }

  // register pointer to LennardJones612 object in KIM object
  pkim->set_model_buffer(static_cast<void*>(modelObject), &ier);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "set_model_buffer", ier);
    delete modelObject;
    return ier;
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//==============================================================================
//
// Implementation of LennardJones612 public wrapper functions
//
//==============================================================================

//******************************************************************************
LennardJones612::LennardJones612(
    KIM_API_model* const pkim,
    char const* const parameterFileNames,
    int const parameterFileNameLength,
    int const numberParameterFiles,
    int* const ier)
{
  implementation_ = new LennardJones612Implementation(
      pkim,
      parameterFileNames,
      parameterFileNameLength,
      numberParameterFiles,
      ier);
}

//******************************************************************************
LennardJones612::~LennardJones612()
{
  delete implementation_;
}

//******************************************************************************
int LennardJones612::Destroy(void* kimmdl)  // static member function
{
  KIM_API_model* const pkim = *static_cast<KIM_API_model**>(kimmdl);
  int ier;
  LennardJones612* const modelObject
      = (LennardJones612*) pkim->get_model_buffer(&ier);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_model_buffer", ier);
    return ier;
  }

  if (modelObject != NULL)
  {
    // delete object itself
    delete modelObject;

    // nullify model buffer
    pkim->set_model_buffer(NULL, &ier);
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612::Reinit(void* kimmdl)  // static member function
{
  KIM_API_model* const pkim = *static_cast<KIM_API_model**>(kimmdl);
  int ier;
  LennardJones612* const modelObject
      = (LennardJones612*) pkim->get_model_buffer(&ier);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_model_buffer", ier);
    return ier;
  }

  return modelObject->implementation_->Reinit(pkim);
}

//******************************************************************************
int LennardJones612::Compute(void* kimmdl)  // static member function
{
  KIM_API_model* const pkim = *static_cast<KIM_API_model**>(kimmdl);
  int ier;
  LennardJones612* const modelObject
      = static_cast<LennardJones612*>(pkim->get_model_buffer(&ier));
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_model_buffer", ier);
    return ier;
  }

  return modelObject->implementation_->Compute(pkim);
}
