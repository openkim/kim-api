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
#include "KIM_Logger.hpp"
#include "old_KIM_API_status.h"

//==============================================================================
//
// This is the standard interface to KIM Model Drivers
//
//==============================================================================

//******************************************************************************
extern "C"
{
int model_driver_init(KIM::Model * const model,
                      char const * const paramfile_names, int const nmstrlen,
                      int const numparamfiles)
{
  int ier;

  // read input files, convert units if needed, compute
  // interpolation coefficients, set cutoff, and publish parameters
  LennardJones612* const modelObject
      = new LennardJones612(model, paramfile_names, nmstrlen,
                            numparamfiles, &ier);
  if (ier < KIM_STATUS_OK)
  {
    // constructor already reported the error
    delete modelObject;
    return ier;
  }

  // register pointer to LennardJones612 object in KIM object
  model->set_model_buffer(static_cast<void*>(modelObject));

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}
}

//==============================================================================
//
// Implementation of LennardJones612 public wrapper functions
//
//==============================================================================

//******************************************************************************
LennardJones612::LennardJones612(
    KIM::Model * const model,
    char const* const parameterFileNames,
    int const parameterFileNameLength,
    int const numberParameterFiles,
    int* const ier)
{
  implementation_ = new LennardJones612Implementation(
      model,
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
// static member function
int LennardJones612::Destroy(KIM::Model * const model)
{
  LennardJones612 * modelObject;
  model->get_model_buffer(reinterpret_cast<void**>(&modelObject));

  if (modelObject != NULL)
  {
    // delete object itself
    delete modelObject;

    // nullify model buffer
    model->set_model_buffer(NULL);
  }

  // everything is good
  return KIM_STATUS_OK;
}

//******************************************************************************
// static member function
int LennardJones612::Reinit(KIM::Model * const model)
{
  LennardJones612 * modelObject;
  model->get_model_buffer(reinterpret_cast<void**>(&modelObject));

  return modelObject->implementation_->Reinit(model);
}

//******************************************************************************
// static member function
int LennardJones612::Compute(KIM::Model const * const model)
{
  LennardJones612 * modelObject;
  model->get_model_buffer(reinterpret_cast<void**>(&modelObject));

  return modelObject->implementation_->Compute(model);
}
