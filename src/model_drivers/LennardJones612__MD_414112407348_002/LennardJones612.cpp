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


//==============================================================================
//
// This is the standard interface to KIM Model Drivers
//
//==============================================================================

//******************************************************************************
extern "C"
{
int model_driver_init(
    KIM::ModelDriverInitialization * const modelDriverInitialization,
    KIM::LengthUnit const requestedLengthUnit,
    KIM::EnergyUnit const requestedEnergyUnit,
    KIM::ChargeUnit const requestedChargeUnit,
    KIM::TemperatureUnit const requestedTemperatureUnit,
    KIM::TimeUnit const requestedTimeUnit)
{
  int ier;

  // read input files, convert units if needed, compute
  // interpolation coefficients, set cutoff, and publish parameters
  LennardJones612* const modelObject
      = new LennardJones612(modelDriverInitialization,
                            requestedLengthUnit,
                            requestedEnergyUnit,
                            requestedChargeUnit,
                            requestedTemperatureUnit,
                            requestedTimeUnit,
                            &ier);
  if (ier)
  {
    // constructor already reported the error
    delete modelObject;
    return ier;
  }

  // register pointer to LennardJones612 object in KIM object
  modelDriverInitialization->set_model_buffer(static_cast<void*>(modelObject));

  // everything is good
  ier = false;
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
    KIM::ModelDriverInitialization * const modelDriverInitialization,
    KIM::LengthUnit const requestedLengthUnit,
    KIM::EnergyUnit const requestedEnergyUnit,
    KIM::ChargeUnit const requestedChargeUnit,
    KIM::TemperatureUnit const requestedTemperatureUnit,
    KIM::TimeUnit const requestedTimeUnit,
    int* const ier)
{
  implementation_ = new LennardJones612Implementation(
      modelDriverInitialization,
      requestedLengthUnit,
      requestedEnergyUnit,
      requestedChargeUnit,
      requestedTemperatureUnit,
      requestedTimeUnit,
      ier);
}

//******************************************************************************
LennardJones612::~LennardJones612()
{
  delete implementation_;
}

//******************************************************************************
// static member function
int LennardJones612::Destroy(KIM::ModelDestroy * const modelDestroy)
{
  LennardJones612 * modelObject;
  modelDestroy->get_model_buffer(reinterpret_cast<void**>(&modelObject));

  if (modelObject != NULL)
  {
    // delete object itself
    delete modelObject;
  }

  // everything is good
  return false;
}

//******************************************************************************
// static member function
int LennardJones612::Reinit(
    KIM::ModelReinitialization * const modelReinitialization)
{
  LennardJones612 * modelObject;
  modelReinitialization->get_model_buffer(
      reinterpret_cast<void**>(&modelObject));

  return modelObject->implementation_->Reinit(modelReinitialization);
}

//******************************************************************************
// static member function
int LennardJones612::Compute(KIM::ModelCompute const * const modelCompute)
{
  LennardJones612 * modelObject;
  modelCompute->get_model_buffer(reinterpret_cast<void**>(&modelObject));

  return modelObject->implementation_->Compute(modelCompute);
}
