//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Andrew Akerson
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//


#include "LennardJones612.hpp"
#include "LennardJones612Implementation.hpp"
#include <cstddef>


//==============================================================================
//
// This is the standard interface to KIM Model Drivers
//
//==============================================================================

//******************************************************************************
extern "C" {
int model_driver_create(KIM::ModelDriverCreate * const modelDriverCreate,
                        KIM::LengthUnit const requestedLengthUnit,
                        KIM::EnergyUnit const requestedEnergyUnit,
                        KIM::ChargeUnit const requestedChargeUnit,
                        KIM::TemperatureUnit const requestedTemperatureUnit,
                        KIM::TimeUnit const requestedTimeUnit)
{
  int ier;

  // read input files, convert units if needed, compute
  // interpolation coefficients, set cutoff, and publish parameters
  LennardJones612 * const modelObject
      = new LennardJones612(modelDriverCreate,
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
  modelDriverCreate->SetModelBufferPointer(static_cast<void *>(modelObject));

  // everything is good
  ier = false;
  return ier;
}
}  // extern "C"

//==============================================================================
//
// Implementation of LennardJones612 public wrapper functions
//
//==============================================================================

//******************************************************************************
LennardJones612::LennardJones612(
    KIM::ModelDriverCreate * const modelDriverCreate,
    KIM::LengthUnit const requestedLengthUnit,
    KIM::EnergyUnit const requestedEnergyUnit,
    KIM::ChargeUnit const requestedChargeUnit,
    KIM::TemperatureUnit const requestedTemperatureUnit,
    KIM::TimeUnit const requestedTimeUnit,
    int * const ier)
{
  implementation_ = new LennardJones612Implementation(modelDriverCreate,
                                                      requestedLengthUnit,
                                                      requestedEnergyUnit,
                                                      requestedChargeUnit,
                                                      requestedTemperatureUnit,
                                                      requestedTimeUnit,
                                                      ier);
}

//******************************************************************************
LennardJones612::~LennardJones612() { delete implementation_; }

//******************************************************************************
// static member function
int LennardJones612::Destroy(KIM::ModelDestroy * const modelDestroy)
{
  LennardJones612 * modelObject;
  modelDestroy->GetModelBufferPointer(reinterpret_cast<void **>(&modelObject));

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
int LennardJones612::Refresh(KIM::ModelRefresh * const modelRefresh)
{
  LennardJones612 * modelObject;
  modelRefresh->GetModelBufferPointer(reinterpret_cast<void **>(&modelObject));

  return modelObject->implementation_->Refresh(modelRefresh);
}

//******************************************************************************
// static member function
int LennardJones612::Compute(
    KIM::ModelCompute const * const modelCompute,
    KIM::ModelComputeArguments const * const modelComputeArguments)
{
  LennardJones612 * modelObject;
  modelCompute->GetModelBufferPointer(reinterpret_cast<void **>(&modelObject));

  return modelObject->implementation_->Compute(modelCompute,
                                               modelComputeArguments);
}

//******************************************************************************
// static member function
int LennardJones612::ComputeArgumentsCreate(
    KIM::ModelCompute const * const modelCompute,
    KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
{
  LennardJones612 * modelObject;
  modelCompute->GetModelBufferPointer(reinterpret_cast<void **>(&modelObject));

  return modelObject->implementation_->ComputeArgumentsCreate(
      modelComputeArgumentsCreate);
}

//******************************************************************************
// static member function
int LennardJones612::ComputeArgumentsDestroy(
    KIM::ModelCompute const * const modelCompute,
    KIM::ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
{
  LennardJones612 * modelObject;
  modelCompute->GetModelBufferPointer(reinterpret_cast<void **>(&modelObject));

  return modelObject->implementation_->ComputeArgumentsDestroy(
      modelComputeArgumentsDestroy);
}
