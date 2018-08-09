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


#ifndef LENNARD_JONES_612_HPP_
#define LENNARD_JONES_612_HPP_

#include "KIM_ModelDriverHeaders.hpp"

extern "C"
{
int model_driver_create(
    KIM::ModelDriverCreate * const modelDriverCreate,
    KIM::LengthUnit const requestedLengthUnit,
    KIM::EnergyUnit const requestedEnergyUnit,
    KIM::ChargeUnit const requestedChargeUnit,
    KIM::TemperatureUnit const requestedTemperatureUnit,
    KIM::TimeUnit const requestedTimeUnit);
}

class LennardJones612Implementation;

class LennardJones612
{
 public:
  LennardJones612(
      KIM::ModelDriverCreate* const modelDriverCreate,
      KIM::LengthUnit const requestedLengthUnit,
      KIM::EnergyUnit const requestedEnergyUnit,
      KIM::ChargeUnit const requestedChargeUnit,
      KIM::TemperatureUnit const requestedTemperatureUnit,
      KIM::TimeUnit const requestedTimeUnit,
      int* const ier);
  ~LennardJones612();

  // no need to make these "extern" since KIM will only access them
  // via function pointers.  "static" is required so that there is not
  // an implicit this pointer added to the prototype by the C++ compiler
  static int Destroy(KIM::ModelDestroy * const modelDestroy);
  static int Refresh(KIM::ModelRefresh * const modelRefresh);
  static int Compute(
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArguments const * const modelComputeArguments);
  static int ComputeArgumentsCreate(
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);
  static int ComputeArgumentsDestroy(
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

 private:
  LennardJones612Implementation* implementation_;
};

#endif  // LENNARD_JONES_612_HPP_
