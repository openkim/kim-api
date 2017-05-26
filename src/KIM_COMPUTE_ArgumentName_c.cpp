//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common
// Development and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name
// LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner].
// All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

extern "C"
{
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_COMPUTE_ArgumentName.h"
#endif
}  // extern "C"

namespace
{
KIM::COMPUTE::ArgumentName makeArgumentNameCpp(
    KIM_COMPUTE_ArgumentName const argumentName)
{
  KIM::COMPUTE::ArgumentName const * const argumentNameCpp
      = reinterpret_cast
      <KIM::COMPUTE::ArgumentName const * const>(&argumentName);
  return *argumentNameCpp;
}

}  // namespace

extern "C"
{
char const * const KIM_COMPUTE_ArgumentNameString(
    KIM_COMPUTE_ArgumentName argumentName)
{
  return (makeArgumentNameCpp(argumentName)).string().c_str();
}

// Order doesn't matter as long as all values are unique
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles = {0};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfSpecies = {1};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleSpecies = {2};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleContributing = {3};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates = {4};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_energy = {5};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_forces = {6};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleEnergy = {7};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_virial = {8};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleVirial = {9};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_hessian = {10};

void KIM_COMPUTE_ARGUMENT_NAME_get_number_of_arguments(
    int * const numberOfArguments)
{
  KIM::COMPUTE::ARGUMENT_NAME::get_number_of_arguments(numberOfArguments);
}

int KIM_COMPUTE_ARGUMENT_NAME_get_argument(
    int const index,
    KIM_COMPUTE_ArgumentName * const argumentName)
{
  KIM::COMPUTE::ArgumentName argumentNameCpp;
  int err = KIM::COMPUTE::ARGUMENT_NAME::get_argument(index, &argumentNameCpp);
  if (err) return err;
  KIM_COMPUTE_ArgumentName * argumentNameC
      = reinterpret_cast<KIM_COMPUTE_ArgumentName *>(&argumentNameCpp);
  *argumentName = *argumentNameC;
  return false;
}

int KIM_COMPUTE_ARGUMENT_NAME_get_argument_data_type(
    KIM_COMPUTE_ArgumentName const argumentName,
    KIM_DataType * const dataType)
{
  KIM::COMPUTE::ArgumentName const * const argumentNameCpp
      = reinterpret_cast<KIM::COMPUTE::ArgumentName const *>(&argumentName);
  KIM::DataType dataTypeCpp;
  int err = KIM::COMPUTE::ARGUMENT_NAME::
      get_argument_data_type(*argumentNameCpp, &dataTypeCpp);
  if (err) return err;
  KIM_DataType * dataTypeC = reinterpret_cast<KIM_DataType *>(&dataTypeCpp);

  *dataType = *dataTypeC;
  return false;
}
}  // extern "C"
