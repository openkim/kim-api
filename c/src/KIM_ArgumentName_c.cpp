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
extern "C"
{
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif
extern "C"
{
#ifndef KIM_ARGUMENT_NAME_H_
#include "KIM_ArgumentName.h"
#endif
}  // extern "C"


namespace
{
KIM::ArgumentName makeArgumentNameCpp(KIM_ArgumentName const argumentName)
{
  KIM::ArgumentName const * const argumentNameCpp
      = reinterpret_cast <KIM::ArgumentName const * const>(&argumentName);
  return *argumentNameCpp;
}

KIM_ArgumentName makeArgumentNameC(KIM::ArgumentName const argumentName)
{
  KIM_ArgumentName const * const argumentNameC
      = reinterpret_cast <KIM_ArgumentName const * const>(&argumentName);
  return *argumentNameC;
}
}  // namespace

extern "C"
{
KIM_ArgumentName KIM_ArgumentNameFromString(char const * const str)
{
  return makeArgumentNameC(KIM::ArgumentName(std::string(str)));
}

int KIM_ArgumentNameEqual(KIM_ArgumentName const left,
                          KIM_ArgumentName const right)
{
  return (left.argumentNameID == right.argumentNameID);
}

int KIM_ArgumentNameNotEqual(KIM_ArgumentName const left,
                             KIM_ArgumentName const right)
{
  return (!KIM_ArgumentNameEqual(left, right));
}

char const * const KIM_ArgumentNameString(KIM_ArgumentName argumentName)
{
  return (makeArgumentNameCpp(argumentName)).String().c_str();
}

// Order doesn't matter as long as all values are unique
KIM_ArgumentName const KIM_ARGUMENT_NAME_numberOfParticles = {0};
KIM_ArgumentName const KIM_ARGUMENT_NAME_particleSpeciesCodes = {1};
KIM_ArgumentName const KIM_ARGUMENT_NAME_particleContributing = {2};
KIM_ArgumentName const KIM_ARGUMENT_NAME_coordinates = {3};
KIM_ArgumentName const KIM_ARGUMENT_NAME_partialEnergy = {4};
KIM_ArgumentName const KIM_ARGUMENT_NAME_partialForces = {5};
KIM_ArgumentName const KIM_ARGUMENT_NAME_partialParticleEnergy = {6};
KIM_ArgumentName const KIM_ARGUMENT_NAME_partialVirial = {7};
KIM_ArgumentName const KIM_ARGUMENT_NAME_partialParticleVirial = {8};
KIM_ArgumentName const KIM_ARGUMENT_NAME_partialHessian = {9};

void KIM_ARGUMENT_NAME_GetNumberOfArguments(int * const numberOfArguments)
{
  KIM::ARGUMENT_NAME::GetNumberOfArguments(numberOfArguments);
}

int KIM_ARGUMENT_NAME_GetArgumentName(int const index,
                                      KIM_ArgumentName * const argumentName)
{
  KIM::ArgumentName argumentNameCpp;
  int err = KIM::ARGUMENT_NAME::GetArgumentName(index, &argumentNameCpp);
  if (err) return err;
  KIM_ArgumentName * argumentNameC
      = reinterpret_cast<KIM_ArgumentName *>(&argumentNameCpp);
  *argumentName = *argumentNameC;
  return false;
}

int KIM_ARGUMENT_NAME_GetArgumentDataType(
    KIM_ArgumentName const argumentName,
    KIM_DataType * const dataType)
{
  KIM::ArgumentName const * const argumentNameCpp
      = reinterpret_cast<KIM::ArgumentName const *>(&argumentName);
  KIM::DataType dataTypeCpp;
  int err = KIM::ARGUMENT_NAME::
      GetArgumentDataType(*argumentNameCpp, &dataTypeCpp);
  if (err) return err;
  KIM_DataType * dataTypeC = reinterpret_cast<KIM_DataType *>(&dataTypeCpp);

  *dataType = *dataTypeC;
  return false;
}
}  // extern "C"
