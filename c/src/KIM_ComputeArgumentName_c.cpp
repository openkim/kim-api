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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
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

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif
extern "C"
{
#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_ComputeArgumentName.h"
#endif
}  // extern "C"


namespace
{
KIM::ComputeArgumentName makeComputeArgumentNameCpp(
    KIM_ComputeArgumentName const computeArgumentName)
{
  KIM::ComputeArgumentName const * const computeArgumentNameCpp
      = reinterpret_cast <KIM::ComputeArgumentName const * const>
      (&computeArgumentName);
  return *computeArgumentNameCpp;
}

KIM_ComputeArgumentName makeComputeArgumentNameC(
    KIM::ComputeArgumentName const computeArgumentName)
{
  KIM_ComputeArgumentName const * const computeArgumentNameC
      = reinterpret_cast <KIM_ComputeArgumentName const * const>
      (&computeArgumentName);
  return *computeArgumentNameC;
}
}  // namespace

extern "C"
{
KIM_ComputeArgumentName KIM_ComputeArgumentNameFromString(
    char const * const str)
{
  return makeComputeArgumentNameC(KIM::ComputeArgumentName(std::string(str)));
}

int KIM_ComputeArgumentNameEqual(KIM_ComputeArgumentName const left,
                                 KIM_ComputeArgumentName const right)
{
  return (left.computeArgumentNameID == right.computeArgumentNameID);
}

int KIM_ComputeArgumentNameNotEqual(KIM_ComputeArgumentName const left,
                                    KIM_ComputeArgumentName const right)
{
  return (!KIM_ComputeArgumentNameEqual(left, right));
}

char const * const KIM_ComputeArgumentNameString(
    KIM_ComputeArgumentName computeArgumentName)
{
  return makeComputeArgumentNameCpp(computeArgumentName).String().c_str();
}

// Order doesn't matter as long as all values are unique
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles
= {KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes
= {KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleContributing
= {KIM::COMPUTE_ARGUMENT_NAME::particleContributing.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates
= {KIM::COMPUTE_ARGUMENT_NAME::coordinates.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialEnergy
= {KIM::COMPUTE_ARGUMENT_NAME::partialEnergy.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialForces
= {KIM::COMPUTE_ARGUMENT_NAME::partialForces.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy
= {KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialVirial
= {KIM::COMPUTE_ARGUMENT_NAME::partialVirial.computeArgumentNameID};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial
= {KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial.computeArgumentNameID};

void KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames(
    int * const numberOfComputeArgumentNames)
{
  KIM::COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames(
      numberOfComputeArgumentNames);
}

int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName(
    int const index,
    KIM_ComputeArgumentName * const computeArgumentName)
{
  KIM::ComputeArgumentName computeArgumentNameCpp;
  int err = KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName(
      index, &computeArgumentNameCpp);
  if (err) return err;
  KIM_ComputeArgumentName * computeArgumentNameC
      = reinterpret_cast<KIM_ComputeArgumentName *>(&computeArgumentNameCpp);
  *computeArgumentName = *computeArgumentNameC;
  return false;
}

int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType(
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_DataType * const dataType)
{
  KIM::ComputeArgumentName const * const computeArgumentNameCpp
      = reinterpret_cast<KIM::ComputeArgumentName const *>
      (&computeArgumentName);
  KIM::DataType dataTypeCpp;
  int err = KIM::COMPUTE_ARGUMENT_NAME::
      GetComputeArgumentDataType(*computeArgumentNameCpp, &dataTypeCpp);
  if (err) return err;
  KIM_DataType * dataTypeC = reinterpret_cast<KIM_DataType *>(&dataTypeCpp);

  *dataType = *dataTypeC;
  return false;
}
}  // extern "C"
