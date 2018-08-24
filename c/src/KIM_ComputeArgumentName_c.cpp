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
      = reinterpret_cast <KIM::ComputeArgumentName const *>
      (&computeArgumentName);
  return *computeArgumentNameCpp;
}

KIM_ComputeArgumentName makeComputeArgumentNameC(
    KIM::ComputeArgumentName const computeArgumentName)
{
  KIM_ComputeArgumentName const * const computeArgumentNameC
      = reinterpret_cast <KIM_ComputeArgumentName const *>
      (&computeArgumentName);
  return *computeArgumentNameC;
}

KIM_DataType makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType const * const dataTypeC
      = reinterpret_cast<KIM_DataType const *>(&dataType);
  return *dataTypeC;
}
}  // namespace

extern "C"
{
KIM_ComputeArgumentName KIM_ComputeArgumentName_FromString(
    char const * const str)
{
  return makeComputeArgumentNameC(KIM::ComputeArgumentName(std::string(str)));
}

int KIM_ComputeArgumentName_Equal(KIM_ComputeArgumentName const left,
                                  KIM_ComputeArgumentName const right)
{
  return (left.computeArgumentNameID == right.computeArgumentNameID);
}

int KIM_ComputeArgumentName_NotEqual(KIM_ComputeArgumentName const left,
                                     KIM_ComputeArgumentName const right)
{
  return (!KIM_ComputeArgumentName_Equal(left, right));
}

char const * KIM_ComputeArgumentName_String(
    KIM_ComputeArgumentName computeArgumentName)
{
  return makeComputeArgumentNameCpp(computeArgumentName).String().c_str();
}

// Order doesn't matter as long as all values are unique
#include "KIM_ComputeArgumentName.inc"
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles
= {ID_numberOfParticles};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes
= {ID_particleSpeciesCodes};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleContributing
= {ID_particleContributing};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates
= {ID_coordinates};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialEnergy
= {ID_partialEnergy};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialForces
= {ID_partialForces};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy
= {ID_partialParticleEnergy};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialVirial
= {ID_partialVirial};
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial
= {ID_partialParticleVirial};

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
  int error = KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName(
      index, &computeArgumentNameCpp);
  if (error) return error;
  *computeArgumentName = makeComputeArgumentNameC(computeArgumentNameCpp);
  return false;
}

int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType(
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_DataType * const dataType)
{
  KIM::ComputeArgumentName computeArgumentNameCpp
      = makeComputeArgumentNameCpp(computeArgumentName);
  KIM::DataType dataTypeCpp;
  int error = KIM::COMPUTE_ARGUMENT_NAME::
      GetComputeArgumentDataType(computeArgumentNameCpp, &dataTypeCpp);
  if (error) return error;
  *dataType = makeDataTypeC(dataTypeCpp);
  return false;
}

}  // extern "C"
