//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
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

//
// Release: This file is part of the kim-api-2.4.1 package.
//


#include <string>

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif
extern "C" {
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif
extern "C" {
#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_ComputeArgumentName.h"
#endif
}  // extern "C"


namespace
{
KIM::ComputeArgumentName
makeComputeArgumentNameCpp(KIM_ComputeArgumentName const computeArgumentName)
{
  KIM::ComputeArgumentName const * const computeArgumentNameCpp
      = reinterpret_cast<KIM::ComputeArgumentName const *>(
          &computeArgumentName);
  return *computeArgumentNameCpp;
}

KIM_ComputeArgumentName
makeComputeArgumentNameC(KIM::ComputeArgumentName const computeArgumentName)
{
  KIM_ComputeArgumentName const * const computeArgumentNameC
      = reinterpret_cast<KIM_ComputeArgumentName const *>(&computeArgumentName);
  return *computeArgumentNameC;
}

KIM_DataType makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType const * const dataTypeC
      = reinterpret_cast<KIM_DataType const *>(&dataType);
  return *dataTypeC;
}
}  // namespace

extern "C" {
KIM_ComputeArgumentName
KIM_ComputeArgumentName_FromString(char const * const str)
{
  return makeComputeArgumentNameC(KIM::ComputeArgumentName(std::string(str)));
}

int KIM_ComputeArgumentName_Known(
    KIM_ComputeArgumentName const computeArgumentName)
{
  return makeComputeArgumentNameCpp(computeArgumentName).Known();
}

int KIM_ComputeArgumentName_Equal(KIM_ComputeArgumentName const lhs,
                                  KIM_ComputeArgumentName const rhs)
{
  return (lhs.computeArgumentNameID == rhs.computeArgumentNameID);
}

int KIM_ComputeArgumentName_NotEqual(KIM_ComputeArgumentName const lhs,
                                     KIM_ComputeArgumentName const rhs)
{
  return (!KIM_ComputeArgumentName_Equal(lhs, rhs));
}

char const *
KIM_ComputeArgumentName_ToString(KIM_ComputeArgumentName computeArgumentName)
{
  return makeComputeArgumentNameCpp(computeArgumentName).ToString().c_str();
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
    int const index, KIM_ComputeArgumentName * const computeArgumentName)
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
  int error = KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentDataType(
      computeArgumentNameCpp, &dataTypeCpp);
  if (error) return error;
  *dataType = makeDataTypeC(dataTypeCpp);
  return false;
}

}  // extern "C"
