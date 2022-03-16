//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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
// Release: This file is part of the kim-api.git repository.
//


#include <string>

#ifndef KIM_TEMPERATURE_UNIT_HPP_
#include "KIM_TemperatureUnit.hpp"
#endif
extern "C" {
#ifndef KIM_TEMPERATURE_UNIT_H_
#include "KIM_TemperatureUnit.h"
#endif
}


namespace
{
KIM::TemperatureUnit
makeTemperatureUnitCpp(KIM_TemperatureUnit const temperatureUnit)
{
  KIM::TemperatureUnit const * const temperatureUnitCpp
      = reinterpret_cast<KIM::TemperatureUnit const *>(&temperatureUnit);
  return *temperatureUnitCpp;
}

KIM_TemperatureUnit
makeTemperatureUnitC(KIM::TemperatureUnit const temperatureUnit)
{
  KIM_TemperatureUnit const * const temperatureUnitC
      = reinterpret_cast<KIM_TemperatureUnit const *>(&temperatureUnit);
  return *temperatureUnitC;
}
}  // namespace

extern "C" {
KIM_TemperatureUnit KIM_TemperatureUnit_FromString(char const * const str)
{
  return makeTemperatureUnitC(KIM::TemperatureUnit(std::string(str)));
}

int KIM_TemperatureUnit_Known(KIM_TemperatureUnit const temperatureUnit)
{
  return makeTemperatureUnitCpp(temperatureUnit).Known();
}

int KIM_TemperatureUnit_Equal(KIM_TemperatureUnit const lhs,
                              KIM_TemperatureUnit const rhs)
{
  return (lhs.temperatureUnitID == rhs.temperatureUnitID);
}

int KIM_TemperatureUnit_NotEqual(KIM_TemperatureUnit const lhs,
                                 KIM_TemperatureUnit const rhs)
{
  return (!KIM_TemperatureUnit_Equal(lhs, rhs));
}

char const *
KIM_TemperatureUnit_ToString(KIM_TemperatureUnit const temperatureUnit)
{
  return makeTemperatureUnitCpp(temperatureUnit).ToString().c_str();
}

#include "KIM_TemperatureUnit.inc"
KIM_TemperatureUnit const KIM_TEMPERATURE_UNIT_unused = {ID_unused};
KIM_TemperatureUnit const KIM_TEMPERATURE_UNIT_K = {ID_K};

void KIM_TEMPERATURE_UNIT_GetNumberOfTemperatureUnits(
    int * const numberOfTemperatureUnits)
{
  KIM::TEMPERATURE_UNIT::GetNumberOfTemperatureUnits(numberOfTemperatureUnits);
}

int KIM_TEMPERATURE_UNIT_GetTemperatureUnit(
    int const index, KIM_TemperatureUnit * const temperatureUnit)
{
  KIM::TemperatureUnit temperatureUnitCpp;
  int error
      = KIM::TEMPERATURE_UNIT::GetTemperatureUnit(index, &temperatureUnitCpp);
  if (error) return error;
  *temperatureUnit = makeTemperatureUnitC(temperatureUnitCpp);
  return false;
}

}  // extern "C"
