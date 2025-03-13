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
// Release: This file is part of the kim-api.git repository.
//


#include <string>

#ifndef KIM_ENERGY_UNIT_HPP_
#include "KIM_EnergyUnit.hpp"
#endif
extern "C" {
#ifndef KIM_ENERGY_UNIT_H_
#include "KIM_EnergyUnit.h"
#endif
}


namespace
{
KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  KIM::EnergyUnit const * const energyUnitCpp
      = reinterpret_cast<KIM::EnergyUnit const *>(&energyUnit);
  return *energyUnitCpp;
}

KIM_EnergyUnit makeEnergyUnitC(KIM::EnergyUnit const energyUnit)
{
  KIM_EnergyUnit const * const energyUnitC
      = reinterpret_cast<KIM_EnergyUnit const *>(&energyUnit);
  return *energyUnitC;
}
}  // namespace

extern "C" {
KIM_EnergyUnit KIM_EnergyUnit_FromString(char const * const str)
{
  return makeEnergyUnitC(KIM::EnergyUnit(std::string(str)));
}

int KIM_EnergyUnit_Known(KIM_EnergyUnit const energyUnit)
{
  return makeEnergyUnitCpp(energyUnit).Known();
}

int KIM_EnergyUnit_Equal(KIM_EnergyUnit const lhs, KIM_EnergyUnit const rhs)
{
  return (lhs.energyUnitID == rhs.energyUnitID);
}

int KIM_EnergyUnit_NotEqual(KIM_EnergyUnit const lhs, KIM_EnergyUnit const rhs)
{
  return (!KIM_EnergyUnit_Equal(lhs, rhs));
}

char const * KIM_EnergyUnit_ToString(KIM_EnergyUnit const energyUnit)
{
  return makeEnergyUnitCpp(energyUnit).ToString().c_str();
}

#include "KIM_EnergyUnit.inc"
KIM_EnergyUnit const KIM_ENERGY_UNIT_unused = {ID_unused};
KIM_EnergyUnit const KIM_ENERGY_UNIT_amu_A2_per_ps2 = {ID_amu_A2_per_ps2};
KIM_EnergyUnit const KIM_ENERGY_UNIT_erg = {ID_erg};
KIM_EnergyUnit const KIM_ENERGY_UNIT_eV = {ID_eV};
KIM_EnergyUnit const KIM_ENERGY_UNIT_Hartree = {ID_Hartree};
KIM_EnergyUnit const KIM_ENERGY_UNIT_J = {ID_J};
KIM_EnergyUnit const KIM_ENERGY_UNIT_kcal_mol = {ID_kcal_mol};

void KIM_ENERGY_UNIT_GetNumberOfEnergyUnits(int * const numberOfEnergyUnits)
{
  KIM::ENERGY_UNIT::GetNumberOfEnergyUnits(numberOfEnergyUnits);
}

int KIM_ENERGY_UNIT_GetEnergyUnit(int const index,
                                  KIM_EnergyUnit * const energyUnit)
{
  KIM::EnergyUnit energyUnitCpp;
  int error = KIM::ENERGY_UNIT::GetEnergyUnit(index, &energyUnitCpp);
  if (error) return error;
  *energyUnit = makeEnergyUnitC(energyUnitCpp);
  return false;
}

}  // extern "C"
