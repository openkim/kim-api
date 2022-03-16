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

#include <map>

#ifndef KIM_ENERGY_UNIT_HPP_
#include "KIM_EnergyUnit.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace ENERGY_UNIT
{
#include "KIM_EnergyUnit.inc"
EnergyUnit const unused(ID_unused);
EnergyUnit const amu_A2_per_ps2(ID_amu_A2_per_ps2);
EnergyUnit const erg(ID_erg);
EnergyUnit const eV(ID_eV);
EnergyUnit const Hartree(ID_Hartree);
EnergyUnit const J(ID_J);
EnergyUnit const kcal_mol(ID_kcal_mol);

namespace
{
typedef std::map<EnergyUnit const, std::string, ENERGY_UNIT::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[unused] = "unused";
  m[amu_A2_per_ps2] = "amu_A2_per_ps2";
  m[erg] = "erg";
  m[eV] = "eV";
  m[Hartree] = "Hartree";
  m[J] = "J";
  m[kcal_mol] = "kcal_mol";
  return m;
}

StringMap const energyUnitToString = GetStringMap();
std::string const energyUnitUnknown("unknown");
}  // namespace


void GetNumberOfEnergyUnits(int * const numberOfEnergyUnits)
{
  *numberOfEnergyUnits = energyUnitToString.size();
}

int GetEnergyUnit(int const index, EnergyUnit * const energyUnit)
{
  int numberOfEnergyUnits;
  GetNumberOfEnergyUnits(&numberOfEnergyUnits);
  if ((index < 0) || (index >= numberOfEnergyUnits)) return true;

  StringMap::const_iterator iter = energyUnitToString.begin();
  int i = 0;
  for (; i < index; ++i) ++iter;
  *energyUnit = iter->first;
  return false;  // no error
}
}  // namespace ENERGY_UNIT

// implementation of EnergyUnit
EnergyUnit::EnergyUnit() {}
EnergyUnit::EnergyUnit(int const id) : energyUnitID(id) {}
EnergyUnit::EnergyUnit(std::string const & str)
{
  energyUnitID = -1;
  for (ENERGY_UNIT::StringMap::const_iterator iter
       = ENERGY_UNIT::energyUnitToString.begin();
       iter != ENERGY_UNIT::energyUnitToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      energyUnitID = (iter->first).energyUnitID;
      break;
    }
  }
}

bool EnergyUnit::Known() const
{
  int numberOfEnergyUnits;
  ENERGY_UNIT::GetNumberOfEnergyUnits(&numberOfEnergyUnits);

  for (int i = 0; i < numberOfEnergyUnits; ++i)
  {
    EnergyUnit eUnit;
    ENERGY_UNIT::GetEnergyUnit(i, &eUnit);

    if (*this == eUnit) { return true; }
  }

  return false;
}

bool EnergyUnit::operator==(EnergyUnit const & rhs) const
{
  return energyUnitID == rhs.energyUnitID;
}
bool EnergyUnit::operator!=(EnergyUnit const & rhs) const
{
  return energyUnitID != rhs.energyUnitID;
}

std::string const & EnergyUnit::ToString() const
{
  ENERGY_UNIT::StringMap::const_iterator iter
      = ENERGY_UNIT::energyUnitToString.find(*this);
  if (iter == ENERGY_UNIT::energyUnitToString.end())
    return ENERGY_UNIT::energyUnitUnknown;
  else
    return iter->second;
}
}  // namespace KIM
