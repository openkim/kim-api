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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
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
EnergyUnit const unused(0);
EnergyUnit const amu_A2_per_ps2(1);
EnergyUnit const erg(2);
EnergyUnit const eV(3);
EnergyUnit const Hartree(4);
EnergyUnit const J(5);
EnergyUnit const kcal_mol(6);

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
}  // namespace
extern StringMap const energyUnitToString = GetStringMap();

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
  for  (; i<index; ++i) ++iter;
  *energyUnit = iter->first;
  return false;  // no error
}
}  // namespace ENERGY_UNIT

// implementation of EnergyUnit
EnergyUnit::EnergyUnit() : energyUnitID(0){}
EnergyUnit::EnergyUnit(int const id) : energyUnitID(id){}
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

bool EnergyUnit::operator==(EnergyUnit const & rhs) const
{return energyUnitID==rhs.energyUnitID;}
bool EnergyUnit::operator!=(EnergyUnit const & rhs) const
{return energyUnitID!=rhs.energyUnitID;}

std::string EnergyUnit::String() const
{
  std::string result;
  ENERGY_UNIT::StringMap::const_iterator iter
      = ENERGY_UNIT::energyUnitToString.find(*this);
  if (iter == ENERGY_UNIT::energyUnitToString.end())
    result = "unknown";
  else
    result = iter->second;

  return result;
}
}  // namespace KIM
