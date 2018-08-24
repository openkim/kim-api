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

#ifndef KIM_ENERGY_UNIT_HPP_
#include "KIM_EnergyUnit.hpp"
#endif
extern "C"
{
#ifndef KIM_ENERGY_UNIT_H_
#include "KIM_EnergyUnit.h"
#endif
}


namespace
{
KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  KIM::EnergyUnit const * const energyUnitCpp
      = reinterpret_cast <KIM::EnergyUnit const *>(&energyUnit);
  return *energyUnitCpp;
}

KIM_EnergyUnit makeEnergyUnitC(KIM::EnergyUnit const energyUnit)
{
  KIM_EnergyUnit const * const energyUnitC
      = reinterpret_cast <KIM_EnergyUnit const *>(&energyUnit);
  return *energyUnitC;
}
}  // namespace

extern "C"
{
KIM_EnergyUnit KIM_EnergyUnit_FromString(char const * const str)
{
  return makeEnergyUnitC(KIM::EnergyUnit(std::string(str)));
}

int KIM_EnergyUnit_Equal(KIM_EnergyUnit const left, KIM_EnergyUnit const right)
{
  return (left.energyUnitID == right.energyUnitID);
}

int KIM_EnergyUnit_NotEqual(KIM_EnergyUnit const left,
                            KIM_EnergyUnit const right)
{
  return (!KIM_EnergyUnit_Equal(left, right));
}

char const * KIM_EnergyUnit_String(KIM_EnergyUnit const energyUnit)
{
  return makeEnergyUnitCpp(energyUnit).String().c_str();
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
