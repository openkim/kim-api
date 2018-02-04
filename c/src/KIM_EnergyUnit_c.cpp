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
KIM::EnergyUnit const makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  KIM::EnergyUnit const * const energyUnitCpp
      = reinterpret_cast <KIM::EnergyUnit const * const>(&energyUnit);
  return *energyUnitCpp;
}

KIM_EnergyUnit const makeEnergyUnitC(KIM::EnergyUnit const energyUnit)
{
  KIM_EnergyUnit const * const energyUnitC
      = reinterpret_cast <KIM_EnergyUnit const * const>(&energyUnit);
  return *energyUnitC;
}
}  // namespace

extern "C"
{
KIM_EnergyUnit KIM_EnergyUnitFromString(char const * const str)
{
  return makeEnergyUnitC(KIM::EnergyUnit(std::string(str)));
}

int KIM_EnergyUnitEqual(KIM_EnergyUnit const left, KIM_EnergyUnit const right)
{
  return (left.energyUnitID == right.energyUnitID);
}

int KIM_EnergyUnitNotEqual(KIM_EnergyUnit const left,
                           KIM_EnergyUnit const right)
{
  return (!KIM_EnergyUnitEqual(left, right));
}

char const * const KIM_EnergyUnitString(KIM_EnergyUnit const energyUnit)
{
  static std::string result;
  result = makeEnergyUnitCpp(energyUnit).String();
  return result.c_str();
}

KIM_EnergyUnit const KIM_ENERGY_UNIT_unused
= {KIM::ENERGY_UNIT::unused.energyUnitID};
KIM_EnergyUnit const KIM_ENERGY_UNIT_amu_A2_per_ps2
= {KIM::ENERGY_UNIT::amu_A2_per_ps2.energyUnitID};
KIM_EnergyUnit const KIM_ENERGY_UNIT_erg
= {KIM::ENERGY_UNIT::erg.energyUnitID};
KIM_EnergyUnit const KIM_ENERGY_UNIT_eV
= {KIM::ENERGY_UNIT::eV.energyUnitID};
KIM_EnergyUnit const KIM_ENERGY_UNIT_Hartree
= {KIM::ENERGY_UNIT::Hartree.energyUnitID};
KIM_EnergyUnit const KIM_ENERGY_UNIT_J
= {KIM::ENERGY_UNIT::J.energyUnitID};
KIM_EnergyUnit const KIM_ENERGY_UNIT_kcal_mol
= {KIM::ENERGY_UNIT::kcal_mol.energyUnitID};

}  // extern "C"