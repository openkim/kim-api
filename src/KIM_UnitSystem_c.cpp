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

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif

extern "C"
{
#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif
}

namespace
{
KIM::LengthUnit const makeLengthUnitCpp(KIM_LengthUnit const lengthUnit)
{
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}

KIM::EnergyUnit const makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  return KIM::EnergyUnit(energyUnit.energyUnitID);
}

KIM::ChargeUnit const makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  return KIM::ChargeUnit(chargeUnit.chargeUnitID);
}

KIM::TemperatureUnit const makeTemperatureUnitCpp(
    KIM_TemperatureUnit const temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}

KIM::TimeUnit const makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
}
}  // namespace

extern "C"
{
KIM_LengthUnit const KIM_UNITS_A = {0};
KIM_LengthUnit const KIM_UNITS_Bohr = {1};
KIM_LengthUnit const KIM_UNITS_cm = {2};
KIM_LengthUnit const KIM_UNITS_m = {3};
KIM_LengthUnit const KIM_UNITS_nm = {4};

char const * const KIM_LengthUnitString(KIM_LengthUnit const lengthUnit)
{
  return (makeLengthUnitCpp(lengthUnit)).string().c_str();
}

KIM_EnergyUnit const KIM_UNITS_amu_A2_per_ps2 = {0};
KIM_EnergyUnit const KIM_UNITS_erg = {1};
KIM_EnergyUnit const KIM_UNITS_eV = {2};
KIM_EnergyUnit const KIM_UNITS_Hartree = {3};
KIM_EnergyUnit const KIM_UNITS_J = {4};
KIM_EnergyUnit const KIM_UNITS_kcal_mol = {5};

char const * const KIM_EnergyUnitString(KIM_EnergyUnit const energyUnit)
{
  return (makeEnergyUnitCpp(energyUnit)).string().c_str();
}

KIM_ChargeUnit const KIM_UNITS_C = {0};
KIM_ChargeUnit const KIM_UNITS_e = {1};
KIM_ChargeUnit const KIM_UNITS_statC = {2};

char const * const KIM_ChargeUnitString(KIM_ChargeUnit const chargeUnit)
{
  return (makeChargeUnitCpp(chargeUnit)).string().c_str();
}

KIM_TemperatureUnit const KIM_UNITS_K = {0};

char const * const KIM_TemperatureUnitString(
    KIM_TemperatureUnit const temperatureUnit)
{
  return (makeTemperatureUnitCpp(temperatureUnit)).string().c_str();
}

KIM_TimeUnit const KIM_UNITS_fs = {0};
KIM_TimeUnit const KIM_UNITS_ps = {1};
KIM_TimeUnit const KIM_UNITS_ns = {2};
KIM_TimeUnit const KIM_UNITS_s = {3};

char const * const KIM_TimeUnitString(KIM_TimeUnit const tiemUnit)
{
  return (makeTimeUnitCpp(tiemUnit)).string().c_str();
}

}  // extern "C"
