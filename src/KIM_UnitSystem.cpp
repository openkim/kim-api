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

#include "old_KIM_API.h"

namespace KIM
{

LengthUnit::LengthUnit() : lengthUnitID(0){}
LengthUnit::LengthUnit(int const id) : lengthUnitID(id){}
bool LengthUnit::operator==(LengthUnit const & rhs) const
{return lengthUnitID==rhs.lengthUnitID;}
bool LengthUnit::operator!=(LengthUnit const & rhs) const
{return lengthUnitID!=rhs.lengthUnitID;}

EnergyUnit::EnergyUnit() : energyUnitID(0){}
EnergyUnit::EnergyUnit(int const id) : energyUnitID(id){}
bool EnergyUnit::operator==(EnergyUnit const & rhs) const
{return energyUnitID==rhs.energyUnitID;}
bool EnergyUnit::operator!=(EnergyUnit const & rhs) const
{return energyUnitID!=rhs.energyUnitID;}

ChargeUnit::ChargeUnit() : chargeUnitID(0){}
ChargeUnit::ChargeUnit(int const id) : chargeUnitID(id){}
bool ChargeUnit::operator==(ChargeUnit const & rhs) const
{return chargeUnitID==rhs.chargeUnitID;}
bool ChargeUnit::operator!=(ChargeUnit const & rhs) const
{return chargeUnitID!=rhs.chargeUnitID;}

TemperatureUnit::TemperatureUnit() : temperatureUnitID(0){}
TemperatureUnit::TemperatureUnit(int const id) : temperatureUnitID(id){}
bool TemperatureUnit::operator==(TemperatureUnit const & rhs) const
{return temperatureUnitID==rhs.temperatureUnitID;}
bool TemperatureUnit::operator!=(TemperatureUnit const & rhs) const
{return temperatureUnitID!=rhs.temperatureUnitID;}

TimeUnit::TimeUnit() : timeUnitID(0){}
TimeUnit::TimeUnit(int const id) : timeUnitID(id){}
bool TimeUnit::operator==(TimeUnit const & rhs) const
{return timeUnitID==rhs.timeUnitID;}
bool TimeUnit::operator!=(TimeUnit const & rhs) const
{return timeUnitID!=rhs.timeUnitID;}

namespace UNITS
{
LengthUnit const A(1);
LengthUnit const Bohr(2);
LengthUnit const cm(3);
LengthUnit const m(4);
LengthUnit const nm(5);

EnergyUnit const amu_A2_per_ps2(1);
EnergyUnit const erg(2);
EnergyUnit const eV(3);
EnergyUnit const Hartree(4);
EnergyUnit const J(5);
EnergyUnit const kcal_mol(6);

ChargeUnit const C(1);
ChargeUnit const e(2);
ChargeUnit const statC(3);

TemperatureUnit const K(1);

TimeUnit const fs(1);
TimeUnit const ps(2);
TimeUnit const ns(3);
TimeUnit const s(4);
}  // namespace UNITS
}  // namespace KIM
