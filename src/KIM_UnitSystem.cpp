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

std::string LengthUnit::string() const
{
  if (*this == UNITS::A) return "A";
  else if (*this == UNITS::Bohr) return "Bohr";
  else if (*this == UNITS::cm) return "cm";
  else if (*this == UNITS::m) return "m";
  else if (*this == UNITS::nm) return "nm";
  else return "unknown";
}

EnergyUnit::EnergyUnit() : energyUnitID(0){}
EnergyUnit::EnergyUnit(int const id) : energyUnitID(id){}
bool EnergyUnit::operator==(EnergyUnit const & rhs) const
{return energyUnitID==rhs.energyUnitID;}
bool EnergyUnit::operator!=(EnergyUnit const & rhs) const
{return energyUnitID!=rhs.energyUnitID;}

std::string EnergyUnit::string() const
{
  if (*this == UNITS::amu_A2_per_ps2) return "amu_A2_per_ps2";
  else if (*this == UNITS::erg) return "erg";
  else if (*this == UNITS::eV) return "eV";
  else if (*this == UNITS::Hartree) return "Hartree";
  else if (*this == UNITS::J) return "J";
  else if (*this == UNITS::kcal_mol) return "kcal_mol";
  else return "unknown";
}

ChargeUnit::ChargeUnit() : chargeUnitID(0){}
ChargeUnit::ChargeUnit(int const id) : chargeUnitID(id){}
bool ChargeUnit::operator==(ChargeUnit const & rhs) const
{return chargeUnitID==rhs.chargeUnitID;}
bool ChargeUnit::operator!=(ChargeUnit const & rhs) const
{return chargeUnitID!=rhs.chargeUnitID;}

std::string ChargeUnit::string() const
{
  if (*this == UNITS::C) return "C";
  else if (*this == UNITS::e) return "e";
  else if (*this == UNITS::statC) return "statC";
  else return "unknown";
}

TemperatureUnit::TemperatureUnit() : temperatureUnitID(0){}
TemperatureUnit::TemperatureUnit(int const id) : temperatureUnitID(id){}
bool TemperatureUnit::operator==(TemperatureUnit const & rhs) const
{return temperatureUnitID==rhs.temperatureUnitID;}
bool TemperatureUnit::operator!=(TemperatureUnit const & rhs) const
{return temperatureUnitID!=rhs.temperatureUnitID;}

std::string TemperatureUnit::string() const
{
  if (*this == UNITS::K) return "K";
  else return "unknown";
}

TimeUnit::TimeUnit() : timeUnitID(0){}
TimeUnit::TimeUnit(int const id) : timeUnitID(id){}
bool TimeUnit::operator==(TimeUnit const & rhs) const
{return timeUnitID==rhs.timeUnitID;}
bool TimeUnit::operator!=(TimeUnit const & rhs) const
{return timeUnitID!=rhs.timeUnitID;}

std::string TimeUnit::string() const
{
  if (*this == UNITS::fs) return "fs";
  else if (*this == UNITS::ps) return "ps";
  else if (*this == UNITS::ns) return "ns";
  else if (*this == UNITS::s) return "s";
  else return "unknown";
}


namespace UNITS
{
LengthUnit const A(0);
LengthUnit const Bohr(1);
LengthUnit const cm(2);
LengthUnit const m(3);
LengthUnit const nm(4);

EnergyUnit const amu_A2_per_ps2(0);
EnergyUnit const erg(1);
EnergyUnit const eV(2);
EnergyUnit const Hartree(3);
EnergyUnit const J(4);
EnergyUnit const kcal_mol(5);

ChargeUnit const C(0);
ChargeUnit const e(1);
ChargeUnit const statC(2);

TemperatureUnit const K(0);

TimeUnit const fs(0);
TimeUnit const ps(1);
TimeUnit const ns(2);
TimeUnit const s(3);
}  // namespace UNITS
}  // namespace KIM
