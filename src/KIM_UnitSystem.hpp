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
#define KIM_UNIT_SYSTEM_HPP_

#include <string>

namespace KIM
{

class LengthUnit
{
  int lengthUnitID;
 public:
  LengthUnit();
  LengthUnit(int const id);
  bool operator==(LengthUnit const & rhs) const;
  bool operator!=(LengthUnit const & rhs) const;
};

class EnergyUnit
{
  int energyUnitID;
 public:
  EnergyUnit();
  EnergyUnit(int const id);
  bool operator==(EnergyUnit const & rhs) const;
  bool operator!=(EnergyUnit const & rhs) const;
};

class ChargeUnit
{
  int chargeUnitID;
 public:
  ChargeUnit();
  ChargeUnit(int const id);
  bool operator==(ChargeUnit const & rhs) const;
  bool operator!=(ChargeUnit const & rhs) const;
};

class TemperatureUnit
{
  int temperatureUnitID;
 public:
  TemperatureUnit();
  TemperatureUnit(int const id);
  bool operator==(TemperatureUnit const & rhs) const;
  bool operator!=(TemperatureUnit const & rhs) const;
};

class TimeUnit
{
  int timeUnitID;
 public:
  TimeUnit();
  TimeUnit(int const id);
  bool operator==(TimeUnit const & rhs) const;
  bool operator!=(TimeUnit const & rhs) const;
};

namespace UNITS
{
extern LengthUnit const A;
extern LengthUnit const Bohr;
extern LengthUnit const cm;
extern LengthUnit const m;
extern LengthUnit const nm;

extern EnergyUnit const amu_A2_per_ps2;
extern EnergyUnit const erg;
extern EnergyUnit const eV;
extern EnergyUnit const Hartree;
extern EnergyUnit const J;
extern EnergyUnit const kcal_mol;

extern ChargeUnit const C;
extern ChargeUnit const e;
extern ChargeUnit const statC;

extern TemperatureUnit const K;

extern TimeUnit const fs;
extern TimeUnit const ps;
extern TimeUnit const ns;
extern TimeUnit const s;
}  // namespace UNITS
}  // namespace KIM
#endif  // KIM_UNIT_SYSTEM_HPP_
