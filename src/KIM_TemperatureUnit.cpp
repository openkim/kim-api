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

#ifndef KIM_TEMPERATURE_UNIT_HPP_
#include "KIM_TemperatureUnit.hpp"
#endif

namespace KIM
{

TemperatureUnit::TemperatureUnit() : temperatureUnitID(0){}
TemperatureUnit::TemperatureUnit(int const id) : temperatureUnitID(id){}
bool TemperatureUnit::operator==(TemperatureUnit const & rhs) const
{return temperatureUnitID==rhs.temperatureUnitID;}
bool TemperatureUnit::operator!=(TemperatureUnit const & rhs) const
{return temperatureUnitID!=rhs.temperatureUnitID;}

std::string TemperatureUnit::string() const
{
  if (*this == TEMPERATURE_UNIT::unused) return "unused";
  else if (*this == TEMPERATURE_UNIT::K) return "K";
  else return "unknown";
}

namespace TEMPERATURE_UNIT
{
TemperatureUnit const unused(0);
TemperatureUnit const K(1);
}  // namespace TEMPERATURE_UNIT

}  // namespace KIM
