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

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace SUPPORT_STATUS
{
#include "KIM_SupportStatus.inc"
SupportStatus const requiredByAPI(ID_requiredByAPI);
SupportStatus const notSupported(ID_notSupported);
SupportStatus const required(ID_required);
SupportStatus const optional(ID_optional);

namespace
{
typedef std::map<SupportStatus const, std::string, SUPPORT_STATUS::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[requiredByAPI] = "requiredByAPI";
  m[notSupported] = "notSupported";
  m[required] = "required";
  m[optional] = "optional";
  return m;
}

StringMap const supportStatusToString = GetStringMap();
std::string const supportStatusUnknown("unknown");
}  // namespace


void GetNumberOfSupportStatuses(int * const numberOfSupportStatuses)
{
  *numberOfSupportStatuses = supportStatusToString.size();
}

int GetSupportStatus(int const index, SupportStatus * const supportStatus)
{
  int numberOfSupportStatuses;
  GetNumberOfSupportStatuses(&numberOfSupportStatuses);
  if ((index < 0) || (index >= numberOfSupportStatuses)) return true;

  StringMap::const_iterator iter = supportStatusToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *supportStatus = iter->first;
  return false;  // no error
}
}  // namespace SUPPORT_STATUS

// implementation of SupportStatus
SupportStatus::SupportStatus() {}
SupportStatus::SupportStatus(int const id) : supportStatusID(id) {}
SupportStatus::SupportStatus(std::string const & str)
{
  supportStatusID = -1;
  for (SUPPORT_STATUS::StringMap::const_iterator iter
       = SUPPORT_STATUS::supportStatusToString.begin();
       iter != SUPPORT_STATUS::supportStatusToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      supportStatusID = (iter->first).supportStatusID;
      break;
    }
  }
}

bool SupportStatus::Known() const
{
  int numberOfSupportStatuses;
  SUPPORT_STATUS::GetNumberOfSupportStatuses(&numberOfSupportStatuses);

  for (int i = 0; i < numberOfSupportStatuses; ++i)
  {
    SupportStatus supStatus;
    SUPPORT_STATUS::GetSupportStatus(i, &supStatus);

    if (*this == supStatus) { return true; }
  }

  return false;
}

bool SupportStatus::operator==(SupportStatus const & rhs) const
{
  return supportStatusID == rhs.supportStatusID;
}
bool SupportStatus::operator!=(SupportStatus const & rhs) const
{
  return supportStatusID != rhs.supportStatusID;
}

std::string const & SupportStatus::ToString() const
{
  SUPPORT_STATUS::StringMap::const_iterator iter
      = SUPPORT_STATUS::supportStatusToString.find(*this);
  if (iter == SUPPORT_STATUS::supportStatusToString.end())
    return SUPPORT_STATUS::supportStatusUnknown;
  else
    return iter->second;
}
}  // namespace KIM
