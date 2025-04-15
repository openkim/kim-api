//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
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
// Release: This file is part of the kim-api-2.4.1 package.
//

#include <map>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LOG_DEFINES_INC_
#include "KIM_LOG_DEFINES.inc"
#endif

namespace KIM
{
// Order is important
namespace LOG_VERBOSITY
{
LogVerbosity const silent(KIM_LOG_VERBOSITY_SILENT_);
LogVerbosity const fatal(KIM_LOG_VERBOSITY_FATAL_);
LogVerbosity const error(KIM_LOG_VERBOSITY_ERROR_);
LogVerbosity const warning(KIM_LOG_VERBOSITY_WARNING_);
LogVerbosity const information(KIM_LOG_VERBOSITY_INFORMATION_);
LogVerbosity const debug(KIM_LOG_VERBOSITY_DEBUG_);

namespace
{
typedef std::map<LogVerbosity const, std::string, LOG_VERBOSITY::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[silent] = "silent";
  m[fatal] = "fatal";
  m[error] = "error";
  m[warning] = "warning";
  m[information] = "information";
  m[debug] = "debug";
  return m;
}

StringMap const logVerbosityToString = GetStringMap();
std::string const logVerbosityUnknown("unknown");
}  // namespace


void GetNumberOfLogVerbosities(int * const numberOfLogVerbosities)
{
  *numberOfLogVerbosities = logVerbosityToString.size();
}

int GetLogVerbosity(int const index, LogVerbosity * const logVerbosity)
{
  int numberOfLogVerbosities;
  GetNumberOfLogVerbosities(&numberOfLogVerbosities);
  if ((index < 0) || (index >= numberOfLogVerbosities)) return true;

  StringMap::const_iterator iter = logVerbosityToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *logVerbosity = iter->first;
  return false;  // no error
}
}  // namespace LOG_VERBOSITY

// implementation of LogVerbosity
LogVerbosity::LogVerbosity() {}
LogVerbosity::LogVerbosity(int const id) : logVerbosityID(id) {}
LogVerbosity::LogVerbosity(std::string const & str)
{
  logVerbosityID = -1;
  for (LOG_VERBOSITY::StringMap::const_iterator iter
       = LOG_VERBOSITY::logVerbosityToString.begin();
       iter != LOG_VERBOSITY::logVerbosityToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      logVerbosityID = (iter->first).logVerbosityID;
      break;
    }
  }
}

bool LogVerbosity::Known() const
{
  int numberOfLogVerbosities;
  LOG_VERBOSITY::GetNumberOfLogVerbosities(&numberOfLogVerbosities);

  for (int i = 0; i < numberOfLogVerbosities; ++i)
  {
    LogVerbosity lv;
    LOG_VERBOSITY::GetLogVerbosity(i, &lv);

    if (*this == lv) { return true; }
  }

  return false;
}

bool LogVerbosity::operator<(LogVerbosity const & rhs) const
{
  return logVerbosityID < rhs.logVerbosityID;
}
bool LogVerbosity::operator>(LogVerbosity const & rhs) const
{
  return logVerbosityID > rhs.logVerbosityID;
}
bool LogVerbosity::operator<=(LogVerbosity const & rhs) const
{
  return logVerbosityID <= rhs.logVerbosityID;
}
bool LogVerbosity::operator>=(LogVerbosity const & rhs) const
{
  return logVerbosityID >= rhs.logVerbosityID;
}
bool LogVerbosity::operator==(LogVerbosity const & rhs) const
{
  return logVerbosityID == rhs.logVerbosityID;
}
bool LogVerbosity::operator!=(LogVerbosity const & rhs) const
{
  return logVerbosityID != rhs.logVerbosityID;
}

std::string const & LogVerbosity::ToString() const
{
  LOG_VERBOSITY::StringMap::const_iterator iter
      = LOG_VERBOSITY::logVerbosityToString.find(*this);
  if (iter == LOG_VERBOSITY::logVerbosityToString.end())
    return LOG_VERBOSITY::logVerbosityUnknown;
  else
    return iter->second;
}
}  // namespace KIM
