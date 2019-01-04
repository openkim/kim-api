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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <map>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
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
LogVerbosity::LogVerbosity() : logVerbosityID(0) {}
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

std::string const & LogVerbosity::String() const
{
  LOG_VERBOSITY::StringMap::const_iterator iter
      = LOG_VERBOSITY::logVerbosityToString.find(*this);
  if (iter == LOG_VERBOSITY::logVerbosityToString.end())
    return LOG_VERBOSITY::logVerbosityUnknown;
  else
    return iter->second;
}
}  // namespace KIM
