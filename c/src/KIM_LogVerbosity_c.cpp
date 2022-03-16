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


#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"


namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  KIM::LogVerbosity const * const logVerbosityCpp
      = reinterpret_cast<KIM::LogVerbosity const *>(&logVerbosity);
  return *logVerbosityCpp;
}

KIM_LogVerbosity makeLogVerbosityC(KIM::LogVerbosity const logVerbosity)
{
  KIM_LogVerbosity const * const logVerbosityC
      = reinterpret_cast<KIM_LogVerbosity const *>(&logVerbosity);
  return *logVerbosityC;
}
}  // namespace

extern "C" {
KIM_LogVerbosity KIM_LogVerbosity_FromString(char const * const str)
{
  return makeLogVerbosityC(KIM::LogVerbosity(std::string(str)));
}

int KIM_LogVerbosity_Known(KIM_LogVerbosity const logVerbosity)
{
  return makeLogVerbosityCpp(logVerbosity).Known();
}

int KIM_LogVerbosity_LessThan(KIM_LogVerbosity const lhs,
                              KIM_LogVerbosity const rhs)
{
  return (lhs.logVerbosityID < rhs.logVerbosityID);
}

int KIM_LogVerbosity_GreaterThan(KIM_LogVerbosity const lhs,
                                 KIM_LogVerbosity const rhs)
{
  return (lhs.logVerbosityID > rhs.logVerbosityID);
}
int KIM_LogVerbosity_LessThanEqual(KIM_LogVerbosity const lhs,
                                   KIM_LogVerbosity const rhs)
{
  return (lhs.logVerbosityID <= rhs.logVerbosityID);
}
int KIM_LogVerbosity_GreaterThanEqual(KIM_LogVerbosity const lhs,
                                      KIM_LogVerbosity const rhs)
{
  return (lhs.logVerbosityID >= rhs.logVerbosityID);
}

int KIM_LogVerbosity_Equal(KIM_LogVerbosity const lhs,
                           KIM_LogVerbosity const rhs)
{
  return (lhs.logVerbosityID == rhs.logVerbosityID);
}

int KIM_LogVerbosity_NotEqual(KIM_LogVerbosity const lhs,
                              KIM_LogVerbosity const rhs)
{
  return (!KIM_LogVerbosity_Equal(lhs, rhs));
}

char const * KIM_LogVerbosity_ToString(KIM_LogVerbosity const logVerbosity)
{
  return makeLogVerbosityCpp(logVerbosity).ToString().c_str();
}

KIM_LogVerbosity const KIM_LOG_VERBOSITY_silent
    = {KIM::LOG_VERBOSITY::silent.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_fatal
    = {KIM::LOG_VERBOSITY::fatal.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_error
    = {KIM::LOG_VERBOSITY::error.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_warning
    = {KIM::LOG_VERBOSITY::warning.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_information
    = {KIM::LOG_VERBOSITY::information.logVerbosityID};
KIM_LogVerbosity const KIM_LOG_VERBOSITY_debug
    = {KIM::LOG_VERBOSITY::debug.logVerbosityID};

void KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities(
    int * const numberOfLogVerbosities)
{
  KIM::LOG_VERBOSITY::GetNumberOfLogVerbosities(numberOfLogVerbosities);
}

int KIM_LOG_VERBOSITY_GetLogVerbosity(int const index,
                                      KIM_LogVerbosity * const logVerbosity)
{
  KIM::LogVerbosity logVerbosityCpp;
  int error = KIM::LOG_VERBOSITY::GetLogVerbosity(index, &logVerbosityCpp);
  if (error) return error;
  *logVerbosity = makeLogVerbosityC(logVerbosityCpp);
  return false;
}

}  // extern "C"
