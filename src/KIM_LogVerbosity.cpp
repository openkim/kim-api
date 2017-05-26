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

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

namespace KIM
{

LogVerbosity::LogVerbosity() : logVerbosityID(0){}
LogVerbosity::LogVerbosity(int const id) : logVerbosityID(id){}

bool LogVerbosity::operator<(LogVerbosity const & rhs) const
{return logVerbosityID < rhs.logVerbosityID;}
bool LogVerbosity::operator>(LogVerbosity const & rhs) const
{return logVerbosityID > rhs.logVerbosityID;}
bool LogVerbosity::operator<=(LogVerbosity const & rhs) const
{return logVerbosityID <= rhs.logVerbosityID;}
bool LogVerbosity::operator>=(LogVerbosity const & rhs) const
{return logVerbosityID >= rhs.logVerbosityID;}
bool LogVerbosity::operator==(LogVerbosity const & rhs) const
{return logVerbosityID == rhs.logVerbosityID;}
bool LogVerbosity::operator!=(LogVerbosity const & rhs) const
{return logVerbosityID != rhs.logVerbosityID;}

std::string LogVerbosity::string() const
{
  if (*this == LOG_VERBOSITY::silent)
    return "silent";
  else if (*this == LOG_VERBOSITY::fatal)
    return "fatal";
  else if (*this == LOG_VERBOSITY::error)
    return "error";
  else if (*this == LOG_VERBOSITY::warning)
    return "warning";
  else if (*this == LOG_VERBOSITY::information)
    return "information";
  else if (*this == LOG_VERBOSITY::debug)
    return "debug";
  else
    return "unknown";
}

// Order is important
namespace LOG_VERBOSITY
{
LogVerbosity const silent(0);
LogVerbosity const fatal(1);
LogVerbosity const error(2);
LogVerbosity const warning(3);
LogVerbosity const information(4);
LogVerbosity const debug(5);
}  // namespace LOG_VERBOSITY

}  // namespace KIM
