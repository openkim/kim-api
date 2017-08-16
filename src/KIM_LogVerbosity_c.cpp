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

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"


namespace
{
KIM::LogVerbosity const makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace

extern "C"
{
int KIM_LogVerbosityEqual(KIM_LogVerbosity const left,
                          KIM_LogVerbosity const right)
{
  return (left.logVerbosityID == right.logVerbosityID);
}

int KIM_LogVerbosityNotEqual(KIM_LogVerbosity const left,
                             KIM_LogVerbosity const right)
{
  return (!KIM_LogVerbosityEqual(left, right));
}

char const * const KIM_LogVerbosityString(KIM_LogVerbosity const logVerbosity)
{
  return (makeLogVerbosityCpp(logVerbosity)).String().c_str();
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

}  // extern "C"
