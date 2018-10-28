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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2.0.0-beta.2 package.
//


#ifndef KIM_LOG_VERBOSITY_HPP_
#define KIM_LOG_VERBOSITY_HPP_

#include <string>

#ifndef KIM_LOG_DEFINES_INC_
#include "KIM_LOG_DEFINES.inc"
#endif

namespace KIM
{
class LogVerbosity
{
 public:
  int logVerbosityID;

  LogVerbosity();
  LogVerbosity(int const id);
  LogVerbosity(std::string const & str);
  bool operator<(LogVerbosity const & rhs) const;
  bool operator>(LogVerbosity const & rhs) const;
  bool operator<=(LogVerbosity const & rhs) const;
  bool operator>=(LogVerbosity const & rhs) const;
  bool operator==(LogVerbosity const & rhs) const;
  bool operator!=(LogVerbosity const & rhs) const;
  std::string const & String() const;
};  // class LogVerbosity

namespace LOG_VERBOSITY
{
extern LogVerbosity const silent;
extern LogVerbosity const fatal;
extern LogVerbosity const error;
extern LogVerbosity const warning;
extern LogVerbosity const information;
extern LogVerbosity const debug;

void GetNumberOfLogVerbosities(int * const numberOfLogVerbosities);
int GetLogVerbosity(int const index, LogVerbosity * const logVerbosity);

struct Comparator
{
  bool operator()(LogVerbosity const & a, LogVerbosity const & b) const
  {
    return a.logVerbosityID < b.logVerbosityID;
  }
};  // struct Comparator
}  // namespace LOG_VERBOSITY
}  // namespace KIM

#endif  // KIM_LOG_VERBOSITY_HPP_
