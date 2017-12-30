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

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LOG_IMPLEMENTATION_HPP_
#include "KIM_LogImplementation.hpp"
#endif

namespace KIM
{
int Log::Create(Log ** const log)
{
  *log = new Log();

  return LogImplementation::Create(&((*log)->pimpl));
}

void Log::Destroy(Log ** const log)
{
  LogImplementation::Destroy(&((*log)->pimpl));
  delete *log;
  *log = 0;
}

std::string Log::GetID() const
{
  return pimpl->GetID();
}

void Log::SetID(std::string const & id)
{
  pimpl->SetID(id);
}

void Log::PushVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushVerbosity(logVerbosity);
}

void Log::PopVerbosity()
{
  pimpl->PopVerbosity();
}

void Log::LogEntry(LogVerbosity const logVerbosity, std::string const & message,
                   int const lineNumber, std::string const & fileName) const
{
  pimpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void Log::LogEntry(LogVerbosity const logVerbosity,
                   std::stringstream const & message,
                   int const lineNumber, std::string const & fileName) const
{
  pimpl->LogEntry(logVerbosity, message.str(), lineNumber, fileName);
}

Log::Log() : pimpl(0)
{
}

Log::~Log()
{
}

}  // namespace KIM
