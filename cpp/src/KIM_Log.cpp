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


#include <cstddef>

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
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
  *log = NULL;
}

void Log::PushDefaultVerbosity(LogVerbosity const logVerbosity)
{
  LogImplementation::PushDefaultVerbosity(logVerbosity);
}

void Log::PopDefaultVerbosity() { LogImplementation::PopDefaultVerbosity(); }

void Log::PushDefaultPrintFunction(LanguageName const languageName,
                                   Function * const fptr)
{
  LogImplementation::PushDefaultPrintFunction(languageName, fptr);
}

void Log::PopDefaultPrintFunction()
{
  LogImplementation::PopDefaultPrintFunction();
}

std::string const & Log::GetID() const { return pimpl->GetID(); }

void Log::SetID(std::string const & id) { pimpl->SetID(id); }

void Log::PushVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushVerbosity(logVerbosity);
}

void Log::PopVerbosity() { pimpl->PopVerbosity(); }

void Log::LogEntry(LogVerbosity const logVerbosity,
                   std::string const & message,
                   int const lineNumber,
                   std::string const & fileName) const
{
  pimpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void Log::LogEntry(LogVerbosity const logVerbosity,
                   std::stringstream const & message,
                   int const lineNumber,
                   std::string const & fileName) const
{
  pimpl->LogEntry(logVerbosity, message.str(), lineNumber, fileName);
}

Log::Log() : pimpl(NULL) {}

Log::~Log() {}

}  // namespace KIM
