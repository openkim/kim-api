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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_LOG_IMPLEMENTATION_HPP_
#define KIM_LOG_IMPLEMENTATION_HPP_

#include <stack>
#include <string>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"  // IWYU pragma: export
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

namespace KIM
{
// Forward declarations

class LogImplementation
{
 public:
  static int Create(LogImplementation ** const logImplementation);
  static void Destroy(LogImplementation ** const logImplementation);

  static void PushDefaultVerbosity(LogVerbosity const logVerbosity);
  static void PopDefaultVerbosity();

  static void PushDefaultPrintFunction(LanguageName const languageName,
                                       Function * const fptr);
  static void PopDefaultPrintFunction();

  std::string const & GetID() const;
  void SetID(std::string const & id);

  void PushVerbosity(LogVerbosity const logVerbosity);
  void PopVerbosity();

  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;

 private:
  // do not allow copy constructor or operator=
  LogImplementation(LogImplementation const &);
  void operator=(LogImplementation const &);

  LogImplementation();
  ~LogImplementation();

  static std::string EntryString(std::string const & logVerbosity,
                                 std::string const & date,
                                 int const sequence,
                                 std::string const & idString,
                                 std::string const & message,
                                 int const lineNumberString,
                                 std::string const & fileName);

  std::string GetTimeStamp() const;

  std::string idString_;
  std::stack<LogVerbosity> verbosity_;

  LanguageName printFunctionLanguageName_;
  Function * printFunctionPointer_;

  mutable std::string latestTimeStamp_;
  mutable unsigned sequence_;

};  // class LogImplementation
}  // namespace KIM
#endif  // KIM_LOG_IMPLEMENTATION_HPP_
