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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_COMPUTE_ARGUMENTS_HPP_
#define KIM_COMPUTE_ARGUMENTS_HPP_

#include <string>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

namespace KIM
{
// Forward declarations
class LogVerbosity;
class LanguageName;
class ComputeArgumentName;
class ComputeCallbackName;
class SupportStatus;
class ModelImplementation;
class ComputeArgumentsImplementation;


class ComputeArguments
{
 public:
  int GetArgumentSupportStatus(ComputeArgumentName const computeArgumentName,
                               SupportStatus * const supportStatus) const;
  int GetCallbackSupportStatus(ComputeCallbackName const computeCallbackName,
                               SupportStatus * const supportStatus) const;


  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int const * const ptr);
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int * const ptr);
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double const * const ptr);
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double * const ptr);

  int SetCallbackPointer(ComputeCallbackName const computeCallbackName,
                         LanguageName const languageName,
                         func * const fptr,
                         void * const dataObject);

  void AreAllRequiredArgumentsAndCallbacksPresent(int * const result) const;

  void SetSimulatorBufferPointer(void * const ptr);
  void GetSimulatorBufferPointer(void ** const ptr) const;

  std::string const & String() const;

  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();

  friend class ModelImplementation;
 private:
  // do not allow copy constructor or operator=
  ComputeArguments(ComputeArguments const &);
  void operator=(ComputeArguments const &);

  ComputeArguments();
  ~ComputeArguments();

  ComputeArgumentsImplementation * pimpl;
};  // class ComputeArguments
}  // namespace KIM
#endif  // KIM_COMPUTE_ARGUMENTS_HPP_
