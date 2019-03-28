//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 3.0 (the "License").
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
// Release: This file is part of the kim-api-2.0.2 package.
//


#ifndef KIM_COMPUTE_ARGUMENTS_HPP_
#define KIM_COMPUTE_ARGUMENTS_HPP_

#include <string>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"
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


/// \brief Provides the primary interface to a %KIM API ComputeArguments object
/// and is meant to be used by simulators.
///
/// \sa KIM_ComputeArguments,
/// kim_compute_arguments_module::kim_compute_arguments_handle_type
///
/// \since 2.0
class ComputeArguments
{
 public:
  /// \brief Get the SupportStatus of a ComputeArgumentName.
  ///
  /// \param[in] computeArgumentName The ComputeArgumentName of interest.
  /// \param[out] supportStatus The Model's corresponding SupportStatus.
  ///
  /// \return \c true if \c computeArgumentName is unknown.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ComputeArguments_GetArgumentSupportStatus,
  /// kim_compute_arguments_module::kim_get_argument_support_status
  ///
  /// \since 2.0
  int GetArgumentSupportStatus(ComputeArgumentName const computeArgumentName,
                               SupportStatus * const supportStatus) const;

  /// \brief Get the SupportStatus of a ComputeCallbackName.
  ///
  /// \param[in] computeCallbackName The ComputeCallbackName of interest.
  /// \param[out] supportStatus The Model's corresponding SupportStatus.
  ///
  /// \return \c true if \c computeCallbackName is unknown.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ComputeArguments_GetCallbackSupportStatus,
  /// kim_compute_arguments_module::kim_get_callback_support_status
  ///
  /// \since 2.0
  int GetCallbackSupportStatus(ComputeCallbackName const computeCallbackName,
                               SupportStatus * const supportStatus) const;

  /// \brief Set the data pointer for a ComputeArgumentName.
  ///
  /// \todo Add more detailed description what it means to provide a NULL or
  /// non-NULL value for various SupportStatus'es.
  ///
  /// \param[in] computeArgumentName The ComputeArgumentName of interest.
  /// \param[in] ptr The data pointer.
  ///
  /// \return \c true if \c computeArgumentName is unknown.
  /// \return \c true if `ptr != NULL` and `computeArgumentName ==
  ///         SUPPORT_STATUS::notSupported`.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ComputeArguments_SetArgumentPointerInteger,
  ///     KIM_ComputeArguments_SetArgumentPointerDouble,
  ///     kim_compute_arguments_module::kim_set_argument_pointer
  ///
  /// \since 2.0
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int const * const ptr);

  /// \overload
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int * const ptr);

  /// \overload
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double const * const ptr);

  /// \overload
  int SetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double * const ptr);

  /// \brief Set the function pointer for a ComputeCallbackName.
  ///
  /// \todo Add more detailed description what it means to provide a NULL or
  /// non-NULL value for various SupportStatus'es.  Also, describe dataObject.
  ///
  /// \param[in] computeCallbackName The ComputeCallbackName of interest.
  /// \param[in] languageName The LanguageName of the callback.
  /// \param[in] fptr The function pointer.
  /// \param[in] dataObject The data pointer associated with the callback.
  ///
  /// \return \c true if \c computeCallbackName is unknown.
  /// \return \c true if `fptr != NULL` and `computeCallbackName ==
  ///         SUPPORT_STATUS::notSupported`.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ComputeArguments_SetCallbackPointer,
  /// kim_compute_arguments_module::kim_set_callback_pointer
  ///
  /// \since 2.0
  int SetCallbackPointer(ComputeCallbackName const computeCallbackName,
                         LanguageName const languageName,
                         Function * const fptr,
                         void * const dataObject);

  /// \brief Determine if non-NULL pointers have been set for all
  /// ComputeArgumentName's and ComputeCallbackName's with SupportStatus
  /// values of SUPPORT_STATUS::requiredByAPI or SUPPORT_STATUS::required.
  ///
  /// \param[out] result Is \c true if all such arguments and callbacks have
  ///             non-NULL pointers, and is \c false otherwise.
  ///
  /// \sa KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent,
  /// kim_compute_arguments_module::kim_are_all_required_present
  ///
  /// \since 2.0
  void AreAllRequiredArgumentsAndCallbacksPresent(int * const result) const;

  /// \brief Set the \ref cache_buffer_pointers "Simulator's buffer pointer"
  /// within the ComputeArguments object.
  ///
  /// The simulator buffer pointer may be used by the simulator to associate
  /// a memory buffer with the ComputeArguments object.
  ///
  /// \param[in] ptr The simulator buffer data pointer.
  ///
  /// \sa KIM_ComputeArguments_SetSimulatorBufferPointer,
  /// kim_compute_arguments_module::kim_set_simulator_buffer_pointer
  ///
  /// \since 2.0
  void SetSimulatorBufferPointer(void * const ptr);


  /// \brief Get the \ref cache_buffer_pointers "Simulator's buffer pointer"
  /// from the ComputeArguments object.
  ///
  /// \param[out] ptr The simulator buffer data pointer.
  ///
  /// \note `ptr == NULL` if the simulator has not previously called
  ///       ComputeArguments::SetSimulatorBufferPointer.
  ///
  /// \sa KIM_ComputeArguments_GetSimulatorBufferPointer,
  /// kim_compute_arguments_module::kim_get_simulator_buffer_pointer
  ///
  /// \since 2.0
  void GetSimulatorBufferPointer(void ** const ptr) const;

  /// \brief Get a string representing the internal state of the
  /// ComputeArguments object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_ComputeArguments_ToString,
  /// kim_compute_arguments_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

  /// \brief Set the identity of the Log object associated with the
  /// ComputeArguments object.
  ///
  /// \param[in] logID String identifying the ComputeArguments object's Log
  /// object.
  ///
  /// \sa KIM_ComputeArguments_SetLogID,
  /// kim_compute_arguments_module::kim_set_log_id
  ///
  /// \since 2.0
  void SetLogID(std::string const & logID);

  /// \brief Push a new LogVerbosity onto the ComputeArguments object's Log
  /// object verbosity stack.
  ///
  /// \param[in] logVerbosity A LogVerbosity value.
  ///
  /// \sa KIM_ComputeArguments_PushLogVerbosity,
  /// kim_compute_arguments_module::kim_push_log_verbosity
  ///
  /// \since 2.0
  void PushLogVerbosity(LogVerbosity const logVerbosity);

  /// \brief Pop a LogVerbosity from the ComputeArguments object's Log object
  /// verbosity stack.
  ///
  /// \sa KIM_ComputeArguments_PopLogVerbosity,
  /// kim_compute_arguments_module::kim_pop_log_verbosity
  ///
  /// \since 2.0
  void PopLogVerbosity();

  /// \brief Allows Model objects to directly access private member data of a
  /// ComputeArguments object.
  ///
  /// \note This has no practical effect on \e users of the %KIM API.
  ///
  /// \since 2.0
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
