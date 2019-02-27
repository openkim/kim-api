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
// Release: This file is part of the kim-api-v2-2.0.1 package.
//


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_HPP_
#define KIM_MODEL_COMPUTE_ARGUMENTS_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class ComputeArgumentName;
class ComputeCallbackName;
class ModelComputeArgumentsImplementation;


/// \brief Provides the interface to a %KIM API ComputeArguments object for use
/// by models within their MODEL_ROUTINE_NAME::Compute routine.
///
/// \sa KIM_ModelComputeArguments,
/// kim_model_compute_arguments_module::kim_model_compute_arguments_handle_type
///
/// \since 2.0
class ModelComputeArguments
{
 public:
  /// \brief Get the neighbor list for a particle of interest corresponding to
  /// a particular neighbor list cutoff distance.
  ///
  /// \param[in]  neighborListIndex Zero-based index corresponding to the
  ///             desired neighbor list cutoff distance.
  /// \param[in]  particleNumber Particle number (using the Model's Numbering,
  ///             as specified in the call to ModelCreate::SetModelNumbering or
  ///             ModelDriverCreate::SetModelNumbering) for the particle of
  ///             interest.
  /// \param[out] numberOfNeighbors Number of neighbor particles in the list.
  /// \param[out] neighborsOfParticle Pointer to array of particle neighbor
  ///             numbers (using the Model's Numbering).
  ///
  /// \return \c true if \c neighborListIndex is invalid.
  /// \return \c true if \c particleNumber is invalid.

  /// \return \c true if the Simulator's COMPUTE_CALLBACK_NAME::GetNeighborList
  ///         routine returns \c true.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ModelComputeArguments_GetNeighborList,
  /// kim_model_compute_arguments_module::kim_get_neighbor_list
  ///
  /// \since 2.0
  int GetNeighborList(int const neighborListIndex,
                      int const particleNumber,
                      int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle) const;

  /// \brief Call the Simulator's COMPUTE_CALLBACK_NAME::ProcessDEDrTerm
  /// routine.
  ///
  /// \todo Add more detailed description of Process mechanism.
  ///
  /// \param[in] de Value of DEDr for particle pair.
  /// \param[in] r Value of particle pair distance, \f$
  ///            \|\mathbf{r}^{(ij)}\|\f$, where \f$\mathbf{r}^{(ij)} \equiv
  ///            \mathbf{r}^{(j)} - \mathbf{r}^{(i)}\f$ (see the \ref theory
  ///            section).
  /// \param[in] dx Value of particle pair relative position vector,
  ///            \f$\mathbf{r}^{(ij)}\f$.
  /// \param[in] i Particle number (using the Model's Numbering, as specified
  ///            in the call to ModelCreate::SetModelNumbering or
  ///            ModelDriverCreate::SetModelNumbering) for first particle in
  ///            pair.
  /// \param[in] j Particle number (using the Model's Numbering) for second
  ///            particle in pair.
  ///
  /// \returns \c true if the Simulator's
  ///          COMPUTE_CALLBACK_NAME::ProcessDEDrTerm routine returns \c true.
  /// \returns \c false otherwise.
  ///
  /// \sa KIM_ModelComputeArguments_ProcessDEDrTerm,
  /// kim_model_compute_arguments_module::kim_process_dedr_term
  ///
  /// \since 2.0
  int ProcessDEDrTerm(double const de,
                      double const r,
                      double const * const dx,
                      int const i,
                      int const j) const;

  /// \brief Call the Simulator's COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term
  /// routine.
  ///
  /// \todo Add more detailed description of Process mechanism.
  ///
  /// \param[in] de Value of D2EDr2 for particle pairs.
  /// \param[in] r Array of particle pair distances.
  /// \param[in] dx Array of particle pair relative position vectors.
  /// \param[in] i Array of particle numbers (using the Model's Numbering, as
  ///            specified in the call to ModelCreate::SetModelNumbering or
  ///            ModelDriverCreate::SetModelNumbering) for first particle in
  ///            each pair.
  /// \param[in] j Array of particle numbers (using the Model's Numbering) for
  ///            second particle in each pair.
  ///
  /// \returns \c true if the Simulator's
  ///          COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term routine returns \c
  ///          true.
  /// \returns \c false otherwise.
  ///
  /// \sa KIM_ModelComputeArguments_ProcessD2EDr2Term,
  /// kim_model_compute_arguments_module::kim_process_d2edr2_term
  ///
  /// \since 2.0
  int ProcessD2EDr2Term(double const de,
                        double const * const r,
                        double const * const dx,
                        int const * const i,
                        int const * const j) const;

  /// \brief Get the data pointer for a ComputeArgumentName.
  ///
  /// \param[in] computeArgumentName The ComputeArgumentName of interest.
  /// \param[out] ptr The data pointer.
  ///
  /// \return \c true if \c computeArgumentName is unknown.
  /// \return \c true if `computeArgumentName == SUPPORT_STATUS::notSupported`.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ModelComputeArguments_GetArgumentPointerInteger,
  ///     KIM_ModelComputeArguments_GetArgumentPointerDouble,
  ///     kim_model_compute_arguments_module::kim_get_argument_pointer
  ///
  /// \since 2.0
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int const ** const ptr) const;

  /// \overload
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         int ** const ptr) const;

  /// \overload
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double const ** const ptr) const;

  /// \overload
  int GetArgumentPointer(ComputeArgumentName const computeArgumentName,
                         double ** const ptr) const;

  /// \brief Determine if the Simulator has provided a non-NULL function
  /// pointer for a ComputeCallbackName of interest.
  ///
  /// \param[in]  computeCallbackName The ComputeCallbackName of interest.
  /// \param[out] present Is \c true if the callback's function pointer is
  ///             non-NULL, and is \c false otherwise.
  ///
  /// \return \c true if \c computeArgumentName is unknown.
  /// \return \c true if \c computeArguemntName is SUPPORT_STATUS::notSupported.
  /// \return \c false otherwise.
  ///
  /// \post \c present is unchanged in an error occurs.
  ///
  /// \sa KIM_ModelComputeArguments_IsCallbackPresent,
  /// kim_model_compute_arguments_module::kim_is_callback_present
  ///
  /// \since 2.0
  int IsCallbackPresent(ComputeCallbackName const computeCallbackName,
                        int * const present) const;

  /// \brief Set the \ref cache_buffer_pointers "Model's buffer pointer"
  /// within the ComputeArguments object.
  ///
  /// The model buffer pointer may be used by the model to associate
  /// a memory buffer with the ComputeArguments object.
  ///
  /// \param[in] ptr The model buffer data pointer.
  ///
  /// \sa KIM_ModelComputeArguments_SetModelBufferPointer,
  /// kim_model_compute_arguments_module::kim_set_model_buffer_pointer
  ///
  /// \since 2.0
  void SetModelBufferPointer(void * const ptr);

  /// \brief Get the \ref cache_buffer_pointers "Model's buffer pointer"
  /// within the ComputeArguments object.
  ///
  /// The model buffer pointer may be used by the Model to associate
  /// a memory buffer with the ComputeArguments object.
  ///
  /// \param[out] ptr The model buffer data pointer.
  ///
  /// \note `ptr == NULL` if the model has not previously called
  ///       ModelComputeArguments::SetModelBufferPointer.
  ///
  /// \sa KIM_ModelComputeArguments_GetModelBufferPointer,
  /// kim_model_compute_arguments_module::kim_get_model_buffer_pointer
  ///
  /// \since 2.0
  void GetModelBufferPointer(void ** const ptr) const;

  /// \brief Write a log entry into the log file.
  ///
  /// This results in a no-op if \c logVerbosity is LOG_VERBOSITY::silent or if
  /// \c logVerbosity is greater-than the Log object's top LogVerbosity on its
  /// stack.
  ///
  /// \param[in] logVerbosity The LogVerbosity level for the entry.
  /// \param[in] message The body text of the log entry.
  /// \param[in] lineNumber The source code file line number.
  /// \param[in] fileName The source code file name.
  ///
  /// \sa KIM_ModelComputeArguments_LogEntry,
  /// kim_model_compute_arguments_module::kim_log_entry
  ///
  /// \since 2.0
  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;

  /// \overload
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

  /// \brief Get a string representing the internal state of the
  /// ComputeArguments object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_ModelComputeArguments_ToString,
  /// kim_model_compute_arguments_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

 private:
  // do not allow copy constructor or operator=
  ModelComputeArguments(ModelComputeArguments const &);
  void operator=(ModelComputeArguments const &);

  ModelComputeArguments();
  ~ModelComputeArguments();

  ModelComputeArgumentsImplementation * pimpl;
};  // class ModelComputeArguments
}  // namespace KIM

#endif  // KIM_MODEL_COMPUTE_ARGUMENTS_HPP_
