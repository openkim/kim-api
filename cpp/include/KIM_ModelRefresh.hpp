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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.2.1 package.
//


#ifndef KIM_MODEL_REFRESH_HPP_
#define KIM_MODEL_REFRESH_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class ModelRefreshImplementation;


/// \brief Provides the interface to a %KIM API Model object for use by models
/// within their MODEL_ROUTINE_NAME::Refresh routine.
///
/// \sa KIM_ModelRefresh,
/// kim_model_refresh_module::kim_model_refresh_handle_type
///
/// \since 2.0
class ModelRefresh
{
 public:
  /// \brief Set the Model's influence distance data pointer.
  ///
  /// \todo Add more detailed description of \c influenceDistance (or link to
  /// docs elsewhere?)
  ///
  /// \param[in] influenceDistance Pointer to Model's influence distance.
  ///
  /// \note The model is responsible for allocating the memory associated with
  /// the influence distance data.  The model must use the \ref
  /// cache_buffer_pointers "Model's buffer pointer" to retain access to this
  /// memory location and avoid a memory leak.
  ///
  /// \sa KIM_ModelRefresh_SetInfluenceDistancePointer,
  /// kim_model_refresh_module::kim_set_influence_distance_pointer
  ///
  /// \since 2.0
  void SetInfluenceDistancePointer(double const * const influenceDistance);

  /// \brief Set the Model's neighbor list data pointers.
  ///
  /// \todo Add more detailed description
  ///
  /// \param[in] numberOfNeighborLists The number of neighbor lists required by
  ///            the Model.
  /// \param[in] cutoffs Array of cutoff values for each of the required
  ///            neighbor lists.
  /// \param[in] modelWillNotRequestNeighborsOfNoncontributingParticles Array
  ///            of integers; \c true or \c false for each neighbor list
  ///            required by the Model.
  ///
  /// \note The model is responsible for allocating the memory associated with
  /// the neighbor list cutoffs and \c
  /// modelWillNotRequestNeighborsOfNoncontributingParticles data.  The model
  /// must use the \ref cache_buffer_pointers "Model's buffer pointer" to
  /// retain access to this memory location and avoid a memory leak.
  ///
  /// \sa KIM_ModelRefresh_SetNeighborListPointers,
  /// kim_model_refresh_module::kim_set_neighbor_list_pointers
  ///
  /// \since 2.0
  void SetNeighborListPointers(
      int const numberOfNeighborLists,
      double const * const cutoffs,
      int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

  /// \brief Get the \ref cache_buffer_pointers "Model's buffer pointer"
  /// within the Model object.
  ///
  /// The model buffer pointer may be used by the Model to associate
  /// a memory buffer with the Model object.
  ///
  /// \param[out] ptr The model buffer data pointer.
  ///
  /// \note `ptr == NULL` if the model has not previously called
  ///       ModelCreate::SetModelBufferPointer or
  ///       ModelDriverCreate::SetModelBufferPointer.
  ///
  /// \sa KIM_ModelRefresh_GetModelBufferPointer,
  /// kim_model_refresh_module::kim_get_model_buffer_pointer
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
  /// \sa KIM_ModelRefresh_LogEntry, kim_model_refresh_module::kim_log_entry
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

  /// \brief Get a string representing the internal state of the Model object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_ModelRefresh_ToString, kim_model_refresh_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

 private:
  // do not allow copy constructor or operator=
  ModelRefresh(ModelRefresh const &);
  void operator=(ModelRefresh const &);

  ModelRefresh();
  ~ModelRefresh();

  ModelRefreshImplementation * pimpl;
};  // class ModelRefresh
}  // namespace KIM

#endif  // KIM_MODEL_REFRESH_HPP_
