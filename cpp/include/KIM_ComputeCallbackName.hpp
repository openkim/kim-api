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
// Release: This file is part of the kim-api-2.3.0 package.
//


#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#define KIM_COMPUTE_CALLBACK_NAME_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// ComputeCallbackName's supported by the %KIM API.
///
/// The enumeration constants are contained in the COMPUTE_CALLBACK_NAME
/// namespace.
///
/// \sa KIM_ComputeCallbackName,
/// kim_compute_callback_name_module::kim_compute_callback_name_type
///
/// \since 2.0
class ComputeCallbackName
{
 public:
  /// \brief Integer identifying the specific ComputeCallbackName represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_ComputeCallbackName::computeCallbackNameID,
  /// kim_compute_callback_name_module::kim_compute_callback_name_type::<!--
  /// -->compute_callback_name_id
  ///
  /// \since 2.0
  int computeCallbackNameID;

  /// \brief Create an uninitialized ComputeCallbackName object.
  ///
  /// \since 2.0
  ComputeCallbackName();

  /// \brief Create a ComputeCallbackName object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  ComputeCallbackName(int const id);

  /// \brief Create a ComputeCallbackName object corresponding to the provided
  /// string.  If the string does not match one of the values defined by the
  /// %KIM API, then an "unknown" object is generated.
  ///
  /// \sa KIM_ComputeCallbackName_FromString,
  /// kim_compute_callback_name_module::kim_from_string
  ///
  /// \since 2.0
  ComputeCallbackName(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// ComputeCallbackName's known to the %KIM API are found in the
  /// COMPUTE_CALLBACK_NAME namespace.
  ///
  /// \sa KIM_ComputeCallbackName_Known,
  /// kim_compute_callback_name_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares ComputeCallbackName objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_ComputeCallbackName_Equal,
  /// kim_compute_callback_name_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(ComputeCallbackName const & rhs) const;

  /// \brief Compares ComputeCallbackName objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_ComputeCallbackName_NotEqual,
  /// kim_compute_callback_name_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(ComputeCallbackName const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the ComputeCallbackName object.
  ///
  /// \note If the ComputeCallbackName object does not correspond to a value
  /// defined by the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_ComputeCallbackName_ToString,
  /// kim_compute_callback_name_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class ComputeCallbackName

/// \brief Contains the enumeration constants and the discovery routines for
/// the ComputeCallbackName \ref extensible_enumeration
/// "Extensible Enumeration".
namespace COMPUTE_CALLBACK_NAME
{
/// \brief The standard \c GetNeighborList callback.
///
/// The C++ prototype for this routine is GetNeighborListFunction().  The C
/// prototype for this routine is KIM_GetNeighborListFunction().
///
/// \todo Add more detailed description of callback.
///
/// \sa KIM_COMPUTE_CALLBACK_NAME_GetNeighborList,
/// kim_compute_callback_name_module::<!--
/// -->kim_compute_callback_name_get_neighbor_list
///
/// \since 2.0
extern ComputeCallbackName const GetNeighborList;

/// \brief The standard \c ProcessDEDrTerm callback.
///
/// The C++ prototype for this routine is ProcessDEDrTermFunction().  The C
/// prototype for this routine is KIM_ProcessDEDrTermFunction().
///
/// \todo Add more detailed description of callback.
///
/// \sa KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm,
/// kim_compute_callback_name_module::<!--
/// -->kim_compute_callback_name_process_dedr_term
///
/// \since 2.0
extern ComputeCallbackName const ProcessDEDrTerm;

/// \brief The standard \c ProcessD2EDr2Term callback.
///
/// The C++ prototype for this routine is ProcessD2EDr2TermFunction().  The C
/// prototype for this routine is KIM_ProcessD2EDr2TermFunction().
///
/// \todo Add more detailed description of callback.
///
/// \sa KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term,
/// kim_compute_callback_name_module::<!--
/// -->kim_compute_callback_name_process_d2edr2_term
///
/// \since 2.0
extern ComputeCallbackName const ProcessD2EDr2Term;


/// \brief Get the number of standard ComputeCallbackName's defined by the %KIM
/// API.
///
/// \param[out] numberOfComputeCallbackNames The number of standard
///             ComputeCallbackName's defined by the %KIM API.
///
/// \sa KIM_COMPUTE_CALLBACK_NAME_GetNumberOfComputeCallbackNames,
/// kim_compute_callback_name_module::kim_get_number_of_compute_callback_names
///
/// \since 2.0
void GetNumberOfComputeCallbackNames(int * const numberOfComputeCallbackNames);

/// \brief Get the identity of each defined standard ComputeCallbackName.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             ComputeCallbackName.  This index ordering is only guaranteed to
///             be stable during the lifetime of the current process.
/// \param[out] computeCallbackName The ComputeCallbackName object associated
///             with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfComputeCallbackNames`.
/// \return \c false otherwise.
///
/// \sa KIM_COMPUTE_CALLBACK_NAME_GetComputeCallbackName,
/// kim_compute_callback_name_module::kim_get_compute_callback_name
///
/// \since 2.0
int GetComputeCallbackName(int const index,
                           ComputeCallbackName * const computeCallbackName);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for
  /// ComputeCallbackName objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(ComputeCallbackName const & a,
                  ComputeCallbackName const & b) const
  {
    return a.computeCallbackNameID < b.computeCallbackNameID;
  }
};  // struct Comparator
}  // namespace COMPUTE_CALLBACK_NAME
}  // namespace KIM

#endif  // KIM_COMPUTE_CALLBACK_NAME_HPP_
