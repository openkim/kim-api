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


#ifndef KIM_LANGUAGE_NAME_HPP_
#define KIM_LANGUAGE_NAME_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// LanguageName's supported by the %KIM API.
///
/// The enumeration constants are contained in the LANGUAGE_NAME namespace.
///
/// \sa KIM_LanguageName, kim_language_name_module::kim_language_name_type
///
/// \since 2.0
class LanguageName
{
 public:
  /// \brief Integer identifying the specific LanguageName represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_LanguageName::languageNameID,
  /// kim_language_name_module::kim_language_name_type::language_name_id
  ///
  /// \since 2.0
  int languageNameID;

  /// \brief Create an uninitialized LanguageName object.
  ///
  /// \since 2.0
  LanguageName();

  /// \brief Create a LanguageName object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  LanguageName(int const id);

  /// \brief Create a LanguageName object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_LanguageName_FromString,
  /// kim_language_name_module::kim_from_string
  ///
  /// \since 2.0
  LanguageName(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// LanguageName's known to the %KIM API are found in the LANGUAGE_NAME
  /// namespace.
  ///
  /// \sa KIM_LanguageName_Known, kim_language_name_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares LanguageName objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_LanguageName_Equal, kim_language_name_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(LanguageName const & rhs) const;

  /// \brief Compares LanguageName objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_LanguageName_NotEqual, kim_language_name_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(LanguageName const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the LanguageName object.
  ///
  /// \note If the LanguageName object does not correspond to a value defined
  /// by the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_LanguageName_ToString, kim_language_name_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class LanguageName

/// \brief Contains the enumeration constants and the discovery routines for
/// the LanguageName \ref extensible_enumeration "Extensible Enumeration".
namespace LANGUAGE_NAME
{
/// \brief The standard \c cpp language.
///
/// \todo Add more detailed description of the language.
///
/// \sa KIM_LANGUAGE_NAME_cpp, kim_language_name_module::kim_language_name_cpp
///
/// \since 2.0
extern LanguageName const cpp;

/// \brief The standard \c c language.
///
/// \todo Add more detailed description of the language.
///
/// \sa KIM_LANGUAGE_NAME_c, kim_language_name_module::kim_language_name_c
///
/// \since 2.0
extern LanguageName const c;

/// \brief The standard \c fortran language.
///
/// \todo Add more detailed description of the language.
///
/// \sa KIM_LANGUAGE_NAME_fortran,
/// kim_language_name_module::kim_language_name_fortran
///
/// \since 2.0
extern LanguageName const fortran;


/// \brief Get the number of standard LanguageName's defined by the %KIM
/// API.
///
/// \param[out] numberOfLanguageNames The number of standard LanguageName's
///             defined by the %KIM API.
///
/// \sa KIM_LANGUAGE_NAME_GetNumberOfLanguageNames,
/// kim_language_name_module::kim_get_number_of_language_names
///
/// \since 2.0
void GetNumberOfLanguageNames(int * const numberOfLanguageNames);

/// \brief Get the identity of each defined standard LanguageName.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             LanguageName.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] languageName The LanguageName object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfLanguageNames`.
/// \return \c false otherwise.
///
/// \sa KIM_LANGUAGE_NAME_GetLanguageName,
/// kim_language_name_module::kim_get_language_name
///
/// \since 2.0
int GetLanguageName(int const index, LanguageName * const languageName);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for LanguageName
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(LanguageName const & a, LanguageName const & b) const
  {
    return a.languageNameID < b.languageNameID;
  }
};  // struct Comparator
}  // namespace LANGUAGE_NAME
}  // namespace KIM

#endif  // KIM_LANGUAGE_NAME_HPP_
