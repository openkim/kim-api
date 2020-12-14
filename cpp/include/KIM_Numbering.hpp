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


#ifndef KIM_NUMBERING_HPP_
#define KIM_NUMBERING_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// Numbering's supported by the %KIM API.
///
/// The enumeration constants are contained in the NUMBERING namespace.
///
/// \sa KIM_Numbering, kim_numbering_module::kim_numbering_type
///
/// \since 2.0
class Numbering
{
 public:
  /// \brief Integer identifying the specific Numbering represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_Numbering::numberingID,
  /// kim_numbering_module::kim_numbering_type::numbering_id
  ///
  /// \since 2.0
  int numberingID;

  /// \brief Create an uninitialized Numbering object.
  ///
  /// \since 2.0
  Numbering();

  /// \brief Create a Numbering object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  Numbering(int const id);

  /// \brief Create a Numbering object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_Numbering_FromString, kim_numbering_module::kim_from_string
  ///
  /// \since 2.0
  Numbering(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// Numbering's known to the %KIM API are found in the NUMBERING namespace.
  ///
  /// \sa KIM_Numbering_Known, kim_numbering_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares Numbering objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_Numbering_Equal, kim_numbering_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(Numbering const & rhs) const;

  /// \brief Compares Numbering objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_Numbering_NotEqual, kim_numbering_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(Numbering const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the Numbering object.
  ///
  /// \note If the Numbering object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_Numbering_ToString, kim_numbering_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class Numbering

/// \brief Contains the enumeration constants and the discovery routines for
/// the Numbering \ref extensible_enumeration "Extensible Enumeration".
namespace NUMBERING
{
/// \brief The standard \c zeroBased numbering.
///
/// Quantities are numbered starting from zero.
///
/// \sa KIM_NUMBERING_zeroBased, kim_numbering_module::kim_numbering_zero_based
///
/// \since 2.0
extern Numbering const zeroBased;

/// \brief The standard \c oneBased numbering.
///
/// Quantities are numbered starting from one.
///
/// \sa KIM_NUMBERING_oneBased, kim_numbering_module::kim_numbering_one_based
///
/// \since 2.0
extern Numbering const oneBased;


/// \brief Get the number of standard Numbering's defined by the %KIM
/// API.
///
/// \param[out] numberOfNumberings The number of standard Numbering's defined
///             by the %KIM API.
///
/// \sa KIM_NUMBERING_GetNumberOfNumberings,
/// kim_numbering_module::kim_get_number_of_numberings
///
/// \since 2.0
void GetNumberOfNumberings(int * const numberOfNumberings);

/// \brief Get the identity of each defined standard Numbering.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             Numbering.  This index ordering is only guaranteed to be stable
///             during the lifetime of the current process.
/// \param[out] numbering The Numbering object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfNumberings`.
/// \return \c false otherwise.
///
/// \sa KIM_NUMBERING_GetNumbering, kim_numbering_module::kim_get_numbering
///
/// \since 2.0
int GetNumbering(int const index, Numbering * const numbering);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for Numbering
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(Numbering const & a, Numbering const & b) const
  {
    return a.numberingID < b.numberingID;
  }
};  // struct Comparator
}  // namespace NUMBERING
}  // namespace KIM

#endif  // KIM_NUMBERING_HPP_
