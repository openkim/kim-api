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


#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#define KIM_COMPUTE_ARGUMENT_NAME_HPP_

#include <string>

namespace KIM
{
// Forward declaration
class DataType;

/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// ComputeArgumentName's supported by the %KIM API.
///
/// The enumeration constants are contained in the COMPUTE_ARGUMENT_NAME
/// namespace.
///
/// \sa KIM_ComputeArgumentName,
/// kim_compute_argument_name_module::kim_compute_argument_name_type
///
/// \since 2.0
class ComputeArgumentName
{
 public:
  /// \brief Integer identifying the specific ComputeArgumentName represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_ComputeArgumentName::computeArgumentNameID,
  /// kim_compute_argument_name_module::kim_compute_argument_name_type::<!--
  /// -->compute_argument_name_id
  ///
  /// \since 2.0
  int computeArgumentNameID;

  /// \brief Create an uninitialized ComputeArgumentName object.
  ///
  /// \since 2.0
  ComputeArgumentName();

  /// \brief Create a ComputeArgumentName object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  ComputeArgumentName(int const id);

  /// \brief Create a ComputeArgumentName object corresponding to the provided
  /// string.  If the string does not match one of the values defined by the
  /// %KIM API, then an "unknown" object is generated.
  ///
  /// \sa KIM_ComputeArgumentName_FromString,
  /// kim_compute_argument_name_module::kim_from_string
  ///
  /// \since 2.0
  ComputeArgumentName(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// ComputeArgumentName's known to the %KIM API are found in the
  /// COMPUTE_ARGUMENT_NAME namespace.
  ///
  /// \sa KIM_ComputeArgumentName_Known,
  /// kim_compute_argument_name_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares ComputeArgumentName objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_ComputeArgumentName_Equal,
  /// kim_compute_argument_name_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(ComputeArgumentName const & rhs) const;

  /// \brief Compares ComputeArgumentName objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_ComputeArgumentName_NotEqual,
  /// kim_compute_argument_name_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(ComputeArgumentName const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the ComputeArgumentName object.
  ///
  /// \note If the ComputeArgumentName object does not correspond to a value
  /// defined by the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_ComputeArgumentName_ToString,
  /// kim_compute_argument_name_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class ComputeArgumentName

/// \brief Contains the enumeration constants and the discovery routines for
/// the ComputeArgumentName \ref extensible_enumeration
/// "Extensible Enumeration".
namespace COMPUTE_ARGUMENT_NAME
{
/// \brief The standard \c numberOfParticles argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles,
/// kim_compute_argument_name_module::<!--
/// -->kim_compute_argument_name_number_of_particles
///
/// \since 2.0
extern ComputeArgumentName const numberOfParticles;

/// \brief The standard \c particleSpeciesCodes argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes,
/// kim_compute_argument_name_module::<!--
/// -->kim_compute_argument_name_particle_species_codes
///
/// \since 2.0
extern ComputeArgumentName const particleSpeciesCodes;

/// \brief The standard \c particleContributing argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_particleContributing,
/// kim_compute_argument_name_module::<!--
/// -->kim_compute_argument_name_particle_contributing
///
/// \since 2.0
extern ComputeArgumentName const particleContributing;

/// \brief The standard \c coordinates argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_coordinates,
/// kim_compute_argument_name_module::kim_compute_argument_name_coordinates
///
/// \since 2.0
extern ComputeArgumentName const coordinates;

/// \brief The standard \c partialEnergy argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_partialEnergy,
/// kim_compute_argument_name_module::kim_compute_argument_name_partial_energy
///
/// \since 2.0
extern ComputeArgumentName const partialEnergy;

/// \brief The standard \c partialForces argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_partialForces,
/// kim_compute_argument_name_module::kim_compute_argument_name_partial_forces
///
/// \since 2.0
extern ComputeArgumentName const partialForces;

/// \brief The standard \c partialParticleEnergy argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy,
/// kim_compute_argument_name_module::<!--
/// -->kim_compute_argument_name_partial_particle_energy
///
/// \since 2.0
extern ComputeArgumentName const partialParticleEnergy;

/// \brief The standard \c partialVirial argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_partialVirial,
/// kim_compute_argument_name_module::kim_compute_argument_name_partial_virial
///
/// \since 2.0
extern ComputeArgumentName const partialVirial;

/// \brief The standard \c partialParticleVirial argument.
///
/// \todo Add more detailed description of argument.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial,
/// kim_compute_argument_name_module::<!--
/// -->kim_compute_argument_name_partial_particle_virial
///
/// \since 2.0
extern ComputeArgumentName const partialParticleVirial;


/// \brief Get the number of standard ComputeArgumentName's defined by the %KIM
/// API.
///
/// \param[out] numberOfComputeArgumentNames The number of standard
///             ComputeArgumentName's defined by the %KIM API.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames,
/// kim_compute_argument_name_module::kim_get_number_of_compute_argument_names
///
/// \since 2.0
void GetNumberOfComputeArgumentNames(int * const numberOfComputeArgumentNames);

/// \brief Get the identity of each defined standard ComputeArgumentName.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             ComputeArgumentName.  This index ordering is only guaranteed to
///             be stable during the lifetime of the current process.
/// \param[out] computeArgumentName The ComputeArgumentName object associated
///             with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfComputeArgumentNames`.
/// \return \c false otherwise.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName,
/// kim_compute_argument_name_module::kim_get_compute_argument_name
///
/// \since 2.0
int GetComputeArgumentName(int const index,
                           ComputeArgumentName * const computeArgumentName);

/// \brief Get the DataType of each defined standard ComputeArgumentName.
///
/// \param[in] computeArgumentName The ComputeArgumentName object of interest.
/// \param[out] dataType The DataType of the associated ComputeArgumentName.
///
/// \return \c true if \p computeArgumentName is "unknown".
/// \return \c false otherwise.
///
/// \sa KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType,
/// kim_compute_argument_name_module::kim_get_compute_argument_data_type
///
/// \since 2.0
int GetComputeArgumentDataType(ComputeArgumentName const computeArgumentName,
                               DataType * const dataType);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for
  /// ComputeArgumentsName objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(ComputeArgumentName const & a,
                  ComputeArgumentName const & b) const
  {
    return a.computeArgumentNameID < b.computeArgumentNameID;
  }
};  // struct Comparator
}  // namespace COMPUTE_ARGUMENT_NAME
}  // namespace KIM

#endif  // KIM_COMPUTE_ARGUMENT_NAME_HPP_
