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
// Release: This file is part of the kim-api-2.1.2 package.
//


#ifndef KIM_SEM_VER_HPP_
#define KIM_SEM_VER_HPP_

#include <string>

namespace KIM
{
/// \brief Contains routines related to the %KIM API Semantic Version.
namespace SEM_VER
{
/// \brief Get the %KIM API complete Semantic Version string.
///
/// \sa KIM_SEM_VER_GetSemVer, kim_sem_ver_module::kim_get_sem_ver
///
/// \since 2.0
std::string const & GetSemVer();

/// \brief Compare two Semantic Version strings.
///
/// See the <A HREF="https://semver.org">Semantic Version 2.0.0</A> standard
/// for the definition of the partial ordering for valid Semantic Version
/// strings.
///
/// \param[in]  lhs Left hand side version string in comparison.
/// \param[in]  rhs Right hand side version string in comparison.
/// \param[out] isLessThan Truth value of `lhs < rhs` as defined by the
///             Semantic Version 2.0.0 standard.
///
/// \return \c true if ParseSemVer returns \c true for \c lhs or \c rhs.
/// \return \c false otherwise.
///
/// \sa KIM_SEM_VER_IsLessThan, kim_sem_ver_module::kim_is_less_than
///
/// \since 2.0
int IsLessThan(std::string const & lhs,
               std::string const & rhs,
               int * const isLessThan);

/// \brief Parse Semantic Version string into its six components.
///
/// See the <A HREF="https://semver.org">Semantic Version 2.0.0</A> standard
/// for definitions of valid Semantic Version strings.
///
/// \param[in]  version The Semantic Version string to be parsed.
/// \param[out] major The major version number.
/// \param[out] minor The minor version number.
/// \param[out] patch The patch version number.
/// \param[out] prerelease The prerelease string.
/// \param[out] buildMetadata The build metadata string.
///
/// \return \c true if minor and/or patch are missing.
/// \return \c true if major number has a leading zero or is not a valid
///         integer.
/// \return \c true if minor number has a leading zero or is not a valid
///         integer.
/// \return \c true if patch number has a leading zero or is not a valid
///         integer.
/// \return \c true if the prerelease string is invalid.
/// \return \c true if the build metadata string is invalid.
/// \return \c false otherwise.
///
/// \pre All output arguments may be \c NULL if the corresponding value is
/// not needed.
///
/// \post All output arguments are unchanged if an error occurs.
///
/// \sa KIM_SEM_VER_ParseSemVer, kim_sem_ver_module::kim_parse_sem_ver
///
/// \since 2.0
int ParseSemVer(std::string const & version,
                int * const major,
                int * const minor,
                int * const patch,
                std::string * const prerelease,
                std::string * const buildMetadata);
}  // namespace SEM_VER
}  // namespace KIM

#endif  // KIM_SEM_VER_HPP_
