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


#ifndef KIM_SUPPORTED_EXTENSIONS_HPP_
#define KIM_SUPPORTED_EXTENSIONS_HPP_

#define KIM_SUPPORTED_EXTENSIONS_ID "KIM_SupportedExtensions"
#define KIM_MAX_EXTENSION_ID_LENGTH 128
#define KIM_MAX_NUMBER_OF_EXTENSIONS 64

namespace KIM
{
/// \brief The only standard extension defined by the %KIM API.
///
/// This structure provides a standard mechanism for extensions to be
/// discovered at run time.
///
/// \sa KIM_SupportedExtensions,
/// kim_supported_extensions_module::kim_supported_extensions_type
///
/// \since 2.0
struct SupportedExtensions
{
  /// The number of extensions supported by the Model.
  int numberOfSupportedExtensions;

  /// The unique extension ID's of each supported extension.
  char supportedExtensionID[KIM_MAX_NUMBER_OF_EXTENSIONS]
                           [KIM_MAX_EXTENSION_ID_LENGTH];

  /// \c true if the model requires the simulator to execute the corresponding
  /// extension in order to be used correctly, \c false otherwise.
  int supportedExtensionRequired[KIM_MAX_NUMBER_OF_EXTENSIONS];
};  // struct SupportedExtensions
}  // namespace KIM

#endif  // KIM_SUPPORTED_EXTENSIONS_HPP_
