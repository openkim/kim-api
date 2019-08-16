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
// Release: This file is part of the kim-api-2.1.3 package.
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
