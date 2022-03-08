/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2021, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */
/* SPDX-License-Identifier: LGPL-2.1-or-later                                 */
/*                                                                            */
/* This library is free software; you can redistribute it and/or              */
/* modify it under the terms of the GNU Lesser General Public                 */
/* License as published by the Free Software Foundation; either               */
/* version 2.1 of the License, or (at your option) any later version.         */
/*                                                                            */
/* This library is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          */
/* Lesser General Public License for more details.                            */
/*                                                                            */
/* You should have received a copy of the GNU Lesser General Public License   */
/* along with this library; if not, write to the Free Software Foundation,    */
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_SUPPORTED_EXTENSIONS_H_
#define KIM_SUPPORTED_EXTENSIONS_H_

#define KIM_SUPPORTED_EXTENSIONS_ID "KIM_SupportedExtensions"
#define KIM_MAX_EXTENSION_ID_LENGTH 128
#define KIM_MAX_NUMBER_OF_EXTENSIONS 64

/**
 ** \brief \copybrief KIM::SupportedExtensions
 **
 ** \sa KIM::SupportedExtensions,
 ** kim_supported_extensions_module::kim_supported_extensions_type
 **
 ** \since 2.0
 **/
struct KIM_SupportedExtensions
{
  /**
   ** \brief \copybrief KIM::SupportedExtensions::numberOfSupportedExtensions
   **
   ** \sa KIM::SupportedExtensions::numberOfSupportedExtensions,
   ** kim_supported_extensions_module::kim_supported_extensions_type::<!--
   ** -->number_of_supported_extensions
   **
   ** \since 2.0
   **/
  int numberOfSupportedExtensions;
  /**
   ** \brief \copybrief KIM::SupportedExtensions::supportedExtensionID
   **
   ** \sa KIM::SupportedExtensions::supportedExtensionID,
   ** kim_supported_extensions_module::kim_supported_extensions_type::<!--
   ** -->supported_extension_id
   **
   ** \since 2.0
   **/
  char supportedExtensionID[KIM_MAX_NUMBER_OF_EXTENSIONS]
                           [KIM_MAX_EXTENSION_ID_LENGTH];

  /**
   ** \brief \copybrief KIM::SupportedExtensions::supportedExtensionRequired
   **
   ** \sa KIM::SupportedExtensions::supportedExtensionRequired,
   ** kim_supported_extensions_module::kim_supported_extensions_type::<!--
   ** -->supported_extension_required
   **
   ** \since 2.0
   **/
  int supportedExtensionRequired[KIM_MAX_NUMBER_OF_EXTENSIONS];
};

#ifndef KIM_SUPPORTED_EXTENSIONS_DEFINED_
#define KIM_SUPPORTED_EXTENSIONS_DEFINED_
typedef struct KIM_SupportedExtensions KIM_SupportedExtensions;
#endif

#endif /* KIM_SUPPORTED_EXTENSIONS_H_ */
