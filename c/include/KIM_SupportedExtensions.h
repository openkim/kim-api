/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
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
 ** \sa KIM::SupportedExtensions
 **
 ** \since 2.0
 **/
struct KIM_SupportedExtensions
{
  /**
   ** \brief \copybrief KIM::SupportedExtensions::numberOfSupportedExtensions
   **
   ** \sa KIM::SupportedExtensions::numberOfSupportedExtensions
   **
   ** \since 2.0
   **/
  int numberOfSupportedExtensions;
  /**
   ** \brief \copybrief KIM::SupportedExtensions::supportedExtensionID
   **
   ** \sa KIM::SupportedExtensions::supportedExtensionID
   **
   ** \since 2.0
   **/
  char supportedExtensionID[KIM_MAX_NUMBER_OF_EXTENSIONS]
                           [KIM_MAX_EXTENSION_ID_LENGTH];

  /**
   ** \brief \copybrief KIM::SupportedExtensions::supportedExtensionRequired
   **
   ** \sa KIM::SupportedExtensions::supportedExtensionRequired
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
