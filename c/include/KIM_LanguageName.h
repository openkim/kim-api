/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2022, Regents of the University of Minnesota.          */
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
/* Release: This file is part of the kim-api-2.3.0 package.                   */
/*                                                                            */


#ifndef KIM_LANGUAGE_NAME_H_
#define KIM_LANGUAGE_NAME_H_

/**
 ** \brief \copybrief KIM::LanguageName
 **
 ** \sa KIM::LanguageName, kim_language_name_module::kim_language_name_type
 **
 ** \since 2.0
 **/
struct KIM_LanguageName
{
  /**
   ** \brief \copybrief KIM::LanguageName::languageNameID
   **
   ** \sa KIM::LanguageName::languageNameID,
   ** kim_language_name_module::kim_language_name_type::language_name_id
   **
   ** \since 2.0
   **/
  int languageNameID;
};
#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

/**
 ** \brief \copybrief KIM::LanguageName::LanguageName(std::string const &)
 **
 ** \sa KIM::LanguageName::LanguageName(std::string const &),
 ** kim_language_name_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_LanguageName KIM_LanguageName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::LanguageName::Known
 **
 ** \sa KIM::LanguageName::Known, kim_language_name_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_LanguageName_Known(KIM_LanguageName const languageName);

/**
 ** \brief \copybrief KIM::LanguageName::operator==()
 **
 ** \sa KIM::LanguageName::operator==(),
 ** kim_language_name_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_LanguageName_Equal(KIM_LanguageName const lhs,
                           KIM_LanguageName const rhs);

/**
 ** \brief \copybrief KIM::LanguageName::operator!=()
 **
 ** \sa KIM::LanguageName::operator!=(),
 ** kim_language_name_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_LanguageName_NotEqual(KIM_LanguageName const lhs,
                              KIM_LanguageName const rhs);

/**
 ** \brief \copybrief KIM::LanguageName::ToString
 **
 ** \sa KIM::LanguageName::ToString, kim_language_name_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_LanguageName_ToString(KIM_LanguageName const languageName);

/**
 ** \brief \copybrief KIM::LANGUAGE_NAME::cpp
 **
 ** \sa KIM::LANGUAGE_NAME::cpp,
 ** kim_language_name_module::kim_language_name_cpp
 **
 ** \since 2.0
 **/
extern KIM_LanguageName const KIM_LANGUAGE_NAME_cpp;

/**
 ** \brief \copybrief KIM::LANGUAGE_NAME::c
 **
 ** \sa KIM::LANGUAGE_NAME::c, kim_language_name_module::kim_language_name_c
 **
 ** \since 2.0
 **/
extern KIM_LanguageName const KIM_LANGUAGE_NAME_c;

/**
 ** \brief \copybrief KIM::LANGUAGE_NAME::fortran
 **
 ** \sa KIM::LANGUAGE_NAME::fortran,
 ** kim_language_name_module::kim_language_name_fortran
 **
 ** \since 2.0
 **/
extern KIM_LanguageName const KIM_LANGUAGE_NAME_fortran;

/**
 ** \brief \copybrief KIM::LANGUAGE_NAME::GetNumberOfLanguageNames
 **
 ** \sa KIM::LANGUAGE_NAME::GetNumberOfLanguageNames,
 ** kim_language_name_module::kim_get_number_of_language_names
 **
 ** \since 2.0
 **/
void KIM_LANGUAGE_NAME_GetNumberOfLanguageNames(
    int * const numberOfLanguageNames);

/**
 ** \brief \copybrief KIM::LANGUAGE_NAME::GetLanguageName
 **
 ** \sa KIM::LANGUAGE_NAME::GetLanguageName,
 ** kim_language_name_module::kim_get_language_name
 **
 ** \since 2.0
 **/
int KIM_LANGUAGE_NAME_GetLanguageName(int const index,
                                      KIM_LanguageName * const languageName);

#endif /* KIM_LANGUAGE_NAME_H_ */
