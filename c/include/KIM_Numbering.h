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
/* Release: This file is part of the kim-api-2.4.1 package.                   */
/*                                                                            */


#ifndef KIM_NUMBERING_H_
#define KIM_NUMBERING_H_

/**
 ** \brief \copybrief KIM::Numbering
 **
 ** \sa KIM::Numbering, kim_numbering_module::kim_numbering_type
 **
 ** \since 2.0
 **/
struct KIM_Numbering
{
  /**
   ** \brief \copybrief KIM::Numbering::numberingID
   **
   ** \sa KIM::Numbering::numberingID,
   ** kim_numbering_module::kim_numbering_type::numbering_id
   **
   ** \since 2.0
   **/
  int numberingID;
};
#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_Numbering KIM_Numbering;
#endif

/**
 ** \brief \copybrief KIM::Numbering::Numbering(std::string const &)
 **
 ** \sa KIM::Numbering::Numbering(std::string const &),
 ** kim_numbering_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_Numbering KIM_Numbering_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::Numbering::Known
 **
 ** \sa KIM::Numbering::Known, kim_numbering_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_Numbering_Known(KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::Numbering::operator==()
 **
 ** \sa KIM::Numbering::operator==(), kim_numbering_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_Numbering_Equal(KIM_Numbering const lhs, KIM_Numbering const rhs);

/**
 ** \brief \copybrief KIM::Numbering::operator!=()
 **
 ** \sa KIM::Numbering::operator!=(), kim_numbering_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_Numbering_NotEqual(KIM_Numbering const lhs, KIM_Numbering const rhs);

/**
 ** \brief \copybrief KIM::Numbering::ToString
 **
 ** \sa KIM::Numbering::ToString, kim_numbering_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_Numbering_ToString(KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::NUMBERING::zeroBased
 **
 ** \sa KIM::NUMBERING::zeroBased,
 ** kim_numbering_module::kim_numbering_zero_based
 **
 ** \since 2.0
 **/
extern KIM_Numbering const KIM_NUMBERING_zeroBased;

/**
 ** \brief \copybrief KIM::NUMBERING::oneBased
 **
 ** \sa KIM::NUMBERING::oneBased, kim_numbering_module::kim_numbering_one_based
 **
 ** \since 2.0
 **/
extern KIM_Numbering const KIM_NUMBERING_oneBased;

/**
 ** \brief \copybrief KIM::NUMBERING::GetNumberOfNumberings
 **
 ** \sa KIM::NUMBERING::GetNumberOfNumberings,
 ** kim_numbering_module::kim_get_number_of_numberings
 **
 ** \since 2.0
 **/
void KIM_NUMBERING_GetNumberOfNumberings(int * const numberOfNumberings);

/**
 ** \brief \copybrief KIM::NUMBERING::GetNumbering
 **
 ** \sa KIM::NUMBERING::GetNumbering, kim_numbering_module::kim_get_numbering
 **
 ** \since 2.0
 **/
int KIM_NUMBERING_GetNumbering(int const index,
                               KIM_Numbering * const numbering);

#endif /* KIM_NUMBERING_H_ */
