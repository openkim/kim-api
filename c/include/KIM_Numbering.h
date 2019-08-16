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
/* Release: This file is part of the kim-api-2.1.3 package.                   */
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
