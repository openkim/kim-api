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


#ifndef KIM_NUMBERING_H_
#define KIM_NUMBERING_H_

/**
 ** \brief \copybrief KIM::Numbering
 **
 ** \sa KIM::Numbering
 **
 ** \since 2.0
 **/
struct KIM_Numbering
{
  /**
   ** \brief \copybrief KIM::Numbering::numberingID
   **
   ** \sa KIM::Numbering::numberingID
   **
   ** \since 2.0
   **/
  int numberingID;
};
#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
typedef struct KIM_Numbering KIM_Numbering;
#endif

/**
 ** \brief \copybrief KIM::Numbering::Numbering(std::string const &)
 **
 ** \sa KIM::Numbering::Numbering(std::string const &)
 **
 ** \since 2.0
 **/
KIM_Numbering KIM_Numbering_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::Numbering::Known
 **
 ** \sa KIM::Numbering::Known
 **
 ** \since 2.0
 **/
int KIM_Numbering_Known(KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::Numbering::operator==()
 **
 ** \sa KIM::Numbering::operator==()
 **
 ** \since 2.0
 **/
int KIM_Numbering_Equal(KIM_Numbering const lhs, KIM_Numbering const rhs);

/**
 ** \brief \copybrief KIM::Numbering::operator!=()
 **
 ** \sa KIM::Numbering::operator!=()
 **
 ** \since 2.0
 **/
int KIM_Numbering_NotEqual(KIM_Numbering const lhs, KIM_Numbering const rhs);

/**
 ** \brief \copybrief KIM::Numbering::ToString
 **
 ** \sa KIM::Numbering::ToString
 **
 ** \since 2.0
 **/
char const * KIM_Numbering_ToString(KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::NUMBERING::zeroBased
 **
 ** \sa KIM::NUMBERING::zeroBased
 **
 ** \since 2.0
 **/
extern KIM_Numbering const KIM_NUMBERING_zeroBased;

/**
 ** \brief \copybrief KIM::NUMBERING::oneBased
 **
 ** \sa KIM::NUMBERING::oneBased
 **
 ** \since 2.0
 **/
extern KIM_Numbering const KIM_NUMBERING_oneBased;

/**
 ** \brief \copybrief KIM::NUMBERING::GetNumberOfNumberings
 **
 ** \sa KIM::NUMBERING::GetNumberOfNumberings
 **
 ** \since 2.0
 **/
void KIM_NUMBERING_GetNumberOfNumberings(int * const numberOfNumberings);

/**
 ** \brief \copybrief KIM::NUMBERING::GetNumbering
 **
 ** \sa KIM::NUMBERING::GetNumbering
 **
 ** \since 2.0
 **/
int KIM_NUMBERING_GetNumbering(int const index,
                               KIM_Numbering * const numbering);

#endif /* KIM_NUMBERING_H_ */
