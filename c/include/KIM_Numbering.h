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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
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

struct KIM_Numbering
{
  int numberingID;
};
#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
typedef struct KIM_Numbering KIM_Numbering;
#endif

KIM_Numbering KIM_Numbering_FromString(char const * const str);

int KIM_Numbering_Equal(KIM_Numbering const left, KIM_Numbering const right);
int KIM_Numbering_NotEqual(KIM_Numbering const left, KIM_Numbering const right);
char const * KIM_Numbering_String(KIM_Numbering const numbering);

extern KIM_Numbering const KIM_NUMBERING_zeroBased;
extern KIM_Numbering const KIM_NUMBERING_oneBased;

void KIM_NUMBERING_GetNumberOfNumberings(int * const numberOfNumberings);
int KIM_NUMBERING_GetNumbering(int const index,
                               KIM_Numbering * const numbering);

#endif  /* KIM_NUMBERING_H_ */
