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
/* Copyright (c) 2016--2017, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_LANGUAGE_NAME_H_
#define KIM_LANGUAGE_NAME_H_

struct KIM_LanguageName
{
  int languageNameID;
};
#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

int KIM_LanguageNameEqual(KIM_LanguageName const left,
                          KIM_LanguageName const right);
int KIM_LanguageNameNotEqual(KIM_LanguageName const left,
                             KIM_LanguageName const right);
char const * const KIM_LanguageNameString(KIM_LanguageName const languageName);

extern KIM_LanguageName const KIM_LANGUAGE_NAME_Cpp;
extern KIM_LanguageName const KIM_LANGUAGE_NAME_C;
extern KIM_LanguageName const KIM_LANGUAGE_NAME_Fortran;

#endif  /* KIM_LANGUAGE_NAME_H_ */
