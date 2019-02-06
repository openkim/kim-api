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
/* Release: This file is part of the kim-api-v2-2.0.0 package.                */
/*                                                                            */


#ifndef KIM_SEM_VER_H_
#define KIM_SEM_VER_H_

/**
 ** \brief \copybrief KIM::SEM_VER::GetSemVer
 **
 ** \sa KIM::SEM_VER::GetSemVer, kim_sem_ver_module::kim_get_sem_ver
 **
 ** \since 2.0
 **/
char const * KIM_SEM_VER_GetSemVer();

/**
 ** \brief \copybrief KIM::SEM_VER::IsLessThan
 **
 ** \sa KIM::SEM_VER::IsLessThan, kim_sem_ver_module::kim_is_less_than
 **
 ** \since 2.0
 **/
int KIM_SEM_VER_IsLessThan(char const * const lhs,
                           char const * const rhs,
                           int * const isLessThan);

/**
 ** \brief \copybrief KIM::SEM_VER::ParseSemVer
 **
 ** \sa KIM::SEM_VER::ParseSemVer, kim_sem_ver_module::kim_parse_sem_ver
 **
 ** \since 2.0
 **/
int KIM_SEM_VER_ParseSemVer(char const * const version,
                            int const prereleaseLength,
                            int const buildMetadataLength,
                            int * const major,
                            int * const minor,
                            int * const patch,
                            char * const prerelease,
                            char * const buildMetadata);

#endif /* KIM_SEM_VER_H_ */
