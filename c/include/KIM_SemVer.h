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
