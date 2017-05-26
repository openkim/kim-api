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

#ifndef KIM_UTILITY_COMPUTE_H_
#include "KIM_UTILITY_Compute.h"
#endif

#ifndef KIM_COMPUTE_H_
#include "KIM_Compute.h"
#endif

#ifndef KIM_MODEL_H_
#include "KIM_Model.h"
#endif
#define TRUE 1

int KIM_UTILITY_COMPUTE_getm_data(
    KIM_Model const * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...)
{
  va_list argp;
  int err;
  va_start(argp, argumentName);
  err = KIM_UTILITY_COMPUTE_vgetm_data(model, argumentName, argp);
  va_end(argp);
  return err;
}

int KIM_UTILITY_COMPUTE_vgetm_data(
    KIM_Model const * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp)
{
  int err = TRUE;
  KIM_COMPUTE_ArgumentName argN = argumentName;

  while (argN.argumentID != KIM_COMPUTE_ARGUMENT_NAME_End.argumentID)
  {
    void ** ptr = va_arg(argp, void **);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = KIM_Model_get_data(model, argN, ptr);

      if (err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM_COMPUTE_ArgumentName);
  }

  return err;
}

int KIM_UTILITY_COMPUTE_setm_data(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...)
{
  va_list argp;
  int err;
  va_start(argp, argumentName);
  err = KIM_UTILITY_COMPUTE_vsetm_data(model, argumentName, argp);
  va_end(argp);
  return err;
}

int KIM_UTILITY_COMPUTE_vsetm_data(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp)
{
  int err = TRUE;
  KIM_COMPUTE_ArgumentName argN = argumentName;

  while (argN.argumentID != KIM_COMPUTE_ARGUMENT_NAME_End.argumentID)
  {
    int extent = va_arg(argp, int);
    void * ptr = va_arg(argp, void *);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = KIM_Model_set_data(model, argN, extent, ptr);

      if (err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM_COMPUTE_ArgumentName);
  }

  return err;
}

int KIM_UTILITY_COMPUTE_getm_method(
    KIM_Model const * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...)
{
  va_list argp;
  int err;
  va_start(argp, argumentName);
  err = KIM_UTILITY_COMPUTE_vgetm_method(model, argumentName, argp);
  va_end(argp);
  return err;
}

int KIM_UTILITY_COMPUTE_vgetm_method(
    KIM_Model const * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp)
{
  int err = TRUE;
  KIM_COMPUTE_ArgumentName argN = argumentName;

  while (argN.argumentID != KIM_COMPUTE_ARGUMENT_NAME_End.argumentID)
  {
    KIM_COMPUTE_LanguageName * const langN
        = va_arg(argp, KIM_COMPUTE_LanguageName *);
    func ** fptr = va_arg(argp, func **);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = KIM_Model_get_method(model, argN, langN, fptr);

      if (err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM_COMPUTE_ArgumentName);
  }

  return err;
}

int KIM_UTILITY_COMPUTE_setm_method(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...)
{
  va_list argp;
  int err;
  va_start(argp, argumentName);
  err = KIM_UTILITY_COMPUTE_vsetm_method(model, argumentName, argp);
  va_end(argp);
  return err;
}

int KIM_UTILITY_COMPUTE_vsetm_method(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp)
{
  int err = TRUE;
  KIM_COMPUTE_ArgumentName argN = argumentName;

  while (argN.argumentID != KIM_COMPUTE_ARGUMENT_NAME_End.argumentID)
  {
    int extent = va_arg(argp, int);
    KIM_COMPUTE_LanguageName langN = va_arg(argp, KIM_COMPUTE_LanguageName);
    func * fptr = va_arg(argp, func *);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = KIM_Model_set_method(model, argN, extent, langN, fptr);

      if (err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM_COMPUTE_ArgumentName);
  }

  return err;
}

int KIM_UTILITY_COMPUTE_getm_compute(
    KIM_Model const * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...)
{
  va_list argp;
  int err;
  va_start(argp, argumentName);
  err = KIM_UTILITY_COMPUTE_vgetm_compute(model, argumentName, argp);
  va_end(argp);
  return err;
}

int KIM_UTILITY_COMPUTE_vgetm_compute(
    KIM_Model const * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp)
{
  int err = TRUE;
  KIM_COMPUTE_ArgumentName argN = argumentName;

  while (argN.argumentID != KIM_COMPUTE_ARGUMENT_NAME_End.argumentID)
  {
    int * ptr = va_arg(argp, int *);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = KIM_Model_get_compute(model, argN, ptr);

      if (err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM_COMPUTE_ArgumentName);
  }

  return err;
}

int KIM_UTILITY_COMPUTE_setm_compute(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...)
{
  va_list argp;
  int err;
  va_start(argp, argumentName);
  err = KIM_UTILITY_COMPUTE_vsetm_compute(model, argumentName, argp);
  va_end(argp);
  return err;
}

int KIM_UTILITY_COMPUTE_vsetm_compute(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp)
{
  int err = TRUE;
  KIM_COMPUTE_ArgumentName argN = argumentName;

  while (argN.argumentID != KIM_COMPUTE_ARGUMENT_NAME_End.argumentID)
  {
    int comp = va_arg(argp, int);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = KIM_Model_set_compute(model, argN, comp);

      if (err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM_COMPUTE_ArgumentName);
  }

  return err;
}
