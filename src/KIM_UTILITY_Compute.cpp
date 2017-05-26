//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_UTILITY_COMPUTE_HPP_
#include "KIM_UTILITY_Compute.hpp"
#endif

#ifndef KIM_COMPUTE_HPP_
#include "KIM_Compute.hpp"
#endif

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif

namespace KIM
{
namespace UTILITY
{
namespace COMPUTE
{

int getm_data(Model const * const model,
              KIM::COMPUTE::ArgumentName const argumentName, ...)
{
  va_list argp;
  va_start(argp, argumentName);
  int err = vgetm_data(model, argumentName, argp);
  va_end(argp);
  return err;
}

int vgetm_data(Model const * const model,
               KIM::COMPUTE::ArgumentName const argumentName, va_list argp)
{
  int err = KIM_STATUS_OK;
  KIM::COMPUTE::ArgumentName argN = argumentName;

  while (argN != KIM::COMPUTE::ARGUMENT_NAME::End)
  {
    void ** ptr = va_arg(argp, void **);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = model->get_data(argN, ptr);

      if (KIM_STATUS_OK > err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM::COMPUTE::ArgumentName);
  }

  return err;
}

int setm_data(Model * const model,
              KIM::COMPUTE::ArgumentName const argumentName, ...)
{
  va_list argp;
  va_start(argp, argumentName);
  int err = vsetm_data(model, argumentName, argp);
  va_end(argp);
  return err;
}

int vsetm_data(Model * const model,
               KIM::COMPUTE::ArgumentName const argumentName, va_list argp)
{
  int err = KIM_STATUS_OK;
  KIM::COMPUTE::ArgumentName argN = argumentName;

  while (argN != KIM::COMPUTE::ARGUMENT_NAME::End)
  {
    int extent = va_arg(argp, int);
    void * ptr = va_arg(argp, void *);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = model->set_data(argN, extent, ptr);

      if (KIM_STATUS_OK > err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM::COMPUTE::ArgumentName);
  }

  return err;
}

int getm_method(Model const * const model,
                KIM::COMPUTE::ArgumentName const argumentName, ...)
{
  va_list argp;
  va_start(argp, argumentName);
  int err = vgetm_method(model, argumentName, argp);
  va_end(argp);
  return err;
}

int vgetm_method(Model const * const model,
                 KIM::COMPUTE::ArgumentName const argumentName, va_list argp)
{
  int err = KIM_STATUS_OK;
  KIM::COMPUTE::ArgumentName argN = argumentName;

  while (argN != KIM::COMPUTE::ARGUMENT_NAME::End)
  {
    KIM::COMPUTE::LanguageName * const
        langN = va_arg(argp, KIM::COMPUTE::LanguageName *);
    func ** fptr = va_arg(argp, func **);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = model->get_method(argN, langN, fptr);

      if (KIM_STATUS_OK > err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM::COMPUTE::ArgumentName);
  }

  return err;
}

int setm_method(Model * const model,
                KIM::COMPUTE::ArgumentName const argumentName, ...)
{
  va_list argp;
  va_start(argp, argumentName);
  int err = vsetm_method(model, argumentName, argp);
  va_end(argp);
  return err;
}

int vsetm_method(Model * const model,
                 KIM::COMPUTE::ArgumentName const argumentName, va_list argp)
{
  int err = KIM_STATUS_OK;
  KIM::COMPUTE::ArgumentName argN = argumentName;

  while (argN != KIM::COMPUTE::ARGUMENT_NAME::End)
  {
    int extent = va_arg(argp, int);
    int langN = va_arg(argp, int);
    func * ptr = va_arg(argp, func *);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = model->set_method(argN, extent, langN, ptr);

      if (KIM_STATUS_OK > err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM::COMPUTE::ArgumentName);
  }

  return err;
}

int getm_compute(Model const * const model,
                 KIM::COMPUTE::ArgumentName const argumentName, ...)
{
  va_list argp;
  va_start(argp, argumentName);
  int err = vgetm_compute(model, argumentName, argp);
  va_end(argp);
  return err;
}

int vgetm_compute(Model const * const model,
                  KIM::COMPUTE::ArgumentName const argumentName, va_list argp)
{
  int err = KIM_STATUS_OK;
  KIM::COMPUTE::ArgumentName argN = argumentName;

  while (argN != KIM::COMPUTE::ARGUMENT_NAME::End)
  {
    int * ptr = va_arg(argp, int *);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = model->get_compute(argN, ptr);

      if (KIM_STATUS_OK > err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM::COMPUTE::ArgumentName);
  }

  return err;
}

int setm_compute(Model * const model,
                 KIM::COMPUTE::ArgumentName const argumentName, ...)
{
  va_list argp;
  va_start(argp, argumentName);
  int err = vsetm_compute(model, argumentName, argp);
  va_end(argp);
  return err;
}

int vsetm_compute(Model * const model,
                  KIM::COMPUTE::ArgumentName const argumentName, va_list argp)
{
  int err = KIM_STATUS_OK;
  KIM::COMPUTE::ArgumentName argN = argumentName;

  while (argN != KIM::COMPUTE::ARGUMENT_NAME::End)
  {
    int ptr = va_arg(argp, int);
    int flag = va_arg(argp, int);

    if (flag)
    {
      err = model->set_compute(argN, ptr);

      if (KIM_STATUS_OK > err)
      {
        break;
      }
    }

    argN = va_arg(argp, KIM::COMPUTE::ArgumentName);
  }

  return err;
}

}  // namespace COMPUTE
}  // namespace UTILITY
}  // namespace KIM
