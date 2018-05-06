//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common
// Development and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name
// LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner].
// All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


namespace
{
struct Handle
{
  void * p;
};
typedef struct Handle Handle;
}  // namespace

extern "C"
{
extern Handle const KIM_MODEL_COMPUTE_null_handle = {0};
extern Handle const KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_null_handle = {0};
extern Handle const KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_null_handle = {0};
extern Handle const KIM_MODEL_CREATE_null_handle = {0};
extern Handle const KIM_MODEL_DESTROY_null_handle = {0};
extern Handle const KIM_MODEL_DRIVER_CREATE_null_handle = {0};
extern Handle const KIM_MODEL_null_handle = {0};
extern Handle const KIM_COMPUTE_ARGUMENTS_null_handle = {0};
extern Handle const KIM_MODEL_REFRESH_null_handle = {0};
}  // extern "C"
