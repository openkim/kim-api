!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!
!
!
! Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!
!
!
! Release: This file is part of the openkim-api.git repository.
!
!
!-------------------------------------------------------------------------------
!
!  Write KIM descriptor file for MiniMol
!  (Also returns the number and identities of the species supported by
!  the Model.)
!
!-------------------------------------------------------------------------------
subroutine Get_Model_NBC_methods(modelname, max_NBCs, model_NBCs, num_NBCs, ier)
use KIM_API
implicit none

!-- Transferred variables
character*80,                         intent(in)   :: modelname
integer,                              intent(in)   :: max_NBCs
character(len=KIM_KEY_STRING_LENGTH), intent(out)  :: model_NBCs(max_NBCs)
integer,                              intent(out)  :: num_NBCs
integer,                              intent(out)  :: ier

!-- Local variables
integer :: i, index

!-- KIM variables
integer(kind=kim_intptr) pkim

! Initialize error flag
ier = KIM_STATUS_OK

! Generate empty KIM object containing generic model info
ier = kim_api_model_info_f(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Identify supported NBCs by seeking index to each of the NBCs in the KIM model
num_NBCs = 0

! CLUSTER
index = kim_api_get_index_f(pkim, "CLUSTER", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "CLUSTER"
endif

! NEIGH_PURE_H
index = kim_api_get_index_f(pkim, "NEIGH_PURE_H", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_PURE_H"
endif

! NEIGH_PURE_F
index = kim_api_get_index_f(pkim, "NEIGH_PURE_F", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_PURE_F"
endif

! NEIGH_RVEC_F
index = kim_api_get_index_f(pkim, "NEIGH_RVEC_F", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_RVEC_F"
endif

! MI_OPBC_H
index = kim_api_get_index_f(pkim, "MI_OPBC_H", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "MI_OPBC_H"
endif

! MI_OPBC_F
index = kim_api_get_index_f(pkim, "MI_OPBC_F", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "MI_OPBC_F"
endif

! free temporary storage
call kim_api_free_f(pkim,ier)

return

end subroutine Get_Model_NBC_methods
