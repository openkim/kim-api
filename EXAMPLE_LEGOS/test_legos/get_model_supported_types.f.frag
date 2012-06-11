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
!  Get number and identities of particle types (species) supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Types(modelname, max_types, model_types, &
                                     num_types, ier)
use KIM_API
implicit none

!-- Transferred variables
character*80,             intent(in)   :: modelname
integer,                  intent(in)   :: max_types
character(len=3),         intent(out)  :: model_types(max_types)
integer,                  intent(out)  :: num_types
integer,                  intent(out)  :: ier

!-- Local variables
integer :: i

!-- KIM variables
character(len=KIM_KEY_STRING_LENGTH) types(1); pointer(ptypes,types)
integer(kind=kim_intptr) pkim

! Initialize error flag
ier = KIM_STATUS_OK

! Generate empty KIM object containing generic model info
ier = kim_api_model_info_f(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Get species supported by the model
ptypes = kim_api_get_model_partcl_typs_f(pkim,num_types,ier)
if (ier.lt.KIM_STATUS_OK) return
if (num_types.gt.max_types) then
   ier = KIM_STATUS_FAIL
   return
endif
do i=1,num_types
   model_types(i) = types(i)(1:scan(types(i),char(0))-1)
enddo

! free temporary storage
call free(ptypes)
call kim_api_free_f(pkim,ier)

return

end subroutine Get_Model_Supported_Types
