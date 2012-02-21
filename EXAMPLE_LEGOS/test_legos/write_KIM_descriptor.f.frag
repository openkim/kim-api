#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
#
# Contributors:
#    Ellad B. Tadmor
#

#
# Release: This file is part of the openkim-api.git repository.
#


!-------------------------------------------------------------------------------
!
!  Write KIM descriptor file for MiniMol
!  (Also returns the number and identities of the species supported by
!  the Model.)
!
!-------------------------------------------------------------------------------
subroutine Write_KIM_descriptor(modelname, kim_descriptor, &
                                max_specs, model_specs, num_specs, ier)
use KIM_API
implicit none

!-- Transferred variables
character(len=80),    intent(in)   :: modelname
character(len=10000), intent(out)  :: kim_descriptor
integer,              intent(in)   :: max_specs
character(len=3),     intent(out)  :: model_specs(max_specs)
integer,              intent(out)  :: num_specs
integer,              intent(out)  :: ier

!-- Local variables
integer :: i, lastend, firstcap, secondcap
character(len=103) :: divider
character(len=1)   :: cr
character(len=52)  :: type_line
character(len=24)  :: spec24

! Initialize error flag
ier = KIM_STATUS_OK

! Figure out which specs are supported by the model from its name
! (This is not a safe approach since the model name format is not
! enforced. Should be changed to a call to KIM service routines to
! query the model about the species it supports.)
! find first underscore (element string should start right after this)
lastend = scan(modelname,"_")
if (lastend.eq.0) then
   ier = KIM_STATUS_PARTICLE_INVALID_TYPE
   return
endif
num_specs = 0
do
   ! find first capital or (underscore following element string) since last end
   firstcap = scan(modelname(lastend+1:),"ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
   if (firstcap.ne.1) then
      ier = KIM_STATUS_PARTICLE_INVALID_TYPE
      return
   endif
   firstcap = lastend + firstcap
   ! find second capital or (underscore following element string) since last end
   secondcap = scan(modelname(firstcap+1:),"ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
   if (secondcap.eq.0) then
      ier = KIM_STATUS_PARTICLE_INVALID_TYPE
      return
   endif
   secondcap = firstcap + secondcap
   ! pull out element and store in model_specs()
   num_specs = num_specs + 1
   if (num_specs.gt.max_specs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_specs(num_specs) = modelname(firstcap:secondcap-1)
   if (modelname(secondcap:secondcap).eq."_") exit
   lastend = secondcap-1
enddo

! Define frequently used variables
!
cr = char(10)
divider = '#######################################################################################################'

! Write Minimol descriptor file into string kim_descriptor
!
kim_descriptor = &
   divider                                                                      // cr // &
   '#'                                                                          // cr // &
   '# CDDL HEADER START'                                                        // cr // &
   '#'                                                                          // cr // &
   '# The contents of this file are subject to the terms of the Common Development' // cr // &
   '# and Distribution License Version 1.0 (the "License").'                    // cr // &
   '#'                                                                          // cr // &
   '# You can obtain a copy of the license at'                                  // cr // &
   '# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the'    // cr // &
   '# specific language governing permissions and limitations under the License.' // cr // &
   '#'                                                                          // cr // &
   '# When distributing Covered Code, include this CDDL HEADER in each file and'// cr // &
   '# include the License file in a prominent location with the name LICENSE.CDDL.' // cr // &
   '# If applicable, add the following below this CDDL HEADER, with the fields' // cr // &
   '# enclosed by brackets "[]" replaced with your own identifying information:'// cr // &
   '#'                                                                          // cr // &
   '# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.' // cr // &
   '#'                                                                          // cr // &
   '# CDDL HEADER END'                                                          // cr // &
   '#'                                                                          // cr // &
                                                                                   cr // &
   '#'                                                                          // cr // &
   '# Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.' // cr // &
   '#'                                                                          // cr // &
   '# Contributors:'                                                            // cr // &
   '#    Automatically generated by calling Test'                               // cr // &
   '#'                                                                          // cr // &
                                                                                   cr // &
   '#'                                                                          // cr // &
   '# See KIM_API/standard.kim for documentation about this file'               // cr // &
   '#'                                                                          // cr // &
   divider                                                                      // cr // &
                                                                                   cr // &
                                                                                   cr // &
   'TEST_NAME := TEST_NAME_STR'                                                 // cr // &
   'Unit_length      := A'                                                      // cr // &
   'Unit_energy      := eV'                                                     // cr // &
   'Unit_charge      := e'                                                      // cr // &
   'Unit_temperature := K'                                                      // cr // &
   'Unit_time        := fs'                                                     // cr // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'SUPPORTED_ATOM/PARTICLES_TYPES:'                                            // cr // &
   '# Symbol/name               Type                    code'                   // cr

do i = 1,num_specs
   spec24 = model_specs(i)
   write(type_line,'(a24,''spec'',20x,i4)') spec24,0
   kim_descriptor = trim(kim_descriptor) // type_line // cr
enddo

kim_descriptor = trim(kim_descriptor) // & 
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'CONVENTIONS:'                                                               // cr // &
   '# Name                      Type'                                           // cr // &
                                                                                   cr // &
   'OneBasedLists               flag'                                           // cr // &
                                                                                   cr // &
   'Neigh_BothAccess            flag'                                           // cr // &
                                                                                   cr // &
   'NEIGH_PURE_H                flag'                                           // cr // &
                                                                                   cr // &
   'NEIGH_PURE_F                flag'                                           // cr // &
                                                                                   cr // &
   'NEIGH_RVEC_F                flag'                                           // cr // &
                                                                                   cr // &
   'MI_OPBC_H                   flag'                                           // cr // &
                                                                                   cr // &
   'MI_OPBC_F                   flag'                                           // cr // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'MODEL_INPUT:'                                                               // cr // &
   '# Name                      Type         Unit       Shape              requirements' // cr // &
   'numberOfParticles           integer      none       []'                     // cr // &
                                                                                   cr // &
   'numberParticleTypes         integer      none       []'                     // cr // &
                                                                                   cr // &
   'particleTypes               integer      none       [numberOfParticles]'    // cr // &
                                                                                   cr // &
   'coordinates                 real*8       length     [numberOfParticles,3]'  // cr // &
                                                                                   cr // &
   'get_neigh                   method       none       []'                     // cr // &
                                                                                   cr // &
   'neighObject                 pointer      none       []'                     // cr // &
                                                                                   cr // &
   'numberContributingParticles integer      none       []'                     // cr // &
                                                                                   cr // &
   'boxSideLengths              real*8       length     [3]'                    // cr // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'MODEL_OUTPUT:'                                                              // cr // &
   '# Name                      Type         Unit       Shape              requirements' // cr // &
                                                                                   cr // &
   'destroy                     method       none       []'                     // cr // &
                                                                                   cr // &
   'compute                     method       none       []'                     // cr // &
                                                                                   cr // &
   'reinit                      method       none       []'                     // cr // &
                                                                                   cr // &
   'cutoff                      real*8       length     []'                     // cr // &
                                                                                   cr // &
   'energy                      real*8       energy     []'                     // cr // &
                                                                                   cr // &
   'forces                      real*8       force      [numberOfParticles,3]'  // cr // &
                                                                                   cr // &
   divider                                                                            // cr

return

end subroutine Write_KIM_descriptor
