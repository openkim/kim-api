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
! Copyright (c) 2013, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
!    Stephen M. Whalen
!

!****************************************************************************
!**
!**  MODULE MODEL_DRIVER_NAME_STR
!**
!**  Lennard-Jones pair potential KIM Model Driver
!**  shifted to have zero energy at the cutoff radius
!**
!**  Language: Fortran 90
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module MODEL_DRIVER_NAME_LC_STR

use KIM_API
implicit none

save
private
public Compute_Energy_Forces, &
       reinit,                &
       destroy,               &
       calc_phi,              &
       calc_phi_dphi,         &
       calc_phi_dphi_d2phi

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code

contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(model_epsilon,  &
                    model_sigma,    &
                    model_shift,    &
                    model_cutoff,r,phi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: model_epsilon
double precision, intent(in)  :: model_sigma
double precision, intent(in)  :: model_shift
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi

!-- Local variables
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
else
   phi = 4.d0*model_epsilon*(sor12-sor6) + model_shift
endif

end subroutine calc_phi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivative dphi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi(model_epsilon,  &
                         model_sigma,    &
                         model_shift,    &
                         model_cutoff,r,phi,dphi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: model_epsilon
double precision, intent(in)  :: model_sigma
double precision, intent(in)  :: model_shift
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi

!-- Local variables
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
else
   phi  = 4.d0*model_epsilon*(sor12-sor6) + model_shift
   dphi = 24.d0*model_epsilon*(-2.d0*sor12+sor6)/r
endif

end subroutine calc_phi_dphi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivatives dphi(r) and d2phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi_d2phi(model_epsilon,  &
                               model_sigma,    &
                               model_shift,    &
                               model_cutoff,r,phi,dphi,d2phi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: model_epsilon
double precision, intent(in)  :: model_sigma
double precision, intent(in)  :: model_shift
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi,d2phi

!-- Local variables
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
   d2phi  = 0.d0
else
   phi   = 4.d0*model_epsilon*(sor12-sor6) + model_shift
   dphi  = 24.d0*model_epsilon*(-2.d0*sor12+sor6)/r
   d2phi = 24.d0*model_epsilon*(26.d0*sor12-7.d0*sor6)/rsq
endif

end subroutine calc_phi_dphi_d2phi

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
integer function Compute_Energy_Forces(pkim)
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in)  :: pkim

!-- Local variables
double precision :: Rij(DIM),Rij_pairs(DIM,2)
double precision :: r,Rsqij,phi,dphi,d2phi,dEidr,d2Eidr
double precision :: r_pairs(2)
integer :: i,j,jj,numnei,atom_ret
integer :: i_pairs(2), j_pairs(2)
integer :: comp_force,comp_energy,comp_enepot,comp_process_dEdr,comp_process_d2Edr2
integer, allocatable, target :: nei1atom_substitute(:)
integer :: idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
double precision  bufparam(1);        pointer(pbufparam, bufparam)

!-- KIM variables
double precision  model_cutoff;       pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
double precision  model_cutsq                                               ! cutoff radius squared
double precision  model_epsilon
double precision  model_sigma
double precision  model_shift
integer N;                    pointer(pN,N)
double precision  energy;               pointer(penergy,energy)
double precision  coordum(DIM,1);       pointer(pcoor,coordum)
double precision  forcedum(DIM,1);      pointer(pforce,forcedum)
double precision  enepotdum(1);         pointer(penepot,enepotdum)
double precision  boxSideLengths(DIM);  pointer(pboxSideLengths,boxSideLengths)
double precision  Rij_list(DIM,1);      pointer(pRij_list,Rij_list)
integer numContrib;                     pointer(pnumContrib,numContrib)
integer nei1atom(1);                    pointer(pnei1atom,nei1atom)
integer particleTypes(1);               pointer(pparticleTypes,particleTypes)
double precision, pointer :: coor(:,:),force(:,:),ene_pot(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib
integer energy_ind
integer forces_ind
integer particleEnergy_ind
integer process_dEdr_ind
integer process_d2Edr2_ind
integer model_index_shift
integer numberOfParticles_ind
integer particleTypes_ind
integer coordinates_ind
integer numberContributingParticles_ind
integer boxSideLengths_ind
integer get_neigh_ind
integer cutoff_ind

numberContrib = 0 ! initialize

! get model buffer from KIM object
pbuffer = kim_api_get_model_buffer_f(pkim, Compute_Energy_Forces)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_model_buffer_f", Compute_Energy_Forces)
   return
endif
pbufind   = buffer(1)
pbufparam = buffer(2)

! Unpack indices from buffer
!
NBC                             = bufind(1)
HalfOrFull                      = bufind(2)
IterOrLoca                      = bufind(3)
energy_ind                      = bufind(4)
forces_ind                      = bufind(5)
particleEnergy_ind              = bufind(6)
process_dEdr_ind                = bufind(7)
process_d2Edr2_ind              = bufind(8)
model_index_shift               = bufind(9)
numberOfParticles_ind           = bufind(10)
particleTypes_ind               = bufind(11)
coordinates_ind                 = bufind(12)
numberContributingParticles_ind = bufind(13)
boxSideLengths_ind              = bufind(14)
get_neigh_ind                   = bufind(15)
cutoff_ind                      = bufind(16)

! Unpack Model's parameters from buffer
!
model_cutsq   = bufparam(2)
model_epsilon = bufparam(3)
model_sigma   = bufparam(4)
model_shift   = bufparam(5)

! Unpack the Model's cutoff stored in the KIM API object
!
pmodel_cutoff = kim_api_get_data_by_index_f(pkim, cutoff_ind, Compute_Energy_Forces)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data_by_index_f", Compute_Energy_Forces)
   return
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and d1Edr
!
call kim_api_getm_compute_by_index_f(pkim, Compute_Energy_Forces, &
     energy_ind,         comp_energy,         1, &
     forces_ind,         comp_force,          1, &
     particleEnergy_ind, comp_enepot,         1, &
     process_dEdr_ind,   comp_process_dEdr,   1, &
     process_d2Edr2_ind, comp_process_d2Edr2, 1)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_compute_by_index_f", Compute_Energy_Forces)
   return
endif

! Unpack data from KIM object
!
call kim_api_getm_data_by_index_f(pkim, Compute_Energy_Forces, &
     numberOfParticles_ind,           pN,              1,                           &
     particleTypes_ind,               pparticleTypes,  1,                           &
     coordinates_ind,                 pcoor,           1,                           &
     numberContributingParticles_ind, pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  &
     boxSideLengths_ind,              pboxSideLengths, TRUEFALSE(NBC.eq.2),         &
     energy_ind,                      penergy,         TRUEFALSE(comp_energy.eq.1), &
     forces_ind,                      pforce,          TRUEFALSE(comp_force.eq.1),  &
     particleEnergy_ind,              penepot,         TRUEFALSE(comp_enepot.eq.1))
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data_by_index_f", Compute_Energy_Forces)
   return
endif

call KIM_to_F90_real_array_2d(coordum,coor,DIM,N)
if (comp_force.eq.1)  call KIM_to_F90_real_array_2d(forcedum,force,DIM,N)
if (comp_enepot.eq.1) call KIM_to_F90_real_array_1d(enepotdum,ene_pot,N)
if (HalfOrFull.eq.1) then
   if (NBC.ne.3) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER case
      numberContrib = N
   endif
endif

! Check to be sure that the atom types are correct
!
Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (particleTypes(i).ne.speccode) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "Unexpected species type detected", Compute_Energy_Forces)
      return
   endif
enddo
Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces
!
if (comp_enepot.eq.1) ene_pot(1:N) = 0.d0
if (comp_energy.eq.1) energy = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.3) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

! Initialize neighbor handling for Iterator mode
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh_f(pkim,0,0,atom_ret,numnei,pnei1atom,pRij_list)
   ! check for successful initialization
   if (Compute_Energy_Forces.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", Compute_Energy_Forces)
      Compute_Energy_Forces = KIM_STATUS_FAIL
      return
   endif
endif

!
!  Compute energy and forces
!

!  Loop over particles and compute energy and forces
!
i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   if (IterOrLoca.eq.1) then    ! ITERATOR mode
      Compute_Energy_Forces = kim_api_get_neigh_f(pkim,0,1,atom_ret,numnei,pnei1atom,pRij_list)
      if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
                                ! incremented past the end of the list,
                                ! terminate loop
      if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then ! some sort of problem, exit
         idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_get_neigh_f", Compute_Energy_Forces)
         return
      endif

      i = atom_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (NBC.eq.3) then        ! CLUSTER NBC method
         numnei = N - i         ! number of neighbors in list i+1, ..., N
         nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
         Compute_Energy_Forces = KIM_STATUS_OK
      else                      ! All other NBCs
         Compute_Energy_Forces = kim_api_get_neigh_f(pkim,1,i,atom_ret,numnei,pnei1atom,pRij_list)
         if (Compute_Energy_Forces.ne.KIM_STATUS_OK) then ! some sort of problem, exit
            idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                          "kim_api_get_neigh_f", Compute_Energy_Forces)
            Compute_Energy_Forces = KIM_STATUS_FAIL
            return
         endif
      endif
   endif

   ! Loop over the neighbors of atom i
   !
   do jj = 1, numnei

      j = nei1atom(jj)                            ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.0) then                          ! all methods except NEIGH_RVEC
         Rij(:) = coor(:,j) - coor(:,i)           ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.2) then
         where ( abs(Rij) .gt. 0.5d0*boxSideLengths )  ! periodic boundary conditions
            Rij = Rij - sign(boxSideLengths,Rij)       ! applied where needed.
         end where                                !
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?

         r = sqrt(Rsqij)                          ! compute distance
         if (comp_process_d2Edr2.eq.1) then
            call calc_phi_dphi_d2phi(model_epsilon, &
                                     model_sigma,   &
                                     model_shift,   &
                                     model_cutoff,  &
                                     r,phi,dphi,d2phi) ! compute pair potential
                                                       !   and it derivatives
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr  = dphi                      !      double contribution
               d2Eidr = d2phi
            else                                  ! FULL mode
               dEidr  = 0.5d0*dphi                !      regular contribution
               d2Eidr = 0.5d0*d2phi
            endif
         elseif (comp_force.eq.1.or.comp_process_dEdr.eq.1) then
            call calc_phi_dphi(model_epsilon, &
                               model_sigma,   &
                               model_shift,   &
                               model_cutoff,  &
                               r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative

            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr = dphi                       !      double contribution
            else                                  ! FULL mode
               dEidr = 0.5d0*dphi                 !      regular contribution
            endif
         else
            call calc_phi(model_epsilon, &
                          model_sigma,   &
                          model_shift,   &
                          model_cutoff,  &
                          r,phi)                  ! compute just pair potential
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + 0.5d0*phi   ! accumulate energy
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) &         ! HALF mode
               ene_pot(j) = ene_pot(j) + 0.5d0*phi! (i and j share it)
         endif
         if (comp_energy.eq.1) then
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               energy = energy + phi              !      add v to total energy
            else                                  ! FULL mode
               energy = energy + 0.5d0*phi        !      add half v to total energy
            endif
         endif

         ! contribution to process_dEdr
         !
         if (comp_process_dEdr.eq.1) then
            Compute_Energy_Forces = kim_api_process_dEdr_f(pkim, dEidr, r, loc(Rij), i, j)
         endif

         ! contribution to process_d2Edr2
         if (comp_process_d2Edr2.eq.1) then
            r_pairs(1) = r
            r_pairs(2) = r
            Rij_pairs(:,1) = Rij
            Rij_pairs(:,2) = Rij
            i_pairs(1) = i
            i_pairs(2) = i
            j_pairs(1) = j
            j_pairs(2) = j

            Compute_Energy_Forces = kim_api_process_d2Edr2_f(pkim, d2Eidr, loc(r_pairs), &
                                    loc(Rij_pairs), loc(i_pairs), loc(j_pairs))
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then
            force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on atom i
            force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on atom j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Free temporary storage
!
if (NBC.eq.3) deallocate( nei1atom_substitute )

! Everything is great
!
Compute_Energy_Forces = KIM_STATUS_OK
return

end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model driver reinitialization routine
!
!-------------------------------------------------------------------------------
integer function reinit(pkim)
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
double precision energy_at_cutoff
integer idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
double precision  bufparam(1);        pointer(pbufparam, bufparam)

!-- KIM variables
double precision  cutoff; pointer(pcutoff,cutoff)
double precision  model_cutoff
double precision  model_epsilon
double precision  model_sigma

! get model buffer from KIM object
pbuffer = kim_api_get_model_buffer_f(pkim, reinit)
if (reinit.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_model_buffer_f", reinit)
   return
endif
pbufind   = buffer(1)
pbufparam = buffer(2)

! get updated values of PARAM_FREE_*
model_cutoff  = bufparam(1)
model_epsilon = bufparam(3)
model_sigma   = bufparam(4)

pcutoff = kim_api_get_data_by_index_f(pkim, bufind(16), reinit)
if (reinit.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data_by_index_f", reinit)
   return
endif

!
! Set new values in KIM object and buffer
!
cutoff      = model_cutoff
bufparam(2) = model_cutoff**2
! calculate pair potential at r=cutoff with shift=0.0
call calc_phi(model_epsilon, &
              model_sigma,   &
              0.d0,          &
              model_cutoff,  &
              model_cutoff,energy_at_cutoff)
bufparam(5) = -energy_at_cutoff

reinit = KIM_STATUS_OK
return

end function reinit

!-------------------------------------------------------------------------------
!
! Model driver destroy routine
!
!-------------------------------------------------------------------------------
integer function destroy(pkim)
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
double precision  bufparam(1);        pointer(pbufparam, bufparam)

! get model buffer from KIM object
pbuffer = kim_api_get_model_buffer_f(pkim, destroy)
if (destroy.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_model_buffer_f", destroy)
   return
endif
pbufind   = buffer(1)
pbufparam = buffer(2)

call free(pbufparam)
call free(pbufind)

call free(pbuffer)

destroy = KIM_STATUS_OK
return

end function destroy

end module MODEL_DRIVER_NAME_LC_STR

!-------------------------------------------------------------------------------
!
! Model driver initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer function model_driver_init(pkim, byte_paramfile, nmstrlen, numparamfiles)
use MODEL_DRIVER_NAME_LC_STR
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim
integer,                  intent(in) :: nmstrlen
integer,                  intent(in) :: numparamfiles
byte,                     intent(in) :: byte_paramfile(nmstrlen*numparamfiles)

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
character(len=nmstrlen) paramfile_names(numparamfiles)
integer i,j,ier, idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
double precision  bufparam(1);        pointer(pbufparam, bufparam)
character (len=80) :: error_message
! define variables for all model parameters to be read in
double precision in_cutoff
double precision in_epsilon
double precision in_sigma
double precision energy_at_cutoff

!-- KIM variables
double precision  cutoff; pointer(pcutoff,cutoff)
character (len=64) NBC_Method;  pointer(pNBC_Method,NBC_Method)

!
! generic code to process model parameter file names from byte string
!
do i=1,numparamfiles
   paramfile_names(i) = "" ! initialize name to empty string
   do j=1,nmstrlen
      ! add characters to file name until a NULL is encountered
      if (char(byte_paramfile((i-1)*nmstrlen+j)) .eq. char(0)) exit
      paramfile_names(i)(j:j) = char(byte_paramfile((i-1)*nmstrlen+j))
   enddo
enddo
!
! end generic code to process model parameter file names
!

! store function pointers in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "compute", one, loc(Compute_Energy_Forces), 1, &
     "reinit",  one, loc(reinit),                1, &
     "destroy", one, loc(destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data_f", ier)
   goto 42
endif

! Read in model parameters from parameter file
!
open(10,file=paramfile_names(1),status="old")
read(10,*,iostat=ier,err=100) in_cutoff
read(10,*,iostat=ier,err=100) in_epsilon
read(10,*,iostat=ier,err=100) in_sigma
close(10)

goto 200
100 continue
! reading parameters failed
ier = KIM_STATUS_FAIL
idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                              "Unable to read LJ parameters, kimerror = ",ier)
goto 42

200 continue

! convert to appropriate units
in_cutoff = in_cutoff * kim_api_convert_to_act_unit_f(pkim, "A", "eV", "e", "K", "ps", &
                                                    1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                               "kim_api_convert_to_act_unit_f", ier)
   goto 42
endif

in_epsilon = in_epsilon * kim_api_convert_to_act_unit_f(pkim, "A", "eV", "e", "K", "ps", &
                                                      0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                               "kim_api_convert_to_act_unit_f", ier)
   goto 42
endif

in_sigma = in_sigma * kim_api_convert_to_act_unit_f(pkim, "A", "eV", "e", "K", "ps", &
                                                  1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                               "kim_api_convert_to_act_unit_f", ier)
   goto 42
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
   goto 42
endif
cutoff = in_cutoff

! allocate buffer
pbuffer   = malloc(2*kim_intptr)
!
! bufind values
!     1 - NBC
!     2 - HalfOrFull
!     3 - IterOrLoca
!     4 - energy_ind
!     5 - forces_ind
!     6 - particleEnergy_ind
!     7 - process_dEdr_ind
!     8 - process_d2Edr2_ind
!     9 - model_index_shift
!    10 - numberOfParticles_ind
!    11 - particleTypes_ind
!    12 - coordinates_ind
!    13 - numberContributingParticles_ind
!    14 - boxSideLengths_ind
!    15 - get_neigh_ind
!    16 - cutoff_ind
pbufind   = malloc(16*4)
!
! bufparam values
!     1 - Pcutoff
!     2 - cutsq
!     3 - epsilon
!     4 - sigma
!     5 - shift
pbufparam = malloc(5*8) ! 8 is size of double precision number
if (pbuffer.eq.0 .or. pbufind .eq. 0 .or. pbufparam.eq.0) then
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "malloc", ier);
   goto 42
endif
! set pointers
buffer(1) = pbufind
buffer(2) = pbufparam

! setup buffer
! set value of parameters
bufparam(1) = in_cutoff
bufparam(2) = in_cutoff**2
bufparam(3) = in_epsilon
bufparam(4) = in_sigma
call calc_phi(in_epsilon, &
              in_sigma,   &
              0.d0,       &
              in_cutoff,  &
              in_cutoff, energy_at_cutoff)
bufparam(5) = -energy_at_cutoff

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method_f(pkim, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method_f", ier)
   goto 42
endif
if (index(NBC_Method,"NEIGH_RVEC_H").eq.1) then
   bufind(1) = 0
   bufind(2) = 1
elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
   bufind(1) = 1
   bufind(2) = 1
elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
   bufind(1) = 0
   bufind(2) = 2
elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
   bufind(1) = 1
   bufind(2) = 2
elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then
   bufind(1) = 2
   bufind(2) = 1
elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
   bufind(1) = 2
   bufind(2) = 2
elseif (index(NBC_Method,"CLUSTER").eq.1) then
   bufind(1) = 3
   bufind(2) = 1
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", ier)
   goto 42
endif
call free(pNBC_Method) ! don't forget to release the memory...

! Determine neighbor list handling mode
!
if (bufind(1).ne.3) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   bufind(3) = kim_api_get_neigh_mode_f(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_mode_f", ier)
      goto 42
   endif
   if (bufind(3).ne.1 .and. bufind(3).ne.2) then
      ier = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',bufind(3)
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    error_message, ier)
      goto 42
   endif
else
   bufind(3) = 2   ! for CLUSTER NBC
endif

bufind(9) = kim_api_get_model_index_shift_f(pkim)

call kim_api_getm_index_f(pkim, ier, &
     "cutoff",                      bufind(16),  1, &
     "numberOfParticles",           bufind(10),  1, &
     "particleTypes",               bufind(11),  1, &
     "numberContributingParticles", bufind(13),  1, &
     "coordinates",                 bufind(12),  1, &
     "get_neigh",                   bufind(15),  1, &
     "boxSideLengths",              bufind(14),  1, &
     "energy",                      bufind(4),   1, &
     "forces",                      bufind(5),   1, &
     "particleEnergy",              bufind(6),   1, &
     "process_dEdr",                bufind(7),   1, &
     "process_d2Edr2",              bufind(8),   1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_index_f", ier)
   goto 42
endif
! end setup buffer

! store in model buffer
call kim_api_set_model_buffer_f(pkim, pbuffer, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_model_buffer_f", ier)
   goto 42
endif

! set pointers to parameters in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "PARAM_FREE_cutoff",  one, loc(bufparam(1)), 1, &
     "PARAM_FIXED_cutsq",  one, loc(bufparam(2)), 1, &
     "PARAM_FREE_epsilon", one, loc(bufparam(3)), 1, &
     "PARAM_FREE_sigma",   one, loc(bufparam(4)), 1, &
     "PARAM_FIXED_shift",  one, loc(bufparam(5)), 1  &
     )
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data_f", ier);
   goto 42
endif

ier = KIM_STATUS_OK
42 continue
model_driver_init = ier
return

end function model_driver_init
