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
!    <FILL name>
!

!****************************************************************************
!**
!**  MODULE model_<FILL element name>_P_<FILL model name>
!**
!**  <FILL model name> pair potential model for <FILL element name>
!**
!**  Reference: <FILL>
!**
!**  Language: Fortran 90
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module model_<FILL element name>_P_<FILL model name>

use KIM_API
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code
double precision, parameter :: model_cutoff  = <FILL cutoff radius> ! cutoff radius
                                                                    ! in angstroms
double precision, parameter :: model_cutsq   = model_cutoff**2

!-------------------------------------------------------------------------------
! Below are the definitions and values of all additional model parameters
!
! Recall that the Fortran 90 format for declaring parameters is as follows:
!
! integer, parameter :: parname = value          ! This defines an integer
!                                                ! parameter called `parname' with a
!                                                ! value equal to `value' (a number)
!
! double precision, parameter :: parname = value ! This defines a real double precision
!                                                ! parameter called `parname' with a
!                                                ! value equal to `value' (a number)
!-------------------------------------------------------------------------------
double precision, parameter :: <FILL parameter name> = <FILL parameter value>

contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(r,phi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: r
double precision, intent(out) :: phi

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
else
   phi = !<FILL functional form of phi(r)>
endif

end subroutine calc_phi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivative dphi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi(r,phi,dphi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
else
   phi  = !<FILL functional form of phi(r)>
   dphi = !<FILL functional form of dphi(r)>
endif

end subroutine calc_phi_dphi

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
double precision :: Rij(DIM)
double precision :: r,Rsqij,phi,dphi,dEidr
integer :: i,j,jj,numnei,atom_ret,comp_force,comp_enepot,comp_virial,comp_energy
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message

!-- KIM variables
integer N;                   pointer(pN,N)
real*8  energy;              pointer(penergy,energy)
real*8  coordum(DIM,1);      pointer(pcoor,coordum)
real*8  forcedum(DIM,1);     pointer(pforce,forcedum)
real*8  enepotdum(1);        pointer(penepot,enepotdum)
real*8  boxSideLengths(DIM); pointer(pboxSideLengths,boxSideLengths)
real*8  Rij_list(DIM,1);     pointer(pRij_list,Rij_list)
integer numContrib;          pointer(pnumContrib,numContrib)
integer nei1atom(1);         pointer(pnei1atom,nei1atom)
integer particleTypes(1);    pointer(pparticleTypes,particleTypes)
real*8  virialdum(1);        pointer(pvirial,virialdum)
character*64 NBC_Method;     pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:),virial_global(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib
integer idum

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method_f(pkim, Compute_Energy_Forces)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method_f", Compute_Energy_Forces)
   return
endif
if (index(NBC_Method,"NEIGH_RVEC_H").eq.1) then
   NBC = 0
   HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
   NBC = 1
   HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
   NBC = 0
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
   NBC = 1
   HalfOrFull = 2
elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then
   NBC = 2
   HalfOrFull = 1
elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
   NBC = 2
   HalfOrFull = 2
elseif (index(NBC_Method,"CLUSTER").eq.1) then
   NBC = 3
   HalfOrFull = 1
else
   Compute_Energy_Forces = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", Compute_Energy_Forces)
   return
endif
call free(pNBC_Method) ! don't forget to release the memory...

! Determine neighbor list handling mode
!
if (NBC.ne.3) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   IterOrLoca = kim_api_get_neigh_mode_f(pkim, Compute_Energy_Forces)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_mode_f", Compute_Energy_Forces)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      Compute_Energy_Forces = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    error_message, Compute_Energy_Forces)
      return
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and virial
!
call kim_api_getm_compute_f(pkim, Compute_Energy_Forces, &
     "energy",         comp_energy, 1, &
     "forces",         comp_force,  1, &
     "particleEnergy", comp_enepot, 1, &
     "virial",         comp_virial, 1)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_compute_f", Compute_Energy_Forces)
   return
endif

! Unpack data from KIM object
!
call kim_api_getm_data_f(pkim, Compute_Energy_Forces, &
     "numberOfParticles",           pN,              1,                           &
     "particleTypes",               pparticleTypes,  1,                           &
     "coordinates",                 pcoor,           1,                           &
     "numberContributingParticles", pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  &
     "boxSideLengths",              pboxSideLengths, TRUEFALSE(NBC.eq.2),         &
     "energy",                      penergy,         TRUEFALSE(comp_energy.eq.1), &
     "forces",                      pforce,          TRUEFALSE(comp_force.eq.1),  &
     "particleEnergy",              penepot,         TRUEFALSE(comp_enepot.eq.1), &
     "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1))
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data_f", Compute_Energy_Forces)
   return
endif

call KIM_to_F90_real_array_2d(coordum,coor,DIM,N)
if (comp_force.eq.1)  call KIM_to_F90_real_array_2d(forcedum,force,DIM,N)
if (comp_enepot.eq.1) call KIM_to_F90_real_array_1d(enepotdum,ene_pot,N)
if (comp_virial.eq.1) call KIM_to_F90_real_array_1d(virialdum,virial_global,6)

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

! Initialize potential energies, forces, virial term
!
if (comp_enepot.eq.1) ene_pot(1:N)   = 0.d0
if (comp_energy.eq.1) energy         = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0
if (comp_virial.eq.1) virial_global  = 0.d0

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
      if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then     ! some sort of problem, exit
         idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_get_neigh_f", Compute_Energy_Forces)
         return
      endif

      i = atom_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (NBC.eq.3) then     ! CLUSTER NBC method
         numnei = N - i      ! number of neighbors in list i+1, ..., N
         nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
         Compute_Energy_Forces = KIM_STATUS_OK
      else
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
         if (comp_force.eq.1.or.comp_virial.eq.1) then
            call calc_phi_dphi(r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr = dphi                       !      double contribution
            else                                  ! FULL mode
               dEidr = 0.5d0*dphi                 !      regular contribution
            endif
         else
            call calc_phi(r,phi)                  ! compute just pair potential
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

         ! contribution to virial tensor, virial(i,j)=r(i)*r(j)*(dV/dr)/r
         !
         if (comp_virial.eq.1) then
            virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dEidr/r
            virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dEidr/r
            virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dEidr/r
            virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dEidr/r
            virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dEidr/r
            virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dEidr/r
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

end module model_<FILL element name>_P_<FILL model name>

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer function model_init(pkim)
use model_<FILL element name>_P_<FILL model name>
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
integer ier, idum

!-- KIM variables
real*8 cutoff; pointer(pcutoff,cutoff)

! store pointer to compute function in KIM object
ier = kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_data", ier)
   goto 42
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
   goto 42
endif
cutoff = model_cutoff

ier = KIM_STATUS_OK
42 continue
model_<FILL element name>_P_<FILL model name>_init = ier
return

end function model_init

