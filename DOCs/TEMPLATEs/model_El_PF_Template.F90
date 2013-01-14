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
! Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
!    Stephen M. Whalen
!    <FILL your name here>
!

!****************************************************************************
!**
!**  MODULE model_<FILL element name>_PF_<FILL model name>
!**
!**  <FILL model name> pair functional model for <FILL element name>
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

module model_<FILL element name>_PF_<FILL model name>

use KIM_API
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff,          &
       Destroy

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
double precision, parameter :: <FILL paramter name> = <FILL parameter value>

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
!  Calculate electron density g(r)
!
!-------------------------------------------------------------------------------
subroutine calc_g(r,g)
implicit none

!-- Transferred variables
double precision, intent(in)  :: r
double precision, intent(out) :: g

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   g = 0.d0
else
   g = !<FILL functional form of g(r)>
endif

end subroutine calc_g

!-------------------------------------------------------------------------------
!
!  Calculate electron density derivative dg(r)
!
!-------------------------------------------------------------------------------
subroutine calc_dg(r,dg)
implicit none

!-- Transferred variables
double precision, intent(in)  :: r
double precision, intent(out) :: dg

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   dg = 0.d0
else
   dg = !<FILL functional form of dg(r)>
endif

end subroutine calc_dg

!-------------------------------------------------------------------------------
!
!  Calculate embedding function U(rho)
!
!-------------------------------------------------------------------------------
subroutine calc_U(rho,U)
implicit none

!-- Transferred variables
double precision, intent(in)  :: rho
double precision, intent(out) :: U

!-- Local variables
! <FILL place any local variable definitions here>

U = !<FILL functional form of U(rho)>

end subroutine calc_U

!-------------------------------------------------------------------------------
!
!  Calculate embedding function U(rho) and first derivative dU(rho)
!
!-------------------------------------------------------------------------------
subroutine calc_U_dU(rho,U,dU)
implicit none

!-- Transferred variables
double precision, intent(in)  :: rho
double precision, intent(out) :: U,dU

!-- Local variables
! <FILL place any local variable definitions here>

U  = !<FILL functional form of U(rho)>
dU = !<FILL functional form of dU(rho)>

end subroutine calc_U_dU


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
double precision :: r,Rsqij,phi,dphi,g,dg,dU,U,dphieff
double precision :: dphii,dUi,Ei,dphij,dUj,Ej
integer :: i,j,jj,numnei,comp_force,comp_enepot,comp_virial,comp_energy
double precision, allocatable :: rho(:),derU(:)
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message

!-- KIM variables
integer N;                  pointer(pN,N)
real*8 energy;              pointer(penergy,energy)
real*8 coordum(DIM,1);      pointer(pcoor,coordum)
real*8 forcedum(DIM,1);     pointer(pforce,forcedum)
real*8 enepotdum(1);        pointer(penepot,enepotdum)
real*8 boxSideLengths(DIM); pointer(pboxSideLengths,boxSideLengths)
real*8 Rij_list(DIM,1);     pointer(pRij_list,Rij_list)
integer numContrib;         pointer(pnumContrib,numContrib)
integer nei1atom(1);        pointer(pnei1atom,nei1atom)
integer particleTypes(1);   pointer(pparticleTypes,particleTypes)
real*8 virialdum(1);        pointer(pvirial,virialdum)
character*64 NBC_Method;    pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:),virial_global(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib
integer idum
integer atom_ret

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
     "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1)  &
     )
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
   else               ! CLUSTER cases
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

! Initialize potential energies, forces, virial term, electron density
!
! Note: that the variable `ene_pot' does not need to be initialized
!       because it's initial value is set during the embedding energy
!       calculation.
!
if (comp_energy.eq.1) energy         = 0.d0
if (comp_force.eq.1)  force(1:DIM,1:N) = 0.d0
if (comp_virial.eq.1) virial_global  = 0.d0
allocate( rho(N) )  ! pair functional electron density
rho(1:N) = 0.d0
if (comp_force.eq.1.or.comp_virial.eq.1) allocate( derU(N) )  ! EAM embedded energy deriv

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.3) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

!
!  Compute energy and forces
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh_f(pkim,0,0,atom_ret,numnei,pnei1atom,pRij_list)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", Compute_Energy_Forces)
      return
   endif
endif

!  Loop over particles in the neighbor list a first time,
!  to compute electron density (=coordination)
!
i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   call get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                   i,numnei,pnei1atom,pRij_list,Compute_Energy_Forces)
   if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit  ! atom counter incremented past end of list
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "get_current_atom_neighbors", Compute_Energy_Forces)
      return
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

      ! compute contribution to electron density
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?
         r = sqrt(Rsqij)                          ! compute distance
         call calc_g(r,g)                         ! compute electron density
         rho(i) = rho(i) + g                      ! accumulate electron density
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) &            ! HALF mode
            rho(j) = rho(j) + g                   !      (add contrib to j)
      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

!  Now that we know the electron densities, calculate embedding part of energy
!  U and its derivative U' (derU)
!
do i = 1,N
   if (comp_force.eq.1.or.comp_virial.eq.1) then
      call calc_U_dU(rho(i),U,dU)                 ! compute embedding energy
                                                  !   and its derivative
      derU(i) = dU                                ! store dU for later use
   else
      call calc_U(rho(i),U)                       ! compute just embedding energy
   endif

   ! accumulate the embedding energy contribution
   !
   ! Assuming U(rho=0) = 0.0d0
   !
   if (comp_enepot.eq.1) then                     ! accumulate embedding energy contribution
      ene_pot(i) = U
   endif
   if (comp_energy.eq.1) then
      energy = energy + U
   endif

   if ((HalfOrFull.eq.1) .and. (i .gt. numberContrib)) exit
enddo

!  Loop over particles in the neighbor list a second time, to compute
!  the forces and complete energy calculation
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh_f(pkim,0,0,atom_ret,numnei,pnei1atom,pRij_list)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", Compute_Energy_Forces)
      return
   endif
endif

i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   call get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                   i,numnei,pnei1atom,pRij_list,Compute_Energy_Forces)

   if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit  ! atom counter incremented past end of list
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "get_current_atom_neighbors", Compute_Energy_Forces)
      return
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
            call calc_dg(r,dg)                    ! compute elect dens first deriv
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dphii  = 0.5d0*dphi
               dphij  = 0.5d0*dphi
               dUi    = derU(i)*dg
               dUj    = derU(j)*dg
            else                                  ! FULL mode
               dphii  = 0.5d0*dphi
               dphij  = 0.0d0
               dUi    = derU(i)*dg
               dUj    = 0.0d0
            endif
            dphieff = dphii + dphij + dUi + dUj
         else
            call calc_phi(r,phi)                  ! compute just pair potential
         endif
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) then         ! HALF mode
            Ei     = 0.5d0*phi
            Ej     = 0.5d0*phi
         else                                  ! FULL mode
            Ei     = 0.5d0*phi
            Ej     = 0.0d0
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + Ei          ! accumulate energy Ei
            ene_pot(j) = ene_pot(j) + Ej          ! accumulate energy Ej
         endif
         if (comp_energy.eq.1) then
            energy = energy + Ei                  ! accumulate energy
            energy = energy + Ej                  ! accumulate energy
         endif

         ! contribution to virial tensor
         !
         if (comp_virial.eq.1) then
            virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dphieff/r
            virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dphieff/r
            virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dphieff/r
            virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dphieff/r
            virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dphieff/r
            virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dphieff/r
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then                        ! Ei contribution
            force(:,i) = force(:,i) + dphieff*Rij/r ! accumulate force on atom i
            force(:,j) = force(:,j) - dphieff*Rij/r ! accumulate force on atom j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Free temporary storage
!
if (NBC.eq.3) deallocate( nei1atom_substitute )
deallocate( rho )
if (comp_force.eq.1.or.comp_virial.eq.1) deallocate( derU )

! Everything is great
!
Compute_Energy_Forces = KIM_STATUS_OK
return

end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Get list of neighbors for current atom using all NBC methods
!
!-------------------------------------------------------------------------------
subroutine get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                      atom,numnei,pnei1atom,pRij_list,ier)
implicit none

!-- Transferred variables
integer,                  intent(in)    :: IterOrLoca
integer,                  intent(in)    :: HalfOrFull
integer,                  intent(in)    :: NBC
integer,                  intent(in)    :: N
integer(kind=kim_intptr), intent(in)    :: pkim
integer,                  intent(inout) :: atom
integer,                  intent(out)   :: numnei
integer,                  intent(out)   :: ier
integer nei1atom(1);    pointer(pnei1atom,nei1atom)
real*8 Rij_list(DIM,1); pointer(pRij_list,Rij_list)

!-- Local variables
integer atom_ret, jj
integer idum

! Set up neighbor list for next atom for all NBC methods
!
if (IterOrLoca.eq.1) then    ! ITERATOR mode
   ier = kim_api_get_neigh_f(pkim,0,1,atom_ret,numnei, &
                             pnei1atom,pRij_list)
   if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) then
                          ! past end of the list, terminate loop in
      return              ! calling routine
   endif
   if (ier.lt.KIM_STATUS_OK) then     ! some sort of problem, exit
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", ier)
      return
   endif
   atom = atom_ret

else                         ! LOCATOR mode

   atom = atom + 1
   if (atom.gt.N) then                     ! incremented past end of list,
      ier = KIM_STATUS_NEIGH_ITER_PAST_END ! terminate loop in calling routine
      return
   endif

   if (NBC.eq.3) then ! CLUSTER NBC method
      numnei = N - atom   ! number of neighbors in list atom+1, ..., N
      nei1atom(1:numnei) = (/ (atom+jj, jj = 1,numnei) /)
      ier = KIM_STATUS_OK
   else
      ier = kim_api_get_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_list)
      if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
         idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_get_neigh_f", ier)
         ier = KIM_STATUS_FAIL
         return
      endif
   endif
endif

return
end subroutine get_current_atom_neighbors

!-------------------------------------------------------------------------------
!
! Model destroy routine
!
!-------------------------------------------------------------------------------
integer function Destroy(pkim)
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!<FILL as necessary>

Destroy = KIM_STATUS_OK
return

end function Destroy

end module model_<FILL element name>_PF_<FILL model name>

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer function model_<FILL element name>_PF_<FILL model name>_init(pkim)
use model_<FILL element name>_PF_<FILL model name>
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
integer ier, idum

!-- KIM variables
real*8 cutoff; pointer(pcutoff,cutoff)

! store function pointers in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "compute", one, loc(Compute_Energy_Forces), 1, &
     "destroy", one, loc(Destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data_f", ier)
   goto 42
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data_f", ier)
   goto 42
endif
cutoff = model_cutoff

!<FILL as necessary>

ier = KIM_STATUS_OK
42 continue
ex_model_<FILL element name>_PF_<FILL model name>_init = ier
return
end function model_<FILL element name>_PF_<FILL model name>_init

