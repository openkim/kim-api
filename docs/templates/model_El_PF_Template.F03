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
! Copyright (c) 2013--2014, Regents of the University of Minnesota.
! All rights reserved.
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
!**  Language: Fortran 2003
!**
!**  Release: This file is part of the kim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module model_<FILL element name>_PF_<FILL model name>

use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff,          &
       Destroy

! Below are the definitions and values of all Model parameters
integer(c_int), parameter :: cd = c_double  ! used for literal constants
integer(c_int), parameter :: DIM = 3        ! dimensionality of space
integer(c_int), parameter :: speccode = 1   ! internal species code
real(c_double), parameter :: model_cutoff = <FILL cutoff radius> ! cutoff radius
                                                                 ! in angstroms
real(c_double), parameter :: model_cutsq  = model_cutoff**2

!-------------------------------------------------------------------------------
! Below are the definitions and values of all additional model parameters
!
! Recall that the Fortran 2003 format for declaring parameters is as follows:
!
! integer(c_int), parameter :: parname = value ! This defines an
!                                              ! integer(c_int) parameter
!                                              ! called `parname' with a value
!                                              ! equal to `value' (a number)
!
! real(c_double), parameter :: parname = value ! This defines a real(c_double)
!                                              ! parameter called `parname'
!                                              ! with a value equal to
!                                              ! `value' (a number)
!-------------------------------------------------------------------------------
real(c_double), parameter :: <FILL paramter name> = <FILL parameter value>

contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(r,phi)
implicit none

!-- Transferred variables
real(c_double), intent(in)    :: r
real(c_double), intent(out)   :: phi

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.0_cd
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
real(c_double), intent(in)    :: r
real(c_double), intent(out)   :: phi,dphi

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   dphi   = 0.0_cd
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
real(c_double), intent(in)    :: r
real(c_double), intent(out)   :: g

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   g = 0.0_cd
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
real(c_double), intent(in)    :: r
real(c_double), intent(out)   :: dg

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   dg = 0.0_cd
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
real(c_double), intent(in)    :: rho
real(c_double), intent(out)   :: U

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
real(c_double), intent(in)    :: rho
real(c_double), intent(out)   :: U,dU

!-- Local variables
! <FILL place any local variable definitions here>

U  = !<FILL functional form of U(rho)>
dU = !<FILL functional form of dU(rho)>

end subroutine calc_U_dU


!-------------------------------------------------------------------------------
!
! Compute energy and forces on particles from the positions.
!
!-------------------------------------------------------------------------------
integer(c_int) function Compute_Energy_Forces(pkim) bind(c)
implicit none

!-- Transferred variables
type(c_ptr), intent(in) :: pkim

!-- Local variables
real(c_double) :: Rij(DIM)
real(c_double) :: r,Rsqij,phi,dphi,g,dg,dU,U,dphieff = 0.0_cd
real(c_double) :: dphii,dUi,Ei,dphij,dUj,Ej
integer(c_int) :: i,j,jj,numnei,comp_force,comp_enepot,comp_virial,comp_energy
real(c_double), allocatable :: rho(:),derU(:)
character(len=80) :: error_message

!-- KIM variables
integer(c_int), pointer :: N;                 type(c_ptr) :: pN
real(c_double), pointer :: energy;            type(c_ptr) :: penergy
real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
real(c_double), pointer :: boxSideLengths(:); type(c_ptr) :: pboxSideLengths
real(c_double), pointer :: Rij_list(:,:);     type(c_ptr) :: pRij_list
integer(c_int), pointer :: numContrib;        type(c_ptr) :: pnumContrib
integer(c_int), pointer :: nei1part(:);       type(c_ptr) :: pnei1part
integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies
real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial
character(len=KIM_KEY_STRING_LENGTH), pointer :: NBC_Method
type(c_ptr) :: pNBC_Method
integer(c_int) IterOrLoca
integer(c_int) HalfOrFull
integer(c_int) NBC
integer(c_int) numberContrib
integer(c_int) idum
integer(c_int) part_ret

numberContrib = 0 ! initialize

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method(pkim, Compute_Energy_Forces)
call c_f_pointer(pNBC_Method, NBC_Method)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_nbc_method", Compute_Energy_Forces)
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
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "Unknown NBC method", Compute_Energy_Forces)
   return
endif
call KIM_API_c_free(pNBC_Method) ! don't forget to release the memory...
NBC_Method => null()             ! nullify association

! Determine neighbor list handling mode
!
if (NBC.ne.3) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   IterOrLoca = kim_api_get_neigh_mode(pkim, Compute_Energy_Forces)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh_mode", &
                                  Compute_Energy_Forces)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      Compute_Energy_Forces = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  error_message, Compute_Energy_Forces)
      return
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperpart,
! energy and virial
!
call kim_api_getm_compute(pkim, Compute_Energy_Forces, &
     "energy",         comp_energy, 1, &
     "forces",         comp_force,  1, &
     "particleEnergy", comp_enepot, 1, &
     "virial",         comp_virial, 1)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_getm_compute", Compute_Energy_Forces)
   return
endif

! Unpack data from KIM object
!
call kim_api_getm_data(pkim, Compute_Energy_Forces, &
     "numberOfParticles",           pN,              1,                           &
     "particleSpecies",             pparticleSpecies,1,                           &
     "coordinates",                 pcoor,           1,                           &
     "numberContributingParticles", pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  &
     "boxSideLengths",              pboxSideLengths, TRUEFALSE(NBC.eq.2),         &
     "energy",                      penergy,         TRUEFALSE(comp_energy.eq.1), &
     "forces",                      pforce,          TRUEFALSE(comp_force.eq.1),  &
     "particleEnergy",              penepot,         TRUEFALSE(comp_enepot.eq.1), &
     "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1)  &
     )
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_getm_data", Compute_Energy_Forces)
   return
endif

call c_f_pointer(pN,               N)
call c_f_pointer(pparticleSpecies, particleSpecies, [N])
call c_f_pointer(pcoor,            coor,            [DIM,N])
if (HalfOrFull.eq.1)  call c_f_pointer(pnumContrib,     numContrib)
if (NBC.eq.2)         call c_f_pointer(pboxSideLengths, boxSideLengths, [DIM])
if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)
if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])
if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])
if (comp_virial.eq.1) call c_f_pointer(pvirial,         virial,         [6])

if (HalfOrFull.eq.1) then
   if (NBC.ne.3) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER cases
      numberContrib = N
   endif
endif

! Check to be sure that the species are correct
!
Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (particleSpecies(i).ne.speccode) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,           &
                                  "Unexpected species specie detected", &
                                  Compute_Energy_Forces)
      return
   endif
enddo
Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces, virial term, electron density
!
! Note: that the variable `enepot' does not need to be initialized
!       because it's initial value is set during the embedding energy
!       calculation.
!
if (comp_energy.eq.1) energy = 0.0_cd
if (comp_force.eq.1)  force  = 0.0_cd
if (comp_virial.eq.1) virial = 0.0_cd
allocate( rho(N) )  ! pair functional electron density
rho = 0.0_cd
! EAM embedded energy deriv
if (comp_force.eq.1.or.comp_virial.eq.1) allocate( derU(N) )

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.3) then
   allocate( nei1part(N) )
endif

!
!  Compute energy and forces
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh(pkim,0,0,part_ret,numnei, &
                                             pnei1part,pRij_list)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh", Compute_Energy_Forces)
      return
   endif
endif

!  Loop over particles in the neighbor list a first time,
!  to compute electron density (=coordination)
!
i = 0
do

   ! Set up neighbor list for next particle for all NBC methods
   !
   call get_current_part_neighbors(IterOrLoca,NBC,N,pkim,i,numnei,nei1part, &
                                   Rij_list,Compute_Energy_Forces)
   ! particle counter incremented past end of list
   if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit

   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,     &
                                  "get_current_part_neighbors", &
                                  Compute_Energy_Forces)
      return
   endif

   ! Loop over the neighbors of particle i
   !
   do jj = 1, numnei

      j = nei1part(jj)                    ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.0) then                  ! all methods except NEIGH_RVEC
         Rij(:) = coor(:,j) - coor(:,i)   ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.2) then
         where ( abs(Rij) .gt. 0.5_cd*boxSideLengths ) ! PBCs
            Rij = Rij - sign(boxSideLengths,Rij)       ! applied where needed.
         end where
      endif

      ! compute contribution to electron density
      !
      Rsqij = dot_product(Rij,Rij)        ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then  ! particles are interacting?
         r = sqrt(Rsqij)                  ! compute distance
         call calc_g(r,g)                 ! compute electron density
         rho(i) = rho(i) + g              ! accumulate electron density
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) &    ! HALF mode
            rho(j) = rho(j) + g           !      (add contrib to j)
      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

!  Now that we know the electron densities, calculate embedding part of energy
!  U and its derivative U' (derU)
!
do i = 1,N
   if (comp_force.eq.1.or.comp_virial.eq.1) then
      call calc_U_dU(rho(i),U,dU)              ! compute embedding energy
                                               !   and its derivative
      derU(i) = dU                             ! store dU for later use
   else
      call calc_U(rho(i),U)                    ! compute just embedding energy
   endif

   ! accumulate the embedding energy contribution
   !
   ! Assuming U(rho=0) = 0.0_cd
   !
   if (comp_enepot.eq.1) then  ! accumulate embedding energy contribution
      enepot(i) = U
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
   Compute_Energy_Forces = kim_api_get_neigh(pkim,0,0,part_ret,numnei, &
                                             pnei1part,pRij_list)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh", Compute_Energy_Forces)
      return
   endif
endif

i = 0
do

   ! Set up neighbor list for next particle for all NBC methods
   !
   call get_current_part_neighbors(IterOrLoca,NBC,N,pkim,i,numnei,nei1part, &
                                   Rij_list,Compute_Energy_Forces)
   ! particle counter incremented past end of list
   if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,     &
                                  "get_current_part_neighbors", &
                                  Compute_Energy_Forces)
      return
   endif

   ! Loop over the neighbors of particle i
   !
   do jj = 1, numnei

      j = nei1part(jj)                   ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.0) then                 ! all methods except NEIGH_RVEC
         Rij(:) = coor(:,j) - coor(:,i)  ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.2) then
         where ( abs(Rij) .gt. 0.5_cd*boxSideLengths ) ! PBCs
            Rij = Rij - sign(boxSideLengths,Rij)       ! applied where needed.
         end where
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?

         r = sqrt(Rsqij)                          ! compute distance
         if (comp_force.eq.1.or.comp_virial.eq.1) then
            call calc_phi_dphi(r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative
            call calc_dg(r,dg)                    ! compute elect dens
                                                  !   first deriv
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dphii  = 0.5_cd*dphi
               dphij  = 0.5_cd*dphi
               dUi    = derU(i)*dg
               dUj    = derU(j)*dg
            else                                  ! FULL mode
               dphii  = 0.5_cd*dphi
               dphij  = 0.0_cd
               dUi    = derU(i)*dg
               dUj    = 0.0_cd
            endif
            dphieff = dphii + dphij + dUi + dUj
         else
            call calc_phi(r,phi)                  ! compute just pair potential
         endif
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) then         ! HALF mode
            Ei     = 0.5_cd*phi
            Ej     = 0.5_cd*phi
         else                                     ! FULL mode
            Ei     = 0.5_cd*phi
            Ej     = 0.0_cd
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            enepot(i) = enepot(i) + Ei            ! accumulate energy Ei
            enepot(j) = enepot(j) + Ej            ! accumulate energy Ej
         endif
         if (comp_energy.eq.1) then
            energy = energy + Ei                  ! accumulate energy
            energy = energy + Ej                  ! accumulate energy
         endif

         ! contribution to virial tensor
         !
         if (comp_virial.eq.1) then
            virial(1) = virial(1) + Rij(1)*Rij(1)*dphieff/r
            virial(2) = virial(2) + Rij(2)*Rij(2)*dphieff/r
            virial(3) = virial(3) + Rij(3)*Rij(3)*dphieff/r
            virial(4) = virial(4) + Rij(2)*Rij(3)*dphieff/r
            virial(5) = virial(5) + Rij(1)*Rij(3)*dphieff/r
            virial(6) = virial(6) + Rij(1)*Rij(2)*dphieff/r
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then                        ! Ei contribution
            force(:,i) = force(:,i) + dphieff*Rij/r ! accumulate force on particle i
            force(:,j) = force(:,j) - dphieff*Rij/r ! accumulate force on particle j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Free temporary storage
!
if (NBC.eq.3) deallocate( nei1part )
deallocate( rho )
if (comp_force.eq.1.or.comp_virial.eq.1) deallocate( derU )

! Everything is great
!
Compute_Energy_Forces = KIM_STATUS_OK
return

end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Get list of neighbors for current particle using all NBC methods
!
!-------------------------------------------------------------------------------
subroutine get_current_part_neighbors(IterOrLoca,NBC,N,pkim,      &
                                      part,numnei,nei1part,Rij_list,ier)
implicit none

!-- Transferred variables
integer(c_int),          intent(in)    :: IterOrLoca
integer(c_int),          intent(in)    :: NBC
integer(c_int),          intent(in)    :: N
type(c_ptr),             intent(in)    :: pkim
integer(c_int),          intent(inout) :: part
integer(c_int),          intent(out)   :: numnei
integer(c_int),          intent(out)   :: ier
integer(c_int), pointer, intent(inout) :: nei1part(:);  type(c_ptr) :: pnei1part
real(c_double), pointer, intent(inout) :: Rij_list(:,:);type(c_ptr) :: pRij_list

!-- Local variables
integer(c_int) part_ret, jj
integer(c_int) idum

! Set up neighbor list for next particle for all NBC methods
!
if (IterOrLoca.eq.1) then    ! ITERATOR mode
   ier = kim_api_get_neigh(pkim,0,1,part_ret,numnei,pnei1part,pRij_list)
   if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) then
                          ! past end of the list, terminate loop in
      return              ! calling routine
   endif
   if (ier.lt.KIM_STATUS_OK) then     ! some sort of problem, exit
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh", ier)
      return
   endif
   part = part_ret
   call c_f_pointer(pnei1part, nei1part, [numnei])
   if (NBC.eq.0) call c_f_pointer(pRij_list, Rij_list, [DIM,numnei])
else                         ! LOCATOR mode

   part = part + 1
   if (part.gt.N) then                     ! incremented past end of list,
      ier = KIM_STATUS_NEIGH_ITER_PAST_END ! terminate loop in calling routine
      return
   endif

   if (NBC.eq.3) then ! CLUSTER NBC method
      numnei = N - part   ! number of neighbors in list part+1, ..., N
      nei1part(1:numnei) = (/ (part+jj, jj = 1,numnei) /)
      ier = KIM_STATUS_OK
   else
      ier = kim_api_get_neigh(pkim,1,part,part_ret,numnei,pnei1part,pRij_list)
      if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_get_neigh", ier)
         ier = KIM_STATUS_FAIL
         return
      endif

      call c_f_pointer(pnei1part, nei1part, [numnei])
      if (NBC.eq.0) call c_f_pointer(pRij_list, Rij_list, [DIM,numnei])
   endif
endif

return
end subroutine get_current_part_neighbors

!-------------------------------------------------------------------------------
!
! Model destroy routine
!
!-------------------------------------------------------------------------------
integer(c_int) function Destroy(pkim) bind(c)
implicit none

!-- Transferred variables
type(c_ptr), intent(in) :: pkim

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
integer(c_int) function model_init(pkim) bind(c)
use, intrinsic :: iso_c_binding
use model_<FILL element name>_PF_<FILL model name>
use KIM_API_F03
implicit none

!-- Transferred variables
type(c_ptr), intent(in) :: pkim

!-- Local variables
integer(c_int), parameter :: one=1
integer(c_int) ier, idum

!-- KIM variables
real(c_double), pointer :: cutoff;  type(c_ptr) :: pcutoff

! store function pointers in KIM object
call kim_api_setm_method(pkim, ier, &
     "compute", one, c_funloc(Compute_Energy_Forces), 1, &
     "destroy", one, c_funloc(Destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_setm_method", ier)
   goto 42
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_data", ier)
   goto 42
endif
call c_f_pointer(pcutoff, cutoff)
cutoff = model_cutoff

!<FILL as necessary>

ier = KIM_STATUS_OK
42 continue
model_init = ier
return

end function model_init
