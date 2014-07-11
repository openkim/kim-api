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
!

!****************************************************************************
!**
!**  MODULE model_Ar_P_MLJ_F03
!**
!**  Lennard-Jones pair potential model for Ar
!**
!**  Reference: Ashcroft and Mermin
!**
!**  Language: Fortran 2003
!**
!**  Release: This file is part of the kim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_Ar_P_MLJ_F03

use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff

! Below are the definitions and values of all Model parameters
integer(c_int), parameter :: cd = c_double ! used for literal constants
integer(c_int), parameter :: DIM=3                   ! dimensionality of space
integer(c_int), parameter :: speccode = 1            ! internal species code
real(c_double), parameter :: model_cutoff  = 8.15_cd ! cutoff radius
                                                     ! in angstroms
real(c_double), parameter :: model_cutsq   = model_cutoff**2

!-------------------------------------------------------------------------------
! Below are the definitions and values of all additional model parameters
!
! Recall that the Fortran 2003 format for declaring parameters is as follows:
!
! integer(c_int), parameter :: parname = value   ! This defines an integer
!                                                ! parameter called `parname'
!                                                ! with a value equal to
!                                                ! `value' (a number)
!
! real(c_double), parameter :: parname = value   ! This defines a real(c_double)
!                                                ! parameter called `parname'
!                                                ! with a value equal to
!                                                ! `value' (a number)
!-------------------------------------------------------------------------------
real(c_double), parameter :: lj_epsilon = 0.0104_cd
real(c_double), parameter :: lj_sigma   = 3.40_cd
real(c_double), parameter :: lj_cutnorm = model_cutoff/lj_sigma
real(c_double), parameter :: lj_A = 12.0_cd*lj_epsilon*(-26.0_cd &
                              + 7.0_cd*lj_cutnorm**6)/(lj_cutnorm**14 &
                              *lj_sigma**2)
real(c_double), parameter :: lj_B = 96.0_cd*lj_epsilon*(7.0_cd &
                              - 2.0_cd*lj_cutnorm**6)/(lj_cutnorm**13*lj_sigma)
real(c_double), parameter :: lj_C = 28.0_cd*lj_epsilon*(-13.0_cd &
                              + 4.0_cd*lj_cutnorm**6)/(lj_cutnorm**12)

contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(r,phi)
implicit none

!-- Transferred variables
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = lj_sigma/r      !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.0_cd
else
   phi = 4.0_cd*lj_epsilon*(sor12-sor6) + lj_A*rsq + lj_B*r + lj_C
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
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi,dphi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = lj_sigma/r      !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   dphi   = 0.0_cd
else
   phi  = 4.0_cd*lj_epsilon*(sor12-sor6) + lj_A*rsq + lj_B*r + lj_C
   dphi = 24.0_cd*lj_epsilon*(-2.0_cd*sor12+sor6)/r  + 2.0_cd*lj_A*r + lj_B
endif

end subroutine calc_phi_dphi

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
integer(c_int) function Compute_Energy_Forces(pkim) bind(c)
implicit none

!-- Transferred variables
type(c_ptr), intent(in)  :: pkim

!-- Local variables
real(c_double) :: Rij(DIM)
real(c_double) :: r,Rsqij,phi,dphi,dEidr = 0.0_cd
integer(c_int) :: i,j,jj,numnei,atom_ret,comp_force,comp_enepot,comp_virial, &
                  comp_energy
character (len=80) :: error_message

!-- KIM variables
integer(c_int), pointer :: N;                 type(c_ptr) :: pN
real(c_double), pointer :: energy;            type(c_ptr) :: penergy
real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
real(c_double), pointer :: boxSideLengths(:); type(c_ptr) :: pboxSideLengths
real(c_double), pointer :: Rij_list(:,:);     type(c_ptr) :: pRij_list
integer(c_int), pointer :: numContrib;        type(c_ptr) :: pnumContrib
integer(c_int), pointer :: nei1atom(:);       type(c_ptr) :: pnei1atom
integer(c_int), pointer :: particleSpecies(:);  type(c_ptr) :: pparticleSpecies
real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial
character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method
integer(c_int) IterOrLoca
integer(c_int) HalfOrFull
integer(c_int) NBC
integer(c_int) numberContrib
integer(c_int) idum

numberContrib = 0 ! initialize

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
Compute_Energy_Forces = kim_api_get_nbc_method(pkim, NBC_Method)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,   &
                               "kim_api_get_nbc_method", &
                               Compute_Energy_Forces)
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

! Determine neighbor list handling mode
!
if (NBC.ne.3) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   IterOrLoca = kim_api_get_neigh_mode(pkim, Compute_Energy_Forces)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,   &
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

! Check to see if we have been asked to compute the forces, energyperatom,
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
call kim_api_getm_data(pkim, Compute_Energy_Forces,                           &
 "numberOfParticles",           pN,              1,                           &
 "particleSpecies",             pparticleSpecies,1,                           &
 "coordinates",                 pcoor,           1,                           &
 "numberContributingParticles", pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  &
 "boxSideLengths",              pboxSideLengths, TRUEFALSE(NBC.eq.2),         &
 "energy",                      penergy,         TRUEFALSE(comp_energy.eq.1), &
 "forces",                      pforce,          TRUEFALSE(comp_force.eq.1),  &
 "particleEnergy",              penepot,         TRUEFALSE(comp_enepot.eq.1), &
 "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1))
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_getm_data_f", Compute_Energy_Forces)
   return
endif

call c_f_pointer(pN,             N)
call c_f_pointer(pparticleSpecies, particleSpecies, [N])
call c_f_pointer(pcoor,          coor,          [DIM,N])
if (HalfOrFull.eq.1)  call c_f_pointer(pnumContrib,     numContrib)
if (NBC.eq.2)         call c_f_pointer(pboxSideLengths, boxSideLengths, [DIM])
if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)
if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])
if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])
if (comp_virial.eq.1) call c_f_pointer(pvirial,         virial,         [6])

if (HalfOrFull.eq.1) then
   if (NBC.ne.3) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER case
      numberContrib = N
   endif
endif

! Check to be sure that the atom species are correct
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

! Initialize potential energies, forces, virial term
!
if (comp_enepot.eq.1) enepot = 0.0_cd
if (comp_energy.eq.1) energy = 0.0_cd
if (comp_force.eq.1)  force  = 0.0_cd
if (comp_virial.eq.1) virial = 0.0_cd

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.3) then
   allocate( nei1atom(N) )
endif

! Initialize neighbor handling for Iterator mode
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh(pkim,0,0,atom_ret,numnei, &
                                             pnei1atom,pRij_list)
   ! check for successful initialization
   if (Compute_Energy_Forces.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh", Compute_Energy_Forces)
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
   if (IterOrLoca.eq.1) then ! ITERATOR mode
      Compute_Energy_Forces = kim_api_get_neigh(pkim,0,1,atom_ret,numnei, &
                                                pnei1atom,pRij_list)
      if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
                             ! incremented past the end of the list,
                             ! terminate loop
      if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
                             ! some sort of problem, exit
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_get_neigh",      &
                                     Compute_Energy_Forces)
         return
      endif

      i = atom_ret

   else                      ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit       ! incremented past end of list,
                             ! terminate loop
      if (NBC.eq.3) then     ! CLUSTER NBC method
         numnei = N - i      ! number of neighbors in list i+1, ..., N
         nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
         Compute_Energy_Forces = KIM_STATUS_OK
      else
         Compute_Energy_Forces = kim_api_get_neigh(pkim,1,i,atom_ret,numnei, &
                                                   pnei1atom,pRij_list)
         if (Compute_Energy_Forces.ne.KIM_STATUS_OK) then
                             ! some sort of problem, exit
            idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                        "kim_api_get_neigh",      &
                                        Compute_Energy_Forces)
            Compute_Energy_Forces = KIM_STATUS_FAIL
            return
         endif
      endif
   endif

   if (NBC.ne.3) call c_f_pointer(pnei1atom, nei1atom, [numnei])
   if (NBC.eq.0) call c_f_pointer(pRij_list, Rij_list, [DIM,numnei])

   ! Loop over the neighbors of atom i
   !
   do jj = 1, numnei

      j = nei1atom(jj)                           ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.0) then                         ! all methods except NEIGH_RVEC
         Rij(:) = coor(:,j) - coor(:,i)          ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.2) then
         where ( abs(Rij) .gt. 0.5_cd*boxSideLengths )
                                                 ! periodic boundary conditions
            Rij = Rij - sign(boxSideLengths,Rij) ! applied where needed.
         end where
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)               ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then         ! particles are interacting?

         r = sqrt(Rsqij)                         ! compute distance
         if (comp_force.eq.1.or.comp_virial.eq.1) then
            call calc_phi_dphi(r,phi,dphi)       ! compute pair potential
                                                 !   and it derivative
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then     ! HALF mode
               dEidr = dphi                      !      double contribution
            else                                 ! FULL mode
               dEidr = 0.5_cd*dphi               !      regular contribution
            endif
         else
            call calc_phi(r,phi)                 ! compute just pair potential
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            enepot(i) = enepot(i) + 0.5_cd*phi   ! accumulate energy
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) &        ! HALF mode
               enepot(j) = enepot(j) + 0.5_cd*phi! (i and j share it)
         endif
         if (comp_energy.eq.1) then
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then     ! HALF mode
               energy = energy + phi             !    add v to total energy
            else                                 ! FULL mode
               energy = energy + 0.5_cd*phi      !    add half v to total energy
            endif
         endif

         ! contribution to virial tensor, virial(i,j)=r(i)*r(j)*(dV/dr)/r
         !
         if (comp_virial.eq.1) then
            virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r
            virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r
            virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r
            virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r
            virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r
            virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r
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
if (NBC.eq.3) deallocate( nei1atom )

! Everything is great
!
Compute_Energy_Forces = KIM_STATUS_OK
return

end function Compute_Energy_Forces

end module ex_model_Ar_P_MLJ_F03

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer(c_int) function model_init(pkim) bind(c)
use, intrinsic :: iso_c_binding
use ex_model_Ar_P_MLJ_F03
use KIM_API_F03
implicit none

!-- Transferred variables
type(c_ptr), intent(in) :: pkim

!-- Local variables
integer(c_int), parameter :: one=1
integer(c_int) ier, idum

!-- KIM variables
real(c_double), pointer :: cutoff; type(c_ptr) :: pcutoff

! store pointer to compute function in KIM object
ier = kim_api_set_method(pkim,"compute",one,c_funloc(Compute_Energy_Forces))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_set_method", ier)
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

ier = KIM_STATUS_OK
42 continue
model_init = ier
return

end function model_init
