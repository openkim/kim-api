! Markup tag documentation:                                                     !..
!    1 - ex_model_Ar_P_MLJ_F03.F03                                              !..
!    2 - model_El_P_Template.F03                                                !..
!                                                                               !..
!                                                                               !12
! CDDL HEADER START                                                             !12
!                                                                               !12
! The contents of this file are subject to the terms of the Common Development  !12
! and Distribution License Version 1.0 (the "License").                         !12
!                                                                               !12
! You can obtain a copy of the license at                                       !12
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the         !12
! specific language governing permissions and limitations under the License.    !12
!                                                                               !12
! When distributing Covered Code, include this CDDL HEADER in each file and     !12
! include the License file in a prominent location with the name LICENSE.CDDL.  !12
! If applicable, add the following below this CDDL HEADER, with the fields      !12
! enclosed by brackets "[]" replaced with your own identifying information:     !12
!                                                                               !12
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved. !12
!                                                                               !12
! CDDL HEADER END                                                               !12
!                                                                               !12
                                                                                !12
!                                                                               !12
! Copyright (c) 2013--2018, Regents of the University of Minnesota.             !12
! All rights reserved.                                                          !12
!                                                                               !12
! Contributors:                                                                 !12
!    Ryan S. Elliott                                                            !12
!    Ellad B. Tadmor                                                            !12
!    Valeriu Smirichinski                                                       !12
!    Stephen M. Whalen                                                          !12
!                                                                               !12
                                                                                !12
!                                                                               !.2
! Portions Copyright (c) <FILL_year>, <FILL_copyright_holder>.                  !.2
! All rights reserved.                                                          !.2
!                                                                               !.2
! Contributors:                                                                 !.2
!    <FILL_name>                                                                !.2
!                                                                               !.2
                                                                                !.2
!****************************************************************************   !12
!**                                                                             !12
!**  MODULE <FILL_model_name>                                                   !12
!**                                                                             !12
!**  Modified Lennard-Jones pair potential (with smooth cutoff) model for Ar    !1.
!**  <FILL_model_class_name> pair potential model for <FILL_element_name>       !.2
!**                                                                             !12
!**  Reference: <FILL_model_reference>                                          !12
!**                                                                             !12
!**  Language: Fortran 2003                                                     !12
!**                                                                             !12
!**  Release: This file is part of the kim-api.git repository.                  !12
!**                                                                             !12
!****************************************************************************   !12
                                                                                !12
                                                                                !12
#include "KIM_API_status.h"                                                     !12
#define THIS_FILE_NAME __FILE__                                                 !12
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))                                     !12
                                                                                !12
module <FILL_model_name>                                                        !12
                                                                                !12
use, intrinsic :: iso_c_binding                                                 !12
use KIM_API_F03                                                                 !12
implicit none                                                                   !12
                                                                                !12
save                                                                            !12
private                                                                         !12
public Compute_Energy_Forces, &                                                 !12
       model_cutoff                                                             !12
                                                                                !12
! Below are the definitions and values of all Model parameters                  !12
integer(c_int), parameter :: cd = c_double  ! used for literal constants        !12
integer(c_int), parameter :: DIM = 3  ! dimensionality of space                 !12
integer(c_int), parameter :: speccode = 1  ! internal species code              !12
real(c_double), parameter :: model_cutoff = 8.15_cd ! cutoff radius             !1.
                                                    ! in angstroms              !1.
real(c_double), parameter :: model_cutoff = <FILL_cutoff>_cd  ! cutoff radius   !.2
                                                              ! in angstroms    !.2
real(c_double), parameter :: model_cutsq = model_cutoff**2                      !12
                                                                                !12
!-------------------------------------------------------------------------------!12
! Below are the definitions and values of all additional model parameters       !12
!                                                                               !12
! Recall that the Fortran 2003 format for declaring parameters is as follows:   !12
!                                                                               !12
! integer(c_int), parameter :: parname = value   ! This defines an integer      !12
!                                                ! parameter called `parname'   !12
!                                                ! with a value equal to        !12
!                                                ! `value' (a number)           !12
!                                                                               !12
! real(c_double), parameter :: parname = value   ! This defines a real(c_double)!12
!                                                ! parameter called `parname'   !12
!                                                ! with a value equal to        !12
!                                                ! `value' (a number)           !12
!-------------------------------------------------------------------------------!12
real(c_double), parameter :: lj_epsilon = 0.0104_cd                             !1.
real(c_double), parameter :: lj_sigma   = 3.40_cd                               !1.
real(c_double), parameter :: lj_cutnorm = model_cutoff/lj_sigma                 !1.
real(c_double), parameter :: lj_A = 12.0_cd*lj_epsilon*(-26.0_cd &              !1.
                              + 7.0_cd*lj_cutnorm**6)/(lj_cutnorm**14 &         !1.
                              *lj_sigma**2)                                     !1.
real(c_double), parameter :: lj_B = 96.0_cd*lj_epsilon*(7.0_cd &                !1.
                              - 2.0_cd*lj_cutnorm**6)/(lj_cutnorm**13*lj_sigma) !1.
real(c_double), parameter :: lj_C = 28.0_cd*lj_epsilon*(-13.0_cd &              !1.
                              + 4.0_cd*lj_cutnorm**6)/(lj_cutnorm**12)          !1.
                                                                                !..
real(c_double), parameter :: <FILL_parameter_name> = <FILL_parameter_value>     !.2
                                                                                !12
contains                                                                        !12
                                                                                !12
!-------------------------------------------------------------------------------!12
!                                                                               !12
!  Calculate pair potential phi(r)                                              !12
!                                                                               !12
!-------------------------------------------------------------------------------!12
subroutine calc_phi(r,phi)                                                      !12
implicit none                                                                   !12
                                                                                !12
!-- Transferred variables                                                       !12
real(c_double), intent(in)  :: r                                                !12
real(c_double), intent(out) :: phi                                              !12
                                                                                !12
!-- Local variables                                                             !12
real(c_double) rsq,sor,sor6,sor12                                               !1.
!<FILL_any_local_variable_definitions_here>                                     !.2
                                                                                !12
rsq  = r*r             !  r^2                                                   !1.
sor  = lj_sigma/r      !  (sig/r)                                               !1.
sor6 = sor*sor*sor     !                                                        !1.
sor6 = sor6*sor6       !  (sig/r)^6                                             !1.
sor12= sor6*sor6       !  (sig/r)^12                                            !1.
if (r .gt. model_cutoff) then                                                   !12
   ! Argument exceeds cutoff radius                                             !12
   phi = 0.0_cd                                                                 !12
else                                                                            !12
   phi = 4.0_cd*lj_epsilon*(sor12-sor6) + lj_A*rsq + lj_B*r + lj_C              !1.
   phi = <FILL_functional_form_of_phi(r)>                                       !.2
endif                                                                           !12
                                                                                !12
end subroutine calc_phi                                                         !12
                                                                                !12
!-------------------------------------------------------------------------------!12
!                                                                               !12
!  Calculate pair potential phi(r) and its derivative dphi(r)                   !12
!                                                                               !12
!-------------------------------------------------------------------------------!12
subroutine calc_phi_dphi(r,phi,dphi)                                            !12
implicit none                                                                   !12
                                                                                !12
!-- Transferred variables                                                       !12
real(c_double), intent(in)  :: r                                                !12
real(c_double), intent(out) :: phi,dphi                                         !12
                                                                                !12
!-- Local variables                                                             !12
real(c_double) rsq,sor,sor6,sor12                                               !1.
! <FILL_any_local_variable_definitions_here>                                    !.2
                                                                                !12
rsq  = r*r             !  r^2                                                   !1.
sor  = lj_sigma/r      !  (sig/r)                                               !1.
sor6 = sor*sor*sor     !                                                        !1.
sor6 = sor6*sor6       !  (sig/r)^6                                             !1.
sor12= sor6*sor6       !  (sig/r)^12                                            !1.
if (r .gt. model_cutoff) then                                                   !12
   ! Argument exceeds cutoff radius                                             !12
   phi    = 0.0_cd                                                              !12
   dphi   = 0.0_cd                                                              !12
else                                                                            !12
   phi  = 4.0_cd*lj_epsilon*(sor12-sor6) + lj_A*rsq + lj_B*r + lj_C             !1.
   dphi = 24.0_cd*lj_epsilon*(-2.0_cd*sor12+sor6)/r  + 2.0_cd*lj_A*r + lj_B     !1.
   phi  = <FILL_functional_form_of_phi(r)>                                      !.2
   dphi = <FILL_functional_form_of_dphi(r)>                                     !.2
endif                                                                           !12
                                                                                !12
end subroutine calc_phi_dphi                                                    !12
                                                                                !12
!-------------------------------------------------------------------------------!12
!                                                                               !12
! Compute energy and forces on particles from the positions.                    !12
!                                                                               !12
!-------------------------------------------------------------------------------!12
integer(c_int) function Compute_Energy_Forces(pkim) bind(c)                     !12
implicit none                                                                   !12
                                                                                !12
!-- Transferred variables                                                       !12
type(c_ptr), intent(in)  :: pkim                                                !12
                                                                                !12
!-- Local variables                                                             !12
real(c_double) :: Rij(DIM)                                                      !12
real(c_double) :: r,Rsqij,phi,dphi,dEidr = 0.0_cd                               !12
integer(c_int) :: i,j,jj,numnei,part_ret,comp_force,comp_enepot,comp_virial, &  !12
                  comp_energy                                                   !12
character (len=80) :: error_message                                             !12
                                                                                !12
!-- KIM variables                                                               !12
integer(c_int), pointer :: N;                 type(c_ptr) :: pN                 !12
real(c_double), pointer :: energy;            type(c_ptr) :: penergy            !12
real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor              !12
real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce             !12
real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot            !12
real(c_double), pointer :: boxSideLengths(:); type(c_ptr) :: pboxSideLengths    !12
real(c_double), pointer :: Rij_list(:,:);     type(c_ptr) :: pRij_list          !12
integer(c_int), pointer :: numContrib;        type(c_ptr) :: pnumContrib        !12
integer(c_int), pointer :: nei1part(:);       type(c_ptr) :: pnei1part          !12
integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies   !12
real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial            !12
character(len=KIM_KEY_STRING_LENGTH) NBC_Method                                 !12
integer(c_int) IterOrLoca                                                       !12
integer(c_int) HalfOrFull                                                       !12
integer(c_int) NBC                                                              !12
integer(c_int) numberContrib                                                    !12
integer(c_int) idum                                                             !12
                                                                                !12
numberContrib = 0 ! initialize                                                  !12
                                                                                !12
! Determine neighbor list boundary condition (NBC)                              !12
! and half versus full mode:                                                    !12
! *****************************                                                 !12
! * HalfOrFull = 1 -- Half                                                      !12
! *            = 2 -- Full                                                      !12
! *****************************                                                 !12
!                                                                               !12
!                                                                               !12
Compute_Energy_Forces = kim_api_get_nbc_method(pkim, NBC_Method)                !12
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then                                !12
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                      !12
                               "kim_api_get_nbc_method", &                      !12
                               Compute_Energy_Forces)                           !12
   return                                                                       !12
endif                                                                           !12
if (index(NBC_Method,"NEIGH_RVEC_H").eq.1) then                                 !12
   NBC = 0                                                                      !12
   HalfOrFull = 1                                                               !12
elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then                             !12
   NBC = 0                                                                      !12
   HalfOrFull = 2                                                               !12
elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then                             !12
   NBC = 1                                                                      !12
   HalfOrFull = 1                                                               !12
elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then                             !12
   NBC = 1                                                                      !12
   HalfOrFull = 2                                                               !12
elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then                                !12
   NBC = 2                                                                      !12
   HalfOrFull = 1                                                               !12
elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then                                !12
   NBC = 2                                                                      !12
   HalfOrFull = 2                                                               !12
elseif (index(NBC_Method,"CLUSTER").eq.1) then                                  !12
   NBC = 3                                                                      !12
   HalfOrFull = 1                                                               !12
else                                                                            !12
   Compute_Energy_Forces = KIM_STATUS_FAIL                                      !12
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                      !12
                               "Unknown NBC method", Compute_Energy_Forces)     !12
   return                                                                       !12
endif                                                                           !12
                                                                                !12
! Determine neighbor list handling mode                                         !12
!                                                                               !12
if (NBC.ne.3) then                                                              !12
   !*****************************                                               !12
   !* IterOrLoca = 1 -- Iterator                                                !12
   !*            = 2 -- Locator                                                 !12
   !*****************************                                               !12
   IterOrLoca = kim_api_get_neigh_mode(pkim, Compute_Energy_Forces)             !12
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then                             !12
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                   !12
                                  "kim_api_get_neigh_mode", &                   !12
                                  Compute_Energy_Forces)                        !12
      return                                                                    !12
   endif                                                                        !12
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then                              !12
      Compute_Energy_Forces = KIM_STATUS_FAIL                                   !12
      write(error_message,'(a,i1)') &                                           !12
         'Unsupported IterOrLoca mode = ',IterOrLoca                            !12
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                   !12
                                  error_message, Compute_Energy_Forces)         !12
      return                                                                    !12
   endif                                                                        !12
else                                                                            !12
   IterOrLoca = 2   ! for CLUSTER NBC                                           !12
endif                                                                           !12
                                                                                !12
! Check to see if we have been asked to compute the forces, energyperpart,      !12
! energy and virial                                                             !12
!                                                                               !12
call kim_api_getm_compute(pkim, Compute_Energy_Forces, &                        !12
     "energy",         comp_energy, 1, &                                        !12
     "forces",         comp_force,  1, &                                        !12
     "particleEnergy", comp_enepot, 1, &                                        !12
     "virial",         comp_virial, 1)                                          !12
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then                                !12
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                      !12
                               "kim_api_getm_compute", Compute_Energy_Forces)   !12
   return                                                                       !12
endif                                                                           !12
                                                                                !12
! Unpack data from KIM object                                                   !12
!                                                                               !12
call kim_api_getm_data(pkim, Compute_Energy_Forces,                           & !12
 "numberOfParticles",           pN,              1,                           & !12
 "particleSpecies",             pparticleSpecies,1,                           & !12
 "coordinates",                 pcoor,           1,                           & !12
 "numberContributingParticles", pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  & !12
 "boxSideLengths",              pboxSideLengths, TRUEFALSE(NBC.eq.2),         & !12
 "energy",                      penergy,         TRUEFALSE(comp_energy.eq.1), & !12
 "forces",                      pforce,          TRUEFALSE(comp_force.eq.1),  & !12
 "particleEnergy",              penepot,         TRUEFALSE(comp_enepot.eq.1), & !12
 "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1))   !12
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then                                !12
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                      !12
                               "kim_api_getm_data_f", Compute_Energy_Forces)    !12
   return                                                                       !12
endif                                                                           !12
                                                                                !12
call c_f_pointer(pN,               N)                                           !12
call c_f_pointer(pparticleSpecies, particleSpecies, [N])                        !12
call c_f_pointer(pcoor,            coor,            [DIM,N])                    !12
if (HalfOrFull.eq.1)  call c_f_pointer(pnumContrib,     numContrib)             !12
if (NBC.eq.2)         call c_f_pointer(pboxSideLengths, boxSideLengths, [DIM])  !12
if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)                 !12
if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])!12
if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])    !12
if (comp_virial.eq.1) call c_f_pointer(pvirial,         virial,         [6])    !12
                                                                                !12
if (HalfOrFull.eq.1) then                                                       !12
   if (NBC.ne.3) then ! non-CLUSTER cases                                       !12
      numberContrib = numContrib                                                !12
   else               ! CLUSTER case                                            !12
      numberContrib = N                                                         !12
   endif                                                                        !12
endif                                                                           !12
                                                                                !12
! Check to be sure that the species are correct                                 !12
!                                                                               !12
Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error                       !12
do i = 1,N                                                                      !12
   if (particleSpecies(i).ne.speccode) then                                     !12
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,      &              !12
                                  "Unexpected species detected", &              !12
                                  Compute_Energy_Forces)                        !12
      return                                                                    !12
   endif                                                                        !12
enddo                                                                           !12
Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok                        !12
                                                                                !12
! Initialize potential energies, forces, virial term                            !12
!                                                                               !12
if (comp_enepot.eq.1) enepot = 0.0_cd                                           !12
if (comp_energy.eq.1) energy = 0.0_cd                                           !12
if (comp_force.eq.1)  force  = 0.0_cd                                           !12
if (comp_virial.eq.1) virial = 0.0_cd                                           !12
                                                                                !12
! Initialize neighbor handling for CLUSTER NBC                                  !12
!                                                                               !12
if (NBC.eq.3) then                                                              !12
   allocate( nei1part(N) )                                                      !12
endif                                                                           !12
                                                                                !12
! Initialize neighbor handling for Iterator mode                                !12
!                                                                               !12
if (IterOrLoca.eq.1) then                                                       !12
   Compute_Energy_Forces = kim_api_get_neigh(pkim,0,0,part_ret,numnei, &        !12
                                             pnei1part,pRij_list)               !12
   ! check for successful initialization                                        !12
   if (Compute_Energy_Forces.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then             !12
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                   !12
                                  "kim_api_get_neigh", Compute_Energy_Forces)   !12
      Compute_Energy_Forces = KIM_STATUS_FAIL                                   !12
      return                                                                    !12
   endif                                                                        !12
endif                                                                           !12
                                                                                !12
!                                                                               !12
!  Compute energy and forces                                                    !12
!                                                                               !12
                                                                                !12
!  Loop over particles and compute energy and forces                            !12
!                                                                               !12
i = 0                                                                           !12
do                                                                              !12
                                                                                !12
   ! Set up neighbor list for next particle for all NBC methods                 !12
   !                                                                            !12
   if (IterOrLoca.eq.1) then ! ITERATOR mode                                    !12
      Compute_Energy_Forces = kim_api_get_neigh(pkim,0,1,part_ret,numnei, &     !12
                                                pnei1part,pRij_list)            !12
      if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit         !12
                             ! incremented past the end of the list,            !12
                             ! terminate loop                                   !12
      if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then                          !12
                             ! some sort of problem, exit                       !12
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                !12
                                     "kim_api_get_neigh",      &                !12
                                     Compute_Energy_Forces)                     !12
         return                                                                 !12
      endif                                                                     !12
                                                                                !12
      i = part_ret                                                              !12
                                                                                !12
   else                      ! LOCATOR mode                                     !12
      i = i + 1                                                                 !12
      if (i.gt.N) exit       ! incremented past end of list,                    !12
                             ! terminate loop                                   !12
      if (NBC.eq.3) then     ! CLUSTER NBC method                               !12
         numnei = N - i      ! number of neighbors in list i+1, ..., N          !12
         nei1part(1:numnei) = (/ (i+jj, jj = 1,numnei) /)                       !12
         Compute_Energy_Forces = KIM_STATUS_OK                                  !12
      else                                                                      !12
         Compute_Energy_Forces = kim_api_get_neigh(pkim,1,i,part_ret,numnei, &  !12
                                                   pnei1part,pRij_list)         !12
         if (Compute_Energy_Forces.ne.KIM_STATUS_OK) then                       !12
                             ! some sort of problem, exit                       !12
            idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &             !12
                                        "kim_api_get_neigh",      &             !12
                                        Compute_Energy_Forces)                  !12
            Compute_Energy_Forces = KIM_STATUS_FAIL                             !12
            return                                                              !12
         endif                                                                  !12
      endif                                                                     !12
   endif                                                                        !12
                                                                                !12
   if (NBC.ne.3) call c_f_pointer(pnei1part, nei1part, [numnei])                !12
   if (NBC.eq.0) call c_f_pointer(pRij_list, Rij_list, [DIM,numnei])            !12
                                                                                !12
   ! Loop over the neighbors of particle i                                      !12
   !                                                                            !12
   do jj = 1, numnei                                                            !12
                                                                                !12
      j = nei1part(jj)                           ! get neighbor ID              !12
                                                                                !12
      ! compute relative position vector                                        !12
      !                                                                         !12
      if (NBC.ne.0) then                         ! all methods except NEIGH_RVEC!12
         Rij(:) = coor(:,j) - coor(:,i)          ! distance vector between i j  !12
      else                                                                      !12
         Rij(:) = Rij_list(:,jj)                                                !12
      endif                                                                     !12
                                                                                !12
      ! apply periodic boundary conditions if required                          !12
      !                                                                         !12
      if (NBC.eq.2) then                                                        !12
         where ( abs(Rij) .gt. 0.5_cd*boxSideLengths )                          !12
                                                 ! periodic boundary conditions !12
            Rij = Rij - sign(boxSideLengths,Rij) ! applied where needed.        !12
         end where                                                              !12
      endif                                                                     !12
                                                                                !12
      ! compute energy and forces                                               !12
      !                                                                         !12
      Rsqij = dot_product(Rij,Rij)               ! compute square distance      !12
      if ( Rsqij .lt. model_cutsq ) then         ! particles are interacting?   !12
                                                                                !12
         r = sqrt(Rsqij)                         ! compute distance             !12
         if (comp_force.eq.1.or.comp_virial.eq.1) then                          !12
            call calc_phi_dphi(r,phi,dphi)       ! compute pair potential       !12
                                                 !   and it derivative          !12
            if ((HalfOrFull.eq.1) .and. &                                       !12
                (j .le. numberContrib)) then     ! HALF mode                    !12
               dEidr = dphi                      !      double contribution     !12
            else                                 ! FULL mode                    !12
               dEidr = 0.5_cd*dphi               !      regular contribution    !12
            endif                                                               !12
         else                                                                   !12
            call calc_phi(r,phi)                 ! compute just pair potential  !12
         endif                                                                  !12
                                                                                !12
         ! contribution to energy                                               !12
         !                                                                      !12
         if (comp_enepot.eq.1) then                                             !12
            enepot(i) = enepot(i) + 0.5_cd*phi   ! accumulate energy            !12
            if ((HalfOrFull.eq.1) .and. &                                       !12
                (j .le. numberContrib)) &        ! HALF mode                    !12
               enepot(j) = enepot(j) + 0.5_cd*phi! (i and j share it)           !12
         endif                                                                  !12
         if (comp_energy.eq.1) then                                             !12
            if ((HalfOrFull.eq.1) .and. &                                       !12
                (j .le. numberContrib)) then     ! HALF mode                    !12
               energy = energy + phi             !    add v to total energy     !12
            else                                 ! FULL mode                    !12
               energy = energy + 0.5_cd*phi      !    add half v to total energy!12
            endif                                                               !12
         endif                                                                  !12
                                                                                !12
         ! contribution to virial tensor, virial(i,j)=r(i)*r(j)*(dV/dr)/r       !12
         !                                                                      !12
         if (comp_virial.eq.1) then                                             !12
            virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r                       !12
            virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r                       !12
            virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r                       !12
            virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r                       !12
            virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r                       !12
            virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r                       !12
         endif                                                                  !12
                                                                                !12
         ! contribution to forces                                               !12
         !                                                                      !12
         if (comp_force.eq.1) then                                              !12
            force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on i       !12
            force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on j       !12
         endif                                                                  !12
                                                                                !12
      endif                                                                     !12
                                                                                !12
   enddo  ! loop on jj                                                          !12
                                                                                !12
enddo  ! infinite do loop (terminated by exit statements above)                 !12
                                                                                !12
! Free temporary storage                                                        !12
!                                                                               !12
if (NBC.eq.3) deallocate( nei1part )                                            !12
                                                                                !12
! Everything is great                                                           !12
!                                                                               !12
Compute_Energy_Forces = KIM_STATUS_OK                                           !12
return                                                                          !12
                                                                                !12
end function Compute_Energy_Forces                                              !12
                                                                                !12
end module <FILL_model_name>                                                    !12
                                                                                !12
!-------------------------------------------------------------------------------!12
!                                                                               !12
! Model initialization routine (REQUIRED)                                       !12
!                                                                               !12
!-------------------------------------------------------------------------------!12
integer(c_int) function model_init(pkim) bind(c)                                !12
use, intrinsic :: iso_c_binding                                                 !12
use <FILL_model_name>                                                           !12
use KIM_API_F03                                                                 !12
implicit none                                                                   !12
                                                                                !12
!-- Transferred variables                                                       !12
type(c_ptr), intent(in) :: pkim                                                 !12
                                                                                !12
!-- Local variables                                                             !12
integer(c_int), parameter :: one=1                                              !12
integer(c_int) ier, idum                                                        !12
                                                                                !12
!-- KIM variables                                                               !12
real(c_double), pointer :: cutoff; type(c_ptr) :: pcutoff                       !12
                                                                                !12
! store pointer to compute function in KIM object                               !12
ier = kim_api_set_method(pkim,"compute",one,c_funloc(Compute_Energy_Forces))    !12
if (ier.lt.KIM_STATUS_OK) then                                                  !12
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                      !12
                               "kim_api_set_method", ier)                       !12
   goto 42                                                                      !12
endif                                                                           !12
                                                                                !12
! store model cutoff in KIM object                                              !12
pcutoff =  kim_api_get_data(pkim,"cutoff",ier)                                  !12
if (ier.lt.KIM_STATUS_OK) then                                                  !12
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &                      !12
                               "kim_api_get_data", ier)                         !12
   goto 42                                                                      !12
endif                                                                           !12
call c_f_pointer(pcutoff, cutoff)                                               !12
cutoff = model_cutoff                                                           !12
                                                                                !12
ier = KIM_STATUS_OK                                                             !12
42 continue                                                                     !12
model_init = ier                                                                !12
return                                                                          !12
                                                                                !12
end function model_init                                                         !12
