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
! Copyright (c) 2013--2018, Regents of the University of Minnesota.
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
!**  MODULE ex_model_Ar_P_MLJ_F03
!**
!**  Modified Lennard-Jones pair potential (with smooth cutoff) model for Ar
!**
!**  Reference: Ashcroft and Mermin
!**
!**  Language: Fortran 2003
!**
!****************************************************************************


module ex_model_Ar_P_MLJ_F03

use, intrinsic :: iso_c_binding
use kim_model_headers_module
implicit none

save
private
public &!Compute_Energy_Forces, &
       model_compute_arguments_create, &
       model_compute_arguments_destroy, &
       model_cutoff, &
       speccode, &
       buffer_type

! Below are the definitions and values of all Model parameters
integer(c_int), parameter :: cd = c_double  ! used for literal constants
integer(c_int), parameter :: DIM = 3  ! dimensionality of space
integer(c_int), parameter :: speccode = 1  ! internal species code
real(c_double), parameter :: model_cutoff = 8.15_cd ! cutoff radius
                                                    ! in angstroms
real(c_double), parameter :: model_cutsq = model_cutoff**2

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

type, bind(c) :: buffer_type
  real(c_double) :: influence_distance
  real(c_double) :: cutoff(1)
  integer(c_int) :: padding_neighbor_hints(1)
  integer(c_int) :: half_list_hints(1)
end type buffer_type

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

!!-------------------------------------------------------------------------------
!!
!! Compute energy and forces on particles from the positions.
!!
!!-------------------------------------------------------------------------------
!integer(c_int) function Compute_Energy_Forces(pkim) bind(c)
!implicit none
!
!!-- Transferred variables
!type(c_ptr), intent(in)  :: pkim
!
!!-- Local variables
!real(c_double) :: Rij(DIM)
!real(c_double) :: r,Rsqij,phi,dphi,dEidr = 0.0_cd
!integer(c_int) :: i,j,jj,numnei,part_ret,comp_force,comp_enepot,comp_virial, &
!                  comp_energy
!character (len=80) :: error_message
!
!!-- KIM variables
!integer(c_int), pointer :: N;                 type(c_ptr) :: pN
!real(c_double), pointer :: energy;            type(c_ptr) :: penergy
!real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
!real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
!real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
!real(c_double), pointer :: Rij_list(:,:);     type(c_ptr) :: pRij_list
!integer(c_int), pointer :: nei1part(:);       type(c_ptr) :: pnei1part
!integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies
!real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial
!integer(c_int) idum
!
!
!! Check to see if we have been asked to compute the forces, energyperpart,
!! energy and virial
!!
!call kim_api_getm_compute(pkim, Compute_Energy_Forces, &
!     "energy",         comp_energy, 1, &
!     "forces",         comp_force,  1, &
!     "particleEnergy", comp_enepot, 1, &
!     "virial",         comp_virial, 1)
!if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
!   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
!                               "kim_api_getm_compute", Compute_Energy_Forces)
!   return
!endif
!
!! Unpack data from KIM object
!!
!call kim_api_getm_data(pkim, Compute_Energy_Forces,                           &
! "numberOfParticles",           pN,              1,                           &
! "particleSpecies",             pparticleSpecies,1,                           &
! "coordinates",                 pcoor,           1,                           &
! "energy",                      penergy,         TRUEFALSE(comp_energy.eq.1), &
! "forces",                      pforce,          TRUEFALSE(comp_force.eq.1),  &
! "particleEnergy",              penepot,         TRUEFALSE(comp_enepot.eq.1), &
! "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1))
!if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
!   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
!                               "kim_api_getm_data_f", Compute_Energy_Forces)
!   return
!endif
!
!call c_f_pointer(pN,               N)
!call c_f_pointer(pparticleSpecies, particleSpecies, [N])
!call c_f_pointer(pcoor,            coor,            [DIM,N])
!if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)
!if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])
!if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])
!if (comp_virial.eq.1) call c_f_pointer(pvirial,         virial,         [6])
!
!
!! Check to be sure that the species are correct
!!
!Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
!do i = 1,N
!   if (particleSpecies(i).ne.speccode) then
!      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,      &
!                                  "Unexpected species detected", &
!                                  Compute_Energy_Forces)
!      return
!   endif
!enddo
!Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok
!
!! Initialize potential energies, forces, virial term
!!
!if (comp_enepot.eq.1) enepot = 0.0_cd
!if (comp_energy.eq.1) energy = 0.0_cd
!if (comp_force.eq.1)  force  = 0.0_cd
!if (comp_virial.eq.1) virial = 0.0_cd
!
!
!!
!!  Compute energy and forces
!!
!
!!  Loop over particles and compute energy and forces
!!
!do i=1,N
!   Compute_Energy_Forces = kim_api_get_neigh(pkim,1,i,part_ret,numnei, &
!                                             pnei1part,pRij_list)
!   if (Compute_Energy_Forces.ne.KIM_STATUS_OK) then
!     ! some sort of problem, exit
!     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
!                                 "kim_api_get_neigh",      &
!                                 Compute_Energy_Forces)
!     Compute_Energy_Forces = KIM_STATUS_FAIL
!     return
!   endif
!
!   call c_f_pointer(pnei1part, nei1part, [numnei])
!
!   ! Loop over the neighbors of particle i
!   !
!   do jj = 1, numnei
!
!      j = nei1part(jj)                           ! get neighbor ID
!
!      ! compute relative position vector
!      !
!      Rij(:) = coor(:,j) - coor(:,i)          ! distance vector between i j
!
!      ! compute energy and forces
!      !
!      Rsqij = dot_product(Rij,Rij)               ! compute square distance
!      if ( Rsqij .lt. model_cutsq ) then         ! particles are interacting?
!
!         r = sqrt(Rsqij)                         ! compute distance
!         if (comp_force.eq.1.or.comp_virial.eq.1) then
!            call calc_phi_dphi(r,phi,dphi)       ! compute pair potential
!                                                 !   and it derivative
!            dEidr = 0.5_cd*dphi
!         else
!            call calc_phi(r,phi)                 ! compute just pair potential
!         endif
!
!         ! contribution to energy
!         !
!         if (comp_enepot.eq.1) then
!            enepot(i) = enepot(i) + 0.5_cd*phi   ! accumulate energy
!         endif
!         if (comp_energy.eq.1) then
!            energy = energy + 0.5_cd*phi
!         endif
!
!         ! contribution to virial tensor, virial(i,j)=r(i)*r(j)*(dV/dr)/r
!         !
!         if (comp_virial.eq.1) then
!            virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r
!            virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r
!            virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r
!            virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r
!            virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r
!            virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r
!         endif
!
!         ! contribution to forces
!         !
!         if (comp_force.eq.1) then
!            force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on i
!            force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on j
!         endif
!
!      endif
!
!   enddo  ! loop on jj
!
!enddo
!
!! Everything is great
!!
!Compute_Energy_Forces = KIM_STATUS_OK
!return
!
!end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model compute arguments create routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_compute_arguments_create_log_macros.fd"
subroutine model_compute_arguments_create(model_compute_handle, &
  model_compute_arguments_create_handle, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_arguments_create_module, &
    log_entry=>kim_model_compute_arguments_create_log_entry
  implicit none

  !-- Transferred variables
  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_create_handle_type), intent(inout) :: &
    model_compute_arguments_create_handle
  integer(c_int), intent(out) :: ierr

  integer(c_int) :: ierr2
  character(kind=c_char, len=100000) compute_arguments_string

  ierr = 0
  ierr2 = 0

  ! register arguments
  call kim_model_compute_arguments_create_set_argument_support_status( &
    model_compute_arguments_create_handle, &
    kim_compute_argument_name_partial_energy, &
    kim_support_status_optional, ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_create_set_argument_support_status( &
    model_compute_arguments_create_handle, &
    kim_compute_argument_name_partial_forces, &
    kim_support_status_optional, ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_create_set_argument_support_status( &
    model_compute_arguments_create_handle, &
    kim_compute_argument_name_partial_particle_energy, &
    kim_support_status_optional, ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_create_set_argument_support_status( &
    model_compute_arguments_create_handle, &
    kim_compute_argument_name_partial_virial, &
    kim_support_status_optional, ierr2)
  ierr = ierr + ierr2

  ! register call backs
  ! NONE

  if (ierr /= 0) then
    ierr = 1
    kim_log_message = "Unable to successfully create compute_arguments object"
    LOG_ERROR()
  else
    call kim_model_compute_arguments_create_string( &
      model_compute_arguments_create_handle, compute_arguments_string)
    print '(A)', trim(compute_arguments_string)
  endif

  return
end subroutine model_compute_arguments_create

!-------------------------------------------------------------------------------
!
! Model compute arguments destroy routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_compute_arguments_destroy_log_macros.fd"
subroutine model_compute_arguments_destroy(model_compute_handle, &
  model_compute_arguments_destroy_handle, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_arguments_destroy_module, &
    log_entry=>kim_model_compute_arguments_destroy_log_entry
  implicit none

  !-- Transferred variables
  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_destroy_handle_type), intent(inout) :: &
    model_compute_arguments_destroy_handle
  integer(c_int), intent(out) :: ierr

  integer(c_int) :: ierr2

  ierr = 0
  ierr2 = 0

  ! nothing to do

  return
end subroutine model_compute_arguments_destroy

end module ex_model_Ar_P_MLJ_F03

!-------------------------------------------------------------------------------
!
! Model create routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_create_log_macros.fd"
subroutine model_create_routine(model_create_handle, requested_length_unit, &
  requested_energy_unit, requested_charge_unit, requested_temperature_unit, &
  requested_time_unit, ierr) bind(c)
use, intrinsic :: iso_c_binding
use ex_model_Ar_P_MLJ_F03
use kim_model_headers_module
implicit none

!-- Transferred variables
type(kim_model_create_handle_type), intent(inout) :: model_create_handle
type(kim_length_unit_type), intent(in) :: requested_length_unit
type(kim_energy_unit_type), intent(in) :: requested_energy_unit
type(kim_charge_unit_type), intent(in) :: requested_charge_unit
type(kim_temperature_unit_type), intent(in) :: requested_temperature_unit
type(kim_time_unit_type), intent(in) :: requested_time_unit
integer(c_int), intent(out) :: ierr

!-- KIM variables
integer(c_int) :: ierr2
type(buffer_type), pointer :: buf
character(kind=c_char, len=100000) model_create_string

kim_log_file = __FILE__

ierr = 0
ierr2 = 0

! set units
call kim_model_create_set_units(model_create_handle, &
  kim_length_unit_a, &
  kim_energy_unit_ev, &
  kim_charge_unit_unused, &
  kim_temperature_unit_unused, &
  kim_time_unit_unused, &
  ierr2)
ierr = ierr + ierr2

! register species
call kim_model_create_set_species_code(model_create_handle, &
  kim_species_name_ar, speccode, ierr2)
ierr = ierr + ierr2

! register numbering
call kim_model_create_set_model_numbering(model_create_handle, &
  kim_numbering_one_based, ierr2);
ierr = ierr + ierr2

! register function pointers
call kim_model_create_set_compute_pointer(model_create_handle, &
  kim_language_name_fortran, c_funloc(kim_model_create_string), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_compute_arguments_create_pointer( &
  model_create_handle, kim_language_name_fortran, &
  c_funloc(model_compute_arguments_create), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_compute_arguments_destroy_pointer( &
  model_create_handle, kim_language_name_fortran, &
  c_funloc(model_compute_arguments_destroy), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_destroy_pointer(model_create_handle, &
  kim_language_name_fortran, c_funloc(kim_model_create_string), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_refresh_pointer( &
  model_create_handle, kim_language_name_fortran, &
  c_funloc(kim_model_create_string), ierr2)
ierr = ierr + ierr2

! allocate buffer
allocate( buf )

! store model buffer in KIM object
call kim_model_create_set_model_buffer_pointer(model_create_handle, &
  c_loc(buf))

! set buffer values
buf%influence_distance = model_cutoff
buf%cutoff = model_cutoff
buf%padding_neighbor_hints = 1
buf%half_list_hints = 0

! register influence distance
call kim_model_create_set_influence_distance_pointer( &
  model_create_handle, buf%influence_distance)

! register cutoff
call kim_model_create_set_neighbor_list_pointers(model_create_handle, &
  1, buf%cutoff, buf%padding_neighbor_hints, buf%half_list_hints)

if (ierr /= 0) then
  ierr = 1
  deallocate( buf )
  kim_log_message = "Unable to successfully initialize model"
  LOG_ERROR()
endif

call kim_model_create_string(model_create_handle, model_create_string)
print '(A)', trim(model_create_string)
stop

return

end subroutine model_create_routine
