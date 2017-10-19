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
! Copyright (c) 2013--2017, Regents of the University of Minnesota.
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
implicit none

save
private
public Compute_Energy_Forces, &
       model_refresh_func, &
       model_destroy_func, &
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
  real(c_double) :: cutoff(1)
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

!-------------------------------------------------------------------------------
!
! Compute energy and forces on particles from the positions.
!
!-------------------------------------------------------------------------------
#include "kim_model_compute_log_macros.fd"
subroutine Compute_Energy_Forces(model_compute, ierr) bind(c)
use kim_log_verbosity_module
use kim_model_compute_module
use kim_argument_name_module
implicit none

!-- Transferred variables
type(kim_model_compute_type), intent(in) :: model_compute
integer(c_int), intent(out) :: ierr

!-- Local variables
real(c_double) :: Rij(DIM)
real(c_double) :: r,Rsqij,phi,dphi,dEidr = 0.0_cd
integer(c_int) :: i,j,jj,numnei,comp_force,comp_enepot,comp_virial, comp_energy
integer(c_int) :: ierr2

!-- KIM variables
integer(c_int), pointer :: N
real(c_double), pointer :: energy
real(c_double), pointer :: coor(:,:)
real(c_double), pointer :: force(:,:)
real(c_double), pointer :: enepot(:)
integer(c_int), pointer :: nei1part(:)
integer(c_int), pointer :: particleSpeciesCodes(:)
integer(c_int), pointer :: particleContributing(:)
real(c_double), pointer :: virial(:)

kim_log_file = __FILE__

! Unpack data from KIM object
!
ierr = 0
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_number_of_particles, N, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_particle_species_codes, n, particleSpeciesCodes, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_particle_contributing, n, particleContributing, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_coordinates, dim, n, coor, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_partial_energy, energy, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_partial_forces, dim, n, force, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_partial_particle_energy, n, enepot, ierr2)
ierr = ierr + ierr2
call kim_model_compute_get_argument_pointer(model_compute, &
  kim_argument_name_partial_virial, 6, virial, ierr2)
ierr = ierr + ierr2
if (ierr /= 0) then
  kim_log_message = "get data"
  LOG_ERROR()
  return
endif

! Check to see if we have been asked to compute the forces, energyperpart,
! energy and virial
!
if (associated(energy)) then
  comp_energy = 1
else
  comp_energy = 0
end if
if (associated(force)) then
  comp_force = 1
else
  comp_force = 0
end if
if (associated(enepot)) then
  comp_enepot = 1
else
  comp_enepot = 0
end if
if (associated(virial)) then
  comp_virial = 1
else
  comp_virial = 0
end if

! Check to be sure that the species are correct
!
ierr = 1 ! assume an error
do i = 1,N
  if (particleSpeciesCodes(i).ne.speccode) then
    kim_log_message = "Unexpected species code detected"
    LOG_ERROR()
    return
  endif
enddo
ierr = 0 ! everything is ok

! Initialize potential energies, forces, virial term
!
if (comp_enepot.eq.1) enepot = 0.0_cd
if (comp_energy.eq.1) energy = 0.0_cd
if (comp_force.eq.1)  force  = 0.0_cd
if (comp_virial.eq.1) virial = 0.0_cd

!
!  Compute energy and forces
!

!  Loop over particles and compute energy and forces
!
do i = 1, N
  if (particleContributing(i) == 1) then
    ! Set up neighbor list for next particle
    call kim_model_compute_get_neighbor_list( &
      model_compute, 1, i, numnei, nei1part, ierr)
    if (ierr /= 0) then
      ! some sort of problem, exit
      kim_log_message = "GetNeighborList failed"
      LOG_ERROR()
      ierr = 1
      return
    endif

    ! Loop over the neighbors of particle i
    !
    do jj = 1, numnei

      j = nei1part(jj)                           ! get neighbor ID

      ! compute relative position vector
      !
      Rij(:) = coor(:,j) - coor(:,i)          ! distance vector between i j

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)               ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then         ! particles are interacting?

        r = sqrt(Rsqij)                         ! compute distance
        if (comp_force.eq.1.or.comp_virial.eq.1) then
          call calc_phi_dphi(r,phi,dphi)       ! compute pair potential
          !   and it derivative
          dEidr = 0.5_cd*dphi               !      regular contribution
        else
          call calc_phi(r,phi)                 ! compute just pair potential
        endif

        ! contribution to energy
        !
        if (comp_enepot.eq.1) then
          enepot(i) = enepot(i) + 0.5_cd*phi   ! accumulate energy
        endif
        if (comp_energy.eq.1) then
          energy = energy + 0.5_cd*phi      !    add half v to total energy
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
          force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on i
          force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on j
        endif

      endif

    enddo  ! loop on jj

  endif  ! if particleContributing

enddo  ! do i

! Everything is great
!
ierr = 0
return

end subroutine Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model destroy routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_destroy_log_macros.fd"
subroutine model_destroy_func(model_destroy, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module
  use kim_model_destroy_module
  implicit none

  !-- Transferred variables
  type(kim_model_destroy_type), intent(inout) :: model_destroy
  integer(c_int), intent(out) :: ierr

  type(buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  kim_log_file = __FILE__

  call kim_model_destroy_get_model_buffer_pointer(model_destroy, pbuf)
  call c_f_pointer(pbuf, buf)
  kim_log_message = "deallocating model buffer"
  LOG_INFORMATION()
  deallocate(buf)
  ierr = 0  ! everything is good
end subroutine model_destroy_func

!-------------------------------------------------------------------------------
!
! Model refresh routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_refresh_log_macros.fd"
subroutine model_refresh_func(model_refresh, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module
  use kim_model_refresh_module
  implicit none

  !-- Transferred variables
  type(kim_model_refresh_type), intent(inout) :: model_refresh
  integer(c_int), intent(out) :: ierr

  type(buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  kim_log_file = __FILE__

  call kim_model_refresh_get_model_buffer_pointer(model_refresh, pbuf)
  call c_f_pointer(pbuf, buf)

  kim_log_message = "Resettings influence distance and cutoffs"
  LOG_INFORMATION()
  call kim_model_refresh_set_influence_distance_pointer( &
    model_refresh, buf%cutoff(1))
  call kim_model_refresh_set_neighbor_list_cutoffs_pointer( &
    model_refresh, 1, buf%cutoff)

  ierr = 0  ! everything is good
end subroutine model_refresh_func

end module ex_model_Ar_P_MLJ_F03

!-------------------------------------------------------------------------------
!
! Model create routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_create_log_macros.fd"
subroutine model_create_routine(model_create, requested_length_unit, &
  requested_energy_unit, requested_charge_unit, requested_temperature_unit, &
  requested_time_unit, ierr) bind(c)
use, intrinsic :: iso_c_binding
use ex_model_Ar_P_MLJ_F03
use kim_log_verbosity_module
use kim_numbering_module
use kim_species_name_module
use kim_argument_name_module
use kim_support_status_module
use kim_unit_system_module
use kim_model_create_module
use kim_language_name_module
implicit none

!-- Transferred variables
type(kim_model_create_type), intent(inout) :: model_create
type(kim_length_unit_type), intent(in) :: requested_length_unit
type(kim_energy_unit_type), intent(in) :: requested_energy_unit
type(kim_charge_unit_type), intent(in) :: requested_charge_unit
type(kim_temperature_unit_type), intent(in) :: requested_temperature_unit
type(kim_time_unit_type), intent(in) :: requested_time_unit
integer(c_int), intent(out) :: ierr

!-- KIM variables
integer(c_int) :: ierr2
type(buffer_type), pointer :: buf

kim_log_file = __FILE__

ierr2 = 0

! register numbering
call kim_model_create_set_model_numbering(model_create, &
  kim_numbering_one_based, ierr2);
ierr = ierr + ierr2

! register species
call kim_model_create_set_species_code(model_create, &
  kim_species_name_ar, speccode, ierr2)
ierr = ierr + ierr2

! register arguments
call kim_model_create_set_argument_support_status( &
  model_create, kim_argument_name_partial_energy, &
  kim_support_status_optional, ierr2)
ierr = ierr + ierr2
call kim_model_create_set_argument_support_status( &
  model_create, kim_argument_name_partial_forces, &
  kim_support_status_optional, ierr2)
ierr = ierr + ierr2
call kim_model_create_set_argument_support_status( &
  model_create, kim_argument_name_partial_particle_energy, &
  kim_support_status_optional, ierr2)
ierr = ierr + ierr2
call kim_model_create_set_argument_support_status( &
  model_create, kim_argument_name_partial_virial, &
  kim_support_status_optional, ierr2)
ierr = ierr + ierr2

! register call backs
! NONE

! register function pointers
call kim_model_create_set_compute_pointer(model_create, &
  kim_language_name_fortran, c_funloc(Compute_Energy_Forces), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_destroy_pointer(model_create, &
  kim_language_name_fortran, c_funloc(model_destroy_func), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_refresh_pointer( &
  model_create, kim_language_name_fortran, &
  c_funloc(model_refresh_func), ierr2)
ierr = ierr + ierr2

! set units
call kim_model_create_set_units(model_create, &
  kim_length_unit_a, &
  kim_energy_unit_ev, &
  kim_charge_unit_unused, &
  kim_temperature_unit_unused, &
  kim_time_unit_unused, &
  ierr2)
ierr = ierr + ierr2

! store model cutoff in KIM object
allocate( buf )
buf%cutoff = model_cutoff
call kim_model_create_set_model_buffer_pointer(model_create, &
  c_loc(buf))

! register influence distance and cutoffs
call kim_model_create_set_influence_distance_pointer( &
  model_create, buf%cutoff(1))

call kim_model_create_set_neighbor_list_cutoffs_pointer(model_create, 1, &
  buf%cutoff)

if (ierr /= 0) then
  ierr = 1
  kim_log_message = "Unable to successfully initialize model"
  LOG_ERROR()
endif

return

end subroutine model_create_routine
