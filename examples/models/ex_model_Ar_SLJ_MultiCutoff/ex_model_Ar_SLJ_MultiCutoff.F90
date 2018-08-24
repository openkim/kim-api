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
!    Ellad B. Tadmor
!

!****************************************************************************
!**
!**  MODULE ex_model_Ar_SLJ_MultiCutoff
!**
!** Spring-modified Lennard-Jones (SLJ) pair potential model for Ar
!**
!**  V = 0.5 \sum_i \sum_j eps_i eps_j 4 [ (sig/r_ij)^12 - (sig/r_ij)^6 ] (1)
!**
!**  where
!**        eps_i = 0.5 \sum_k spring (r_ik)^2                             (2)
!**
!**  See README for details.
!**
!**  Language: Fortran 2003
!**
!**  Author: Ellad B. Tadmor
!**
!**  Date: August 23, 2018
!**
!****************************************************************************


module ex_model_Ar_SLJ_MultiCutoff

use, intrinsic :: iso_c_binding
use kim_model_headers_module
implicit none

save
private
public Compute_Energy_Forces, &
       model_refresh_func, &
       model_destroy_func, &
       model_compute_arguments_create, &
       model_compute_arguments_destroy, &
       model_cutoff1, &
       model_cutoff2, &
       speccode, &
       buffer_type

! Below are the definitions and values of all Model parameters
integer(c_int), parameter :: cd = c_double  ! used for literal constants
integer(c_int), parameter :: DIM = 3  ! dimensionality of space
integer(c_int), parameter :: speccode = 1  ! internal species code

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
real(c_double), parameter :: lj_spring = 0.00051226_cd
real(c_double), parameter :: lj_sigma  = 5.26_cd/1.74724_cd
                                         ! experimental fcc lattice constant is a0=5.26
real(c_double), parameter :: model_cutoff1 = 1.25*lj_sigma
                                             ! short-range nearest neighbor for fcc
real(c_double), parameter :: model_cutoff2 = 2.25*lj_sigma
                                             ! long-range third neighbor for fcc
real(c_double), parameter :: model_cutsq1 = model_cutoff1**2
real(c_double), parameter :: model_cutsq2 = model_cutoff2**2

type, bind(c) :: buffer_type
  real(c_double) :: influence_distance
  real(c_double) :: cutoff(2)
  integer(c_int) :: &
    model_will_not_request_neighbors_of_noncontributing_particles(2)
end type buffer_type

contains

!-------------------------------------------------------------------------------
!
!  Calculate Lennard-Jones potential phi(r) and, if requested,
!  its derivative dphi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(r,phi,dphi,calc_deriv)
implicit none

!-- Transferred variables
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi
real(c_double), intent(out) :: dphi
logical,        intent(in)  :: calc_deriv

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = lj_sigma/r      !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff2) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   if (calc_deriv) dphi   = 0.0_cd
else
   phi  = 4.0_cd*(sor12-sor6)
   if (calc_deriv) dphi = 24.0_cd*(-2.0_cd*sor12+sor6)/r
endif

end subroutine calc_phi

!-------------------------------------------------------------------------------
!
!  Calculate short-range linear spring-based energy amplitude for `atom`
!
!-------------------------------------------------------------------------------
subroutine calc_spring_energyamp(model_compute_arguments_handle, atom, coor, &
                                 eps, ierr)
implicit none

!-- Transferred variables
type(kim_model_compute_arguments_handle_type), &
                         intent(in)  :: model_compute_arguments_handle
integer(c_int),          intent(in)  :: atom
real(c_double),          intent(in)  :: coor(:,:)
real(c_double),          intent(out) :: eps
integer(c_int),          intent(out) :: ierr

!-- Local variables
integer(c_int) k,kk
real(c_double) Rrel(DIM)
real(c_double) Rsqrel
integer(c_int) numneishort
integer(c_int), pointer :: neishort(:)

! Get short-range neighbors of `atom`
call kim_model_compute_arguments_get_neighbor_list( &
  model_compute_arguments_handle, 1, atom, numneishort, neishort, ierr)
if (ierr /= 0) return

eps = 0.0_cd
do kk = 1, numneishort
  k = neishort(kk)
  Rrel(:) = coor(:,k) - coor(:,atom)
  Rsqrel = dot_product(Rrel,Rrel)
  if (Rsqrel .lt. model_cutsq1) eps = eps + Rsqrel
enddo
eps = 0.5_cd*lj_spring*eps

end subroutine calc_spring_energyamp

!-------------------------------------------------------------------------------
!
!  Calculate short-range linear spring-based contribution to force
!
!-------------------------------------------------------------------------------
subroutine calc_spring_force(model_compute_arguments_handle, atom, coor, eps, &
                             phi, force, ierr)
implicit none

!-- Transferred variables
type(kim_model_compute_arguments_handle_type), &
                         intent(in)     :: model_compute_arguments_handle
integer(c_int),          intent(in)     :: atom
real(c_double),          intent(in)     :: coor(:,:)
real(c_double),          intent(in)     :: eps
real(c_double),          intent(in)     :: phi
real(c_double),          intent(inout)  :: force(:,:)
integer(c_int),          intent(out)    :: ierr

!-- Local variables
integer(c_int) k,kk
real(c_double) Rrel(DIM), dforce(DIM)
real(c_double) Rsqrel
integer(c_int) numneishort
integer(c_int), pointer :: neishort(:)

! Get short-range neighbors of `atom`
call kim_model_compute_arguments_get_neighbor_list( &
  model_compute_arguments_handle, 1, atom, numneishort, neishort, ierr)
if (ierr /= 0) return

! Add contribution to force on `atom` and its near neighbors that contribute
! to the spring term
do kk = 1, numneishort
  k = neishort(kk)
  Rrel(:) = coor(:,k) - coor(:,atom)
  Rsqrel = dot_product(Rrel,Rrel)
  if (Rsqrel .lt. model_cutsq1) then
    dforce(:) = 0.5_cd*eps*lj_spring*Rrel(:)*phi
    force(:,atom) = force(:,atom) + dforce(:) ! accumulate force on atom
    force(:,k)    = force(:,k) - dforce(:)    ! accumulate force on k
  endif
enddo

end subroutine calc_spring_force


!-------------------------------------------------------------------------------
!
! Compute energy and forces on particles from the positions.
!
!-------------------------------------------------------------------------------
#include "kim_model_compute_log_macros.fd"
subroutine Compute_Energy_Forces(model_compute_handle, &
  model_compute_arguments_handle, ierr) bind(c)
implicit none

!-- Transferred variables
type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
type(kim_model_compute_arguments_handle_type), intent(in) :: &
  model_compute_arguments_handle
integer(c_int), intent(out) :: ierr

!-- Local variables
real(c_double) :: Rij(DIM),dforce(DIM)
real(c_double) :: r,Rsqij,phi,dphi,dEidr,epsi,epsj
integer(c_int) :: i,j,jj,comp_force,comp_enepot,comp_energy
!integer(c_int) :: comp_virial
integer(c_int) :: numnei
integer(c_int) :: ierr2
logical        :: calc_deriv

!-- KIM variables
integer(c_int), pointer :: N
real(c_double), pointer :: energy
real(c_double), pointer :: coor(:,:)
real(c_double), pointer :: force(:,:)
real(c_double), pointer :: enepot(:)
integer(c_int), pointer :: nei1part(:)
integer(c_int), pointer :: particleSpeciesCodes(:)
integer(c_int), pointer :: particleContributing(:)
!real(c_double), pointer :: virial(:)

kim_log_file = __FILE__

! Unpack data from KIM object
!
ierr = 0
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_number_of_particles, N, ierr2)
ierr = ierr + ierr2
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_particle_species_codes, n, particleSpeciesCodes, &
  ierr2)
ierr = ierr + ierr2
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_particle_contributing, n, particleContributing, &
  ierr2)
ierr = ierr + ierr2
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_coordinates, dim, n, coor, ierr2)
ierr = ierr + ierr2
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_partial_energy, energy, ierr2)
ierr = ierr + ierr2
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_partial_forces, dim, n, force, ierr2)
ierr = ierr + ierr2
call kim_model_compute_arguments_get_argument_pointer( &
  model_compute_arguments_handle, &
  kim_compute_argument_name_partial_particle_energy, n, enepot, ierr2)
ierr = ierr + ierr2
!call kim_model_compute_arguments_get_argument_pointer( &
!  model_compute_arguments_handle, &
!  kim_compute_argument_name_partial_virial, 6, virial, ierr2)
!ierr = ierr + ierr2
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
!if (associated(virial)) then
!  comp_virial = 1
!else
!  comp_virial = 0
!end if
calc_deriv = comp_force.eq.1 !.or.comp_virial.eq.1

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
!if (comp_virial.eq.1) virial = 0.0_cd
if (calc_deriv)       dEidr  = 0.0_cd

!
!  Compute energy and forces
!

!  Loop over particles and compute energy and forces
!
do i = 1, N
  if (particleContributing(i) == 1) then
    ! Set up neighbor list for next particle
    call kim_model_compute_arguments_get_neighbor_list( &
      model_compute_arguments_handle, 2, i, numnei, nei1part, ierr)
    if (ierr /= 0) then
      ! some sort of problem, exit
      kim_log_message = "GetNeighborList failed"
      LOG_ERROR()
      ierr = 1
      return
    endif

   ! Get short range contribution for atom i to energy amplitude
   call calc_spring_energyamp(model_compute_arguments_handle, i, coor, epsi, ierr)
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

      ! Get short range contribution for atom j to energy amplitude
      call calc_spring_energyamp(model_compute_arguments_handle, j, coor, epsj, ierr)
      if (ierr /= 0) then
        ! some sort of problem, exit
        kim_log_message = "GetNeighborList failed"
        LOG_ERROR()
        ierr = 1
        return
      endif

      ! compute relative position vector
      !
      Rij(:) = coor(:,j) - coor(:,i)          ! distance vector between i j

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)               ! compute square distance
      if ( Rsqij .lt. model_cutsq2 ) then        ! particles are interacting?

        r = sqrt(Rsqij)                          ! compute distance
        call calc_phi(r,phi,dphi,calc_deriv)     ! compute pair potential and deriv
        if (calc_deriv) dEidr = 0.5_cd*dphi

        ! contribution to energy
        !
        if (comp_enepot.eq.1) then
          enepot(i) = enepot(i) + 0.5_cd*epsi*epsj*phi   ! accumulate energy
        endif
        if (comp_energy.eq.1) then
          energy = energy + 0.5_cd*epsi*epsj*phi
        endif

!!@@@@@@@@@@@@@@@@@@@@ NOT FIXED YET
!        ! contribution to virial tensor, virial(i,j)=r(i)*r(j)*(dV/dr)/r
!        !
!        if (comp_virial.eq.1) then
!          virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r
!          virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r
!          virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r
!          virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r
!          virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r
!          virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r
!        endif
!!@@@@@@@@@@@@@@@@@@@@


        ! contribution to forces
        !
        if (comp_force.eq.1) then
          ! Contribution due to short range neighbors of i
          call calc_spring_force(model_compute_arguments_handle, i, coor, epsj, &
                                 phi, force, ierr)
          if (ierr /= 0) then
            ! some sort of problem, exit
            kim_log_message = "GetNeighborList failed"
            LOG_ERROR()
            ierr = 1
            return
          endif
          ! Contribution due to short range neighbors of j
          call calc_spring_force(model_compute_arguments_handle, j, coor, epsi, &
                                 phi, force, ierr)
          if (ierr /= 0) then
            ! some sort of problem, exit
            kim_log_message = "GetNeighborList failed"
            LOG_ERROR()
            ierr = 1
            return
          endif
          ! Contribution due to deriv of LJ term
          dforce(:) = epsi*epsj*dEidr*Rij(:)/r
          force(:,i) = force(:,i) + dforce(:) ! accumulate force on i
          force(:,j) = force(:,j) - dforce(:) ! accumulate force on j
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
subroutine model_destroy_func(model_destroy_handle, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  implicit none

  !-- Transferred variables
  type(kim_model_destroy_handle_type), intent(inout) :: model_destroy_handle
  integer(c_int), intent(out) :: ierr

  type(buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  kim_log_file = __FILE__

  call kim_model_destroy_get_model_buffer_pointer(model_destroy_handle, pbuf)
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
subroutine model_refresh_func(model_refresh_handle, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  implicit none

  !-- Transferred variables
  type(kim_model_refresh_handle_type), intent(inout) :: model_refresh_handle
  integer(c_int), intent(out) :: ierr

  type(buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  kim_log_file = __FILE__

  call kim_model_refresh_get_model_buffer_pointer(model_refresh_handle, pbuf)
  call c_f_pointer(pbuf, buf)

  kim_log_message = "Resettings influence distance and cutoffs"
  LOG_INFORMATION()
  call kim_model_refresh_set_influence_distance_pointer( &
    model_refresh_handle, buf%cutoff(1)+buf%cutoff(2))
  call kim_model_refresh_set_neighbor_list_pointers( &
    model_refresh_handle, 1, buf%cutoff, &
    buf%model_will_not_request_neighbors_of_noncontributing_particles)

  ierr = 0  ! everything is good
end subroutine model_refresh_func

!-------------------------------------------------------------------------------
!
! Model compute arguments create routine (REQUIRED)
!
!-------------------------------------------------------------------------------
#include "kim_model_compute_arguments_create_log_macros.fd"
subroutine model_compute_arguments_create(model_compute_handle, &
  model_compute_arguments_create_handle, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  implicit none

  !-- Transferred variables
  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_create_handle_type), intent(inout) :: &
    model_compute_arguments_create_handle
  integer(c_int), intent(out) :: ierr

  integer(c_int) :: ierr2

  ! avoid unsed dummy argument warnings
  if (model_compute_handle .eq. kim_model_compute_null_handle) continue

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
!  call kim_model_compute_arguments_create_set_argument_support_status( &
!    model_compute_arguments_create_handle, &
!    kim_compute_argument_name_partial_virial, &
!    kim_support_status_optional, ierr2)
!  ierr = ierr + ierr2

  ! register call backs
  ! NONE

  if (ierr /= 0) then
    ierr = 1
    kim_log_message = "Unable to successfully create compute_arguments object"
    LOG_ERROR()
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
  implicit none

  !-- Transferred variables
  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_destroy_handle_type), intent(inout) :: &
    model_compute_arguments_destroy_handle
  integer(c_int), intent(out) :: ierr

  integer(c_int) :: ierr2

  ierr = 0
  ierr2 = 0

  ! avoid unsed dummy argument warnings
  if (model_compute_handle .eq. kim_model_compute_null_handle) continue
  if (model_compute_arguments_destroy_handle &
    .eq. kim_model_compute_arguments_destroy_null_handle) continue

  ! nothing to do

  return
end subroutine model_compute_arguments_destroy

end module ex_model_Ar_SLJ_MultiCutoff

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
use ex_model_Ar_SLJ_MultiCutoff
use kim_model_headers_module
implicit none

!-- Transferred variables
type(kim_model_create_handle_type), intent(inout) :: model_create_handle
type(kim_length_unit_type), intent(in), value :: requested_length_unit
type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
type(kim_temperature_unit_type), intent(in), value :: requested_temperature_unit
type(kim_time_unit_type), intent(in), value :: requested_time_unit
integer(c_int), intent(out) :: ierr

!-- KIM variables
integer(c_int) :: ierr2
type(buffer_type), pointer :: buf

kim_log_file = __FILE__

ierr = 0
ierr2 = 0

! avoid unsed dummy argument warnings
if (requested_length_unit .eq. kim_length_unit_unused) continue
if (requested_energy_unit .eq. kim_energy_unit_unused) continue
if (requested_charge_unit .eq. kim_charge_unit_unused) continue
if (requested_temperature_unit .eq. kim_temperature_unit_unused) continue
if (requested_time_unit .eq. kim_time_unit_unused) continue

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
  kim_language_name_fortran, c_funloc(Compute_Energy_Forces), ierr2)
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
  kim_language_name_fortran, c_funloc(model_destroy_func), ierr2)
ierr = ierr + ierr2
call kim_model_create_set_refresh_pointer( &
  model_create_handle, kim_language_name_fortran, &
  c_funloc(model_refresh_func), ierr2)
ierr = ierr + ierr2

! allocate buffer
allocate( buf )

! store model buffer in KIM object
call kim_model_create_set_model_buffer_pointer(model_create_handle, &
  c_loc(buf))

! set buffer values
buf%influence_distance = model_cutoff1 + model_cutoff2
buf%cutoff(1) = model_cutoff1
buf%cutoff(2) = model_cutoff2
buf%model_will_not_request_neighbors_of_noncontributing_particles(1) = 0
buf%model_will_not_request_neighbors_of_noncontributing_particles(2) = 1

! register influence distance
call kim_model_create_set_influence_distance_pointer( &
  model_create_handle, buf%influence_distance)

! register cutoff
call kim_model_create_set_neighbor_list_pointers(model_create_handle, &
  2, buf%cutoff, &
  buf%model_will_not_request_neighbors_of_noncontributing_particles)

if (ierr /= 0) then
  ierr = 1
  deallocate( buf )
  kim_log_message = "Unable to successfully initialize model"
  LOG_ERROR()
endif

return

end subroutine model_create_routine
