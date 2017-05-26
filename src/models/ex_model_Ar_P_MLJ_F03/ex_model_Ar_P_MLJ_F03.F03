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
!**  Release: This file is part of the kim-api-v1.8.0 package.
!**
!****************************************************************************


#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_Ar_P_MLJ_F03

use, intrinsic :: iso_c_binding
implicit none

save
private
public Compute_Energy_Forces, &
       model_destroy, &
       model_cutoff, &
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
  real(c_double) :: cutoff
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
subroutine Compute_Energy_Forces(simulator, arguments, ierr) bind(c)
use kim_simulator_module
use kim_compute_argument_name_module
use kim_compute_simulator_compute_arguments_module
implicit none

!-- Transferred variables
type(kim_simulator_type), intent(in) :: simulator
type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
integer(c_int), intent(out) :: ierr

!-- Local variables
real(c_double) :: Rij(DIM)
real(c_double) :: r,Rsqij,phi,dphi,dEidr = 0.0_cd
integer(c_int) :: i,j,jj,numnei,comp_force,comp_enepot,comp_virial, &
  comp_energy
integer(c_int) :: ierr2

!-- KIM variables
integer(c_int), pointer :: N;                 type(c_ptr) :: pN
real(c_double), pointer :: energy;            type(c_ptr) :: penergy
real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
integer(c_int), pointer :: nei1part(:);       type(c_ptr) :: pnei1part
integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies
integer(c_int), pointer :: particleContributing(:)
type(c_ptr) :: pparticleContributing
real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial

! Check to see if we have been asked to compute the forces, energyperpart,
! energy and virial
!
ierr = 0
call kim_compute_simulator_compute_arguments_get_compute(arguments, &
  kim_compute_argument_name_energy, comp_energy, ierr2)
ierr = ierr + ierr2
call kim_compute_simulator_compute_arguments_get_compute(arguments, &
  kim_compute_argument_name_forces, comp_force, ierr2)
ierr = ierr + ierr2
call kim_compute_simulator_compute_arguments_get_compute(arguments, &
  kim_compute_argument_name_particle_energy, comp_enepot, ierr2)
ierr = ierr + ierr2
call kim_compute_simulator_compute_arguments_get_compute(arguments, &
  kim_compute_argument_name_virial, comp_virial, ierr2)
ierr = ierr + ierr2
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                               "get_compute", ierr)
   return
endif

! Unpack data from KIM object
!
ierr = 0
call kim_compute_simulator_compute_arguments_get_data(arguments, &
  kim_compute_argument_name_number_of_particles, pN, ierr2)
ierr = ierr + ierr2
call kim_compute_simulator_compute_arguments_get_data(arguments, &
  kim_compute_argument_name_particle_species, pparticleSpecies, ierr2)
ierr = ierr + ierr2
call kim_compute_simulator_compute_arguments_get_data(arguments, &
  kim_compute_argument_name_particle_species, pparticleContributing, ierr2)
ierr = ierr + ierr2
call kim_compute_simulator_compute_arguments_get_data(arguments, &
  kim_compute_argument_name_coordinates, pcoor, ierr2)
ierr = ierr + ierr2
if (comp_energy == 1) then
  call kim_compute_simulator_compute_arguments_get_data(arguments, &
    kim_compute_argument_name_energy, penergy, ierr2)
  ierr = ierr + ierr2
endif
if (comp_force == 1) then
  call kim_compute_simulator_compute_arguments_get_data(arguments, &
    kim_compute_argument_name_forces, pforce, ierr2)
  ierr = ierr + ierr2
endif
if (comp_enepot == 1) then
  call kim_compute_simulator_compute_arguments_get_data(arguments, &
    kim_compute_argument_name_particle_energy, penepot, ierr2)
  ierr = ierr + ierr2
endif
if (comp_virial == 1) then
  call kim_compute_simulator_compute_arguments_get_data(arguments, &
    kim_compute_argument_name_virial, pvirial, ierr2)
  ierr = ierr + ierr2
endif
if (ierr /= 0) then
ierr = ierr + ierr2
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                               "get_data", ierr)
   return
endif

call c_f_pointer(pN,               N)
call c_f_pointer(pparticleSpecies, particleSpecies, [N])
call c_f_pointer(pparticleContributing, particleContributing, [N])
call c_f_pointer(pcoor,            coor,            [DIM,N])
if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)
if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])
if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])
if (comp_virial.eq.1) call c_f_pointer(pvirial,         virial,         [6])

! Check to be sure that the species are correct
!
ierr = 1 ! assume an error
do i = 1,N
   if (particleSpecies(i).ne.speccode) then
      call kim_report_error(__LINE__, THIS_FILE_NAME,      &
                                  "Unexpected species detected", &
                                  ierr)
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
    call kim_compute_simulator_compute_arguments_get_neigh( &
      arguments, 1, i, numnei, pnei1part, ierr)
    if (ierr /= 0) then
      ! some sort of problem, exit
      call kim_report_error(__LINE__, THIS_FILE_NAME, &
        "kim_api_get_neigh",      &
        ierr)
      ierr = 1
      return
    endif

    call c_f_pointer(pnei1part, nei1part, [numnei])

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
subroutine model_destroy(simulator, ierr) bind(c)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module
  implicit none

  !-- Transferred variables
  type(kim_simulator_type), intent(inout) :: simulator
  integer(c_int), intent(out) :: ierr

  type(buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  call kim_simulator_get_model_buffer(simulator, pbuf)
  call c_f_pointer(pbuf, buf)
  call kim_simulator_set_model_buffer(simulator, c_null_ptr)
  deallocate(buf)
  ierr = 0  ! everything is good
end subroutine model_destroy

end module ex_model_Ar_P_MLJ_F03

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_init(simulator, ierr) bind(c)
use, intrinsic :: iso_c_binding
use ex_model_Ar_P_MLJ_F03
use kim_simulator_module
implicit none

!-- Transferred variables
type(kim_simulator_type), intent(inout) :: simulator
integer(c_int), intent(out) :: ierr

!-- KIM variables
type(buffer_type), pointer :: buf

! store pointer to compute function in KIM object
call kim_simulator_set_compute_func(simulator, kim_language_name_fortran, &
  c_funloc(Compute_Energy_Forces))
call kim_simulator_set_destroy(simulator, kim_language_name_fortran, &
  c_funloc(model_destroy))

! store model cutoff in KIM object
allocate( buf )
buf%cutoff = model_cutoff
call kim_simulator_set_model_buffer(simulator, c_loc(buf))
call kim_simulator_set_influence_distance(simulator, c_loc(buf%cutoff))
call kim_simulator_set_cutoffs(simulator, 1, c_loc(buf%cutoff))

ierr = 0
return

end subroutine model_init
