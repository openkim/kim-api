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
!**  MODULE ex_model_driver_P_LJ
!**
!**  Lennard-Jones pair potential KIM Model Driver
!**  shifted to have zero energy at the cutoff radius
!**
!**  Language: Fortran 2003
!**
!**  Release: This file is part of the kim-api-v1.8.0 package.
!**
!****************************************************************************

#define THIS_FILE_NAME __FILE__

module ex_model_driver_p_lj

use, intrinsic :: iso_c_binding
use kim_model_module
use kim_logger_module
implicit none

save
private
public BUFFER_TYPE,           &
       Compute_Energy_Forces, &
       reinit,                &
       destroy,               &
       calc_phi,              &
       calc_phi_dphi,         &
       calc_phi_dphi_d2phi,   &
       cutoff_index,          &
       epsilon_index,         &
       sigma_index

! Below are the definitions and values of all Model parameters
integer(c_int), parameter          :: cd = c_double  ! for literal constants
integer(c_int), parameter          :: DIM=3          ! dimensionality of space
integer(c_int), parameter          :: speccode = 1   ! internal species code

integer(c_int), parameter          :: cutoff_index  = 1
integer(c_int), parameter          :: epsilon_index = 2
integer(c_int), parameter          :: sigma_index   = 3

!-------------------------------------------------------------------------------
!
!  Definition of Buffer type
!
!-------------------------------------------------------------------------------
type BUFFER_TYPE
  real(c_double) :: influence_distance
  real(c_double) :: Pcutoff
  real(c_double) :: cutsq
  real(c_double) :: epsilon
  real(c_double) :: sigma
  real(c_double) :: shift
endtype BUFFER_TYPE


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
real(c_double), intent(in)  :: model_epsilon
real(c_double), intent(in)  :: model_sigma
real(c_double), intent(in)  :: model_shift
real(c_double), intent(in)  :: model_cutoff
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.0_cd
else
   phi = 4.0_cd*model_epsilon*(sor12-sor6) + model_shift
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
real(c_double), intent(in)  :: model_epsilon
real(c_double), intent(in)  :: model_sigma
real(c_double), intent(in)  :: model_shift
real(c_double), intent(in)  :: model_cutoff
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi,dphi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   dphi   = 0.0_cd
else
   phi  = 4.0_cd*model_epsilon*(sor12-sor6) + model_shift
   dphi = 24.0_cd*model_epsilon*(-2.0_cd*sor12+sor6)/r
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
real(c_double), intent(in)  :: model_epsilon
real(c_double), intent(in)  :: model_sigma
real(c_double), intent(in)  :: model_shift
real(c_double), intent(in)  :: model_cutoff
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi,dphi,d2phi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   dphi   = 0.0_cd
   d2phi  = 0.0_cd
else
   phi   = 4.0_cd*model_epsilon*(sor12-sor6) + model_shift
   dphi  = 24.0_cd*model_epsilon*(-2.0_cd*sor12+sor6)/r
   d2phi = 24.0_cd*model_epsilon*(26.0_cd*sor12-7.0_cd*sor6)/rsq
endif

end subroutine calc_phi_dphi_d2phi

!-------------------------------------------------------------------------------
!
! Compute energy and forces on particles from the positions.
!
!-------------------------------------------------------------------------------
subroutine Compute_Energy_Forces(model, ierr) bind(c)
use kim_model_module
use kim_compute_module
use kim_utility_compute_module
implicit none

!-- Transferred variables
type(kim_model_type), intent(in) :: model
integer(c_int), intent(out) :: ierr

!-- Local variables
real(c_double) :: r,Rsqij,phi,dphi,d2phi,dEidr,d2Eidr
integer(c_int) :: i,j,jj,numnei
integer(c_int) :: comp_force,comp_energy,comp_enepot,comp_process_dEdr, &
                  comp_process_d2Edr2
type(BUFFER_TYPE), pointer :: buf; type(c_ptr) :: pbuf

real(c_double), pointer :: Rij(:)
real(c_double), pointer :: Rij_pairs(:,:)
real(c_double), pointer :: r_pairs(:)
integer(c_int), pointer :: i_pairs(:), j_pairs(:)

!-- KIM variables
real(c_double) :: model_cutoff
integer(c_int), pointer :: N;                 type(c_ptr) :: pN
real(c_double), pointer :: energy;            type(c_ptr) :: penergy
real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
integer(c_int), pointer :: nei1part(:);       type(c_ptr) :: pnei1part
integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies
integer(c_int), pointer :: particleContributing(:)
type(c_ptr) :: pparticleContributing


! get model buffer from KIM object
call kim_model_get_model_buffer(model, pbuf)
call c_f_pointer(pbuf, buf)

model_cutoff = buf%influence_distance

! Check to see if we have been asked to compute the forces, energyperpart,
! energy and d1Edr
!
call kim_utility_compute_getm_compute(model, ierr, &
  kim_compute_argument_name_energy, comp_energy, 1, &
  kim_compute_argument_name_forces, comp_force, 1, &
  kim_compute_argument_name_particle_energy, comp_enepot, 1, &
  kim_compute_argument_name_process_dedr, comp_process_dedr, 1, &
  kim_compute_argument_name_process_d2edr2, comp_process_d2edr2, 1)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME,        &
                               "kim_model_getm_compute", &
                               ierr)
   return
endif

! Unpack data from KIM object
!
call kim_model_get_data(model, &
  kim_compute_argument_name_number_of_particles, pn, ierr)

call kim_utility_compute_getm_data(model, ierr, &
  kim_compute_argument_name_number_of_particles, pn, 1, &
  kim_compute_argument_name_particle_species, pparticlespecies, 1, &
  kim_compute_argument_name_particle_contributing, pparticlecontributing, 1, &
  kim_compute_argument_name_coordinates, pcoor, 1, &
  kim_compute_argument_name_energy, penergy, 1, &
  kim_compute_argument_name_forces, pforce, 1, &
  kim_compute_argument_name_particle_energy, penepot, 1)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME,     &
                               "kim_model_getm_data", &
                               ierr)
   return
endif

call c_f_pointer(pN,             N)
call c_f_pointer(pparticleSpecies, particleSpecies, [N])
call c_f_pointer(pparticleContributing, particleContributing, [N])
call c_f_pointer(pcoor,          coor,          [DIM,N])

if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)
if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])
if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])


allocate( Rij(DIM) )
if (comp_process_d2Edr2.eq.1) then
  allocate( r_pairs(2)       )
  allocate( Rij_pairs(DIM,2) )
  allocate( i_pairs(2)       )
  allocate( j_pairs(2)       )
endif

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

! Initialize potential energies, forces
!
if (comp_enepot.eq.1) enepot = 0.0_cd
if (comp_energy.eq.1) energy = 0.0_cd
if (comp_force.eq.1)  force  = 0.0_cd

!
!  Compute energy and forces
!

!  Loop over particles and compute energy and forces
!
do i = 1, N

  if (particleContributing(i) == 1) then
    ! Set up neighbor list for next particle
    !
    call kim_model_get_neigh(model, 1, i, numnei, pnei1part, ierr)
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

      j = nei1part(jj)                        ! get neighbor ID

      ! compute relative position vector
      !
      Rij(:) = coor(:,j) - coor(:,i)       ! distance vector between i j

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. buf%cutsq ) then            ! particles are interacting?

        r = sqrt(Rsqij)                          ! compute distance
        if (comp_process_d2Edr2.eq.1) then
          call calc_phi_dphi_d2phi(buf%epsilon, &
            buf%sigma,   &
            buf%shift,   &
            buf%Pcutoff,  &
            r,phi,dphi,d2phi) ! compute pair potential
          !   and it derivatives
          dEidr  = 0.5_cd*dphi               !      regular contribution
          d2Eidr = 0.5_cd*d2phi
        elseif (comp_force.eq.1.or.comp_process_dEdr.eq.1) then
          call calc_phi_dphi(buf%epsilon, &
            buf%sigma,   &
            buf%shift,   &
            buf%Pcutoff,  &
            r,phi,dphi)        ! compute pair potential
          !   and it derivative

          dEidr = 0.5_cd*dphi                !      regular contribution
        else
          call calc_phi(buf%epsilon, &
            buf%sigma,   &
            buf%shift,   &
            buf%Pcutoff,  &
            r,phi)                  ! compute just pair potential
        endif

        ! contribution to energy
        !
        if (comp_enepot.eq.1) then
          enepot(i) = enepot(i) + 0.5_cd*phi    ! accumulate energy
        endif
        if (comp_energy.eq.1) then
          energy = energy + 0.5_cd*phi       !      add half v to total energy
        endif

        ! contribution to process_dEdr
        !
        if (comp_process_dEdr.eq.1) then
          call kim_model_process_dedr(model, deidr, r, c_loc(rij(1)), i, j, &
            ierr)
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

          call kim_model_process_d2edr2(model, d2eidr, &
            c_loc(r_pairs(1)),     &
            c_loc(Rij_pairs(1,1)), &
            c_loc(i_pairs(1)),     &
            c_loc(j_pairs(1)), ierr)
        endif

        ! contribution to forces
        !
        if (comp_force.eq.1) then
          force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on particle i
          force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on particle j
        endif

      endif

    enddo  ! loop on jj

  endif  ! if particleContributing

enddo  ! do i

! Free temporary storage
!
if (comp_process_d2Edr2.eq.1) then
  deallocate( r_pairs   )
  deallocate( Rij_pairs )
  deallocate( i_pairs   )
  deallocate( j_pairs   )
endif

! Everything is great
!
ierr = 0
return

end subroutine Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model driver reinitialization routine
!
!-------------------------------------------------------------------------------
subroutine reinit(model, ierr) bind(c)
use kim_model_module
use kim_compute_module
use kim_logger_module
implicit none

!-- Transferred variables
type(kim_model_type), intent(inout) :: model
integer(c_int), intent(out) :: ierr

!-- Local variables
real(c_double) energy_at_cutoff
type(BUFFER_TYPE), pointer :: buf; type(c_ptr) :: pbuf

!-- KIM variables
real(c_double) :: cutoffs(1)

! get model buffer from KIM object
call kim_model_get_model_buffer(model, pbuf)
call c_f_pointer(pbuf, buf)

! Set new values in KIM object and buffer
!
buf%influence_distance = buf%Pcutoff
buf%cutsq = (buf%Pcutoff)**2
! calculate pair potential at r=cutoff with shift=0.0
call calc_phi(buf%epsilon, &
              buf%sigma,   &
              0.0_cd,      &
              buf%Pcutoff, &
              buf%Pcutoff,energy_at_cutoff)
buf%shift = -energy_at_cutoff

ierr = 0
return

end subroutine reinit

!-------------------------------------------------------------------------------
!
! Model driver destroy routine
!
!-------------------------------------------------------------------------------
subroutine destroy(model, ierr) bind(c)
use kim_model_module
implicit none

!-- Transferred variables
type(kim_model_type), intent(inout) :: model
integer(c_int), intent(out) :: ierr

!-- Local variables
type(BUFFER_TYPE), pointer :: buf; type(c_ptr) :: pbuf

! get model buffer from KIM object
call kim_model_get_model_buffer(model, pbuf)
call c_f_pointer(pbuf, buf)

deallocate( buf )

ierr = 0
return

end subroutine destroy

end module ex_model_driver_p_lj

!-------------------------------------------------------------------------------
!
! Model driver initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_driver_init(model, pparamfile, nmstrlen, numparamfiles, ierr) &
  bind(c)

use, intrinsic :: iso_c_binding
use ex_model_driver_p_lj
use kim_model_module
use kim_compute_module
use kim_unit_system_module
use kim_utility_compute_module
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!-- Transferred variables
type(kim_model_type), intent(inout) :: model
type(c_ptr), value, intent(in) :: pparamfile
integer(c_int), value,    intent(in) :: nmstrlen
integer(c_int), value,    intent(in) :: numparamfiles
character(len=nmstrlen), pointer :: paramfile(:)
integer(c_int), intent(out) :: ierr

!-- Local variables
integer(c_int), parameter :: one=1
integer(c_int) i,j
type(BUFFER_TYPE), pointer :: buf;
! define variables for all model parameters to be read in
real(c_double) factor
real(c_double) in_cutoff
real(c_double) in_epsilon
real(c_double) in_sigma
real(c_double) energy_at_cutoff

call c_f_pointer(pparamfile, paramfile, [numparamfiles])

! find first null character and write spaces everywhere afterward
do i = 1, numparamfiles
  j = index(paramfile(1),char(0))
  paramfile(i)(j:)=" "
end do

! store function pointers in KIM object
call kim_model_set_method(model, &
  kim_compute_argument_name_compute, 1, kim_compute_language_name_fortran, c_funloc(Compute_Energy_Forces), ierr)
call kim_utility_compute_setm_method(model, ierr, &
  kim_compute_argument_name_compute, 1, kim_compute_language_name_fortran, c_funloc(Compute_Energy_Forces), 1, &
  kim_compute_argument_name_reinit,  1, kim_compute_language_name_fortran, c_funloc(reinit), 1, &
  kim_compute_argument_name_destroy, 1, kim_compute_language_name_fortran, c_funloc(destroy), 1)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_setm_method", ierr)
   goto 42
endif

! Read in model parameters from parameter file
!
open(10,file=paramfile(1),status="old")
read(10,*,iostat=ierr,err=100) in_cutoff
read(10,*,iostat=ierr,err=100) in_epsilon
read(10,*,iostat=ierr,err=100) in_sigma
close(10)

goto 200
100 continue
! reading parameters failed
ierr = 1
call kim_report_error(__LINE__, THIS_FILE_NAME, &
                            "Unable to read LJ parameters, kimerror = ",ierr)
goto 42

200 continue

! convert to appropriate units
call kim_model_convert_to_act_unit(model, &
  kim_units_a, &
  kim_units_ev, &
  kim_units_e, &
  kim_units_k, &
  kim_units_ps, &
  1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, factor, ierr)
if (ierr /= 0) then
  call kim_report_error(__LINE__, THIS_FILE_NAME, &
    "kim_api_convert_to_act_unit", ierr)
  goto 42
endif
in_cutoff = in_cutoff * factor

call kim_model_convert_to_act_unit(model, &
  kim_units_a, &
  kim_units_ev, &
  kim_units_e, &
  kim_units_k, &
  kim_units_ps, &
  0.0_cd, 1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, factor, ierr)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                             "kim_api_convert_to_act_unit", ierr)
   goto 42
endif
in_epsilon = in_epsilon * factor

call kim_model_convert_to_act_unit(model, &
  kim_units_a, &
  kim_units_ev, &
  kim_units_e, &
  kim_units_k, &
  kim_units_ps, &
  1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, factor, ierr)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                             "kim_api_convert_to_act_unit", ierr)
   goto 42
endif
in_sigma = in_sigma * factor

allocate( buf )

! setup buffer
! set value of parameters
buf%influence_distance = in_cutoff
buf%Pcutoff = in_cutoff
buf%cutsq   = in_cutoff**2
buf%epsilon = in_epsilon
buf%sigma   = in_sigma
call calc_phi(in_epsilon, &
              in_sigma,   &
              0.0_cd,     &
              in_cutoff,  &
              in_cutoff, energy_at_cutoff)
buf%shift   = -energy_at_cutoff

! store model cutoff in KIM object
call kim_model_set_influence_distance(model, c_loc(buf%influence_distance))
call kim_model_set_cutoffs(model, 1, c_loc(buf%influence_distance))

! end setup buffer

! store in model buffer
call kim_model_set_model_buffer(model, c_loc(buf))

! set pointers to parameters in KIM object
call kim_model_set_parameter(model, cutoff_index, 1, c_loc(buf%pcutoff), ierr)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                               "set_parameter", ierr);
   goto 42
endif

call kim_model_set_parameter(model, epsilon_index, 1, c_loc(buf%epsilon), ierr)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                               "set_parameter", ierr);
   goto 42
endif

call kim_model_set_parameter(model, sigma_index, 1, c_loc(buf%sigma), ierr)
if (ierr /= 0) then
   call kim_report_error(__LINE__, THIS_FILE_NAME, &
                               "set_parameter", ierr);
   goto 42
endif

ierr = 0
42 continue
return

end subroutine model_driver_init
