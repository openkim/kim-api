!****************************************************************************
!**
!**  MODULE model_driver_P_LJ
!**
!**  Lennard-Jones pair potential KIM Model Driver
!**  shifted to have zero energy at the cutoff radius
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Language: Fortran 90
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!****************************************************************************

#include "KIMstatus.h"

module model_driver_P_LJ

use KIMservice
implicit none

save
private
public Compute_Energy_Forces, &
       reinit,                &
       destroy,               &
       calc_phi

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code

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
double precision, intent(in)  :: model_epsilon
double precision, intent(in)  :: model_sigma
double precision, intent(in)  :: model_shift
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi

!-- Local variables
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
else 
   phi = 4.d0*model_epsilon*(sor12-sor6) + model_shift
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
double precision, intent(in)  :: model_epsilon
double precision, intent(in)  :: model_sigma
double precision, intent(in)  :: model_shift
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi

!-- Local variables
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
else 
   phi  = 4.d0*model_epsilon*(sor12-sor6) + model_shift
   dphi = 24.d0*model_epsilon*(-2.d0*sor12+sor6)/r
endif

end subroutine calc_phi_dphi

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
subroutine Compute_Energy_Forces(pkim,ier)
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in)  :: pkim
integer,                  intent(out) :: ier

!-- Local variables
double precision :: Rij(DIM)
double precision :: r,Rsqij,phi,dphi,dEidr
integer :: i,j,jj,numnei,atom_ret
integer :: comp_force,comp_energy,comp_enepot,comp_process_d1Edr
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message
integer :: idum

!-- KIM variables
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_epsilon;   pointer(pmodel_epsilon,model_epsilon)
real*8  model_sigma;     pointer(pmodel_sigma,  model_sigma)
real*8  model_shift;     pointer(pmodel_shift,  model_shift)
integer N;               pointer(pN,N)
real*8  energy;          pointer(penergy,energy)
real*8  coordum(DIM,1);  pointer(pcoor,coordum)
real*8  forcedum(DIM,1); pointer(pforce,forcedum)
real*8  enepotdum(1);    pointer(penepot,enepotdum)
real*8  boxlength(DIM);  pointer(pboxlength,boxlength)
real*8  Rij_list(DIM,1); pointer(pRij_list,Rij_list)
integer numContrib;      pointer(pnumContrib,numContrib)
integer nei1atom(1);     pointer(pnei1atom,nei1atom)
integer atomTypes(1);    pointer(patomTypes,atomTypes)
character*64 NBC_Method; pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib

! Unpack the Model's parameters stored in the KIM API object
!
call kim_api_get_data_multiple_f(pkim, ier, &
     "cutoff",             pmodel_cutoff,  1, &
     "PARAM_FIXED_cutsq",  pmodel_cutsq,   1, &
     "PARAM_FREE_epsilon", pmodel_epsilon, 1, &
     "PARAM_FREE_sigma",   pmodel_sigma,   1, &
     "PARAM_FIXED_shift",  pmodel_shift,   1  &
     )
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
   return
endif

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method_f(pkim, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_nbc_method_f", ier)
   return
endif
if (index(NBC_Method,"CLUSTER").eq.1) then
   NBC = 0
   HalfOrFull = 1
elseif (index(NBC_Method,"MI-OPBC-H").eq.1) then
   NBC = 1
   HalfOrFull = 1
elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
   NBC = 1
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
   NBC = 2
   HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
   NBC = 2
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH-RVEC-F").eq.1) then
   NBC = 3
   HalfOrFull = 2
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, __FILE__, "Unknown NBC method", ier)
   return
endif
call free(pNBC_Method) ! don't forget to release the memory...

! Determine neighbor list handling mode
!
if (NBC.ne.0) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   IterOrLoca = kim_api_get_neigh_mode_f(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_mode_f", ier)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      ier = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      idum = kim_api_report_error_f(__LINE__, __FILE__, error_message, ier)
      stop
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and d1Edr
!
call kim_api_get_compute_multiple_f(pkim, ier, &
     "energy",        comp_energy,        1, &
     "forces",        comp_force,         1, &
     "energyPerAtom", comp_enepot,        1, &
     "process_d1Edr", comp_process_d1Edr, 1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_compute_multiple_f", ier)
   return
endif

! Unpack data from KIM object
!
call kim_api_get_data_multiple_f(pkim, ier, &
     "numberOfAtoms",           pN,          1,                             &
     "atomTypes",               patomTypes,  1,                             &
     "coordinates",             pcoor,       1,                             &
     "numberContributingAtoms", pnumContrib, merge(1,0,(HalfOrFull.eq.1)),  &
     "boxlength",               pboxlength,  merge(1,0,(NBC.eq.1)),         &
     "energy",                  penergy,     merge(1,0,(comp_energy.eq.1)), &
     "forces",                  pforce,      merge(1,0,(comp_force.eq.1)),  &
     "energyPerAtom",           penepot,     merge(1,0,(comp_enepot.eq.1)))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
   return
endif

call toRealArrayWithDescriptor2d(coordum,coor,DIM,N)
if (comp_force.eq.1)  call toRealArrayWithDescriptor2d(forcedum,force,DIM,N)
if (comp_enepot.eq.1) call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N)
if (HalfOrFull.eq.1) then
   if (NBC.ne.0) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER case
      numberContrib = N
   endif
endif

! Check to be sure that the atom types are correct
!
ier = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (atomTypes(i).ne.speccode) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "Unexpected species type detected", ier)
      return
   endif
enddo
ier = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces
!
if (comp_enepot.eq.1) ene_pot(1:N) = 0.d0
if (comp_energy.eq.1) energy = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.0) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

! Initialize neighbor handling for Iterator mode
!
if (IterOrLoca.eq.1) then
   if (HalfOrFull.eq.1) then  ! HALF list
      ier = kim_api_get_half_neigh_f(pkim,0,0,atom_ret,numnei, &
                                     pnei1atom,pRij_list)
   else                       ! FULL list
      ier = kim_api_get_full_neigh_f(pkim,0,0,atom_ret,numnei, &
                                     pnei1atom,pRij_list)
   endif
   ! check for successful initialization
   if (ier.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then
      if (HalfOrFull.eq.1) then
         idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
      else
         idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
      endif
      ier = KIM_STATUS_FAIL
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
      if (HalfOrFull.eq.1) then ! HALF list
         ier = kim_api_get_half_neigh_f(pkim,0,1,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      else                      ! FULL list
         ier = kim_api_get_full_neigh_f(pkim,0,1,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      endif
      if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
                                ! incremented past the end of the list,
                                ! terminate loop
      if (ier.lt.KIM_STATUS_OK) then ! some sort of problem, exit
         if (HalfOrFull.eq.1) then
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
         else
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
         endif
         return
      endif

      i = atom_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (HalfOrFull.eq.1) then ! HALF list
         if (NBC.eq.0) then     ! CLUSTER NBC method
            numnei = N - i      ! number of neighbors in list i+1, ..., N
            nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
            ier = KIM_STATUS_OK
         else
            ier = kim_api_get_half_neigh_f(pkim,1,i,atom_ret,numnei, &
                                           pnei1atom,pRij_list)
         endif
      else                      ! FULL list
         ier = kim_api_get_full_neigh_f(pkim,1,i,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      endif
      if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
         if (HalfOrFull.eq.1) then
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
         else
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
         endif
         ier = KIM_STATUS_FAIL
         return
      endif

   endif
      
   ! Loop over the neighbors of atom i
   !
   do jj = 1, numnei

      j = nei1atom(jj)                            ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.3) then                          ! all methods except NEIGH-RVEC-F
         Rij(:) = coor(:,j) - coor(:,i)           ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.1) then
         where ( abs(Rij) .gt. 0.5d0*boxlength )  ! periodic boundary conditions
            Rij = Rij - sign(boxlength,Rij)       ! applied where needed.
         end where                                ! 
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?

         r = sqrt(Rsqij)                          ! compute distance
         if (comp_force.eq.1.or.comp_process_d1Edr.eq.1) then
            call calc_phi_dphi(model_epsilon, &
                               model_sigma,   &
                               model_shift,   &
                               model_cutoff,  &
                               r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative

            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr = dphi                       !      double contribution
            else                                  ! FULL mode
               dEidr = 0.5d0*dphi                 !      regular contribution
            endif
         else
            call calc_phi(model_epsilon, &
                          model_sigma,   &
                          model_shift,   &
                          model_cutoff,  &
                          r,phi)                  ! compute just pair potential
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + 0.5d0*phi   ! accumulate energy
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) &         ! HALF mode
               ene_pot(j) = ene_pot(j) + 0.5d0*phi! (i and j share it)
         elseif (comp_energy.eq.1) then
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               energy = energy + phi              !      add v to total energy
            else                                  ! FULL mode
               energy = energy + 0.5d0*phi        !      add half v to total energy
            endif
         endif

         ! contribution to process_d1Edr
         !
         if (comp_process_d1Edr.eq.1) then
            call kim_api_process_d1Edr_f(pkim, dEidr, r, loc(Rij), i, j, ier)
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

! Perform final tasks
!
if (comp_enepot.eq.1 .and. comp_energy.eq.1) &
   energy = sum(ene_pot(1:N))                       ! compute total energy

! Free temporary storage
!
if (NBC.eq.0) deallocate( nei1atom_substitute )

! Everything is great
!
ier = KIM_STATUS_OK
return

end subroutine Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model driver reinitialization routine
!
!-------------------------------------------------------------------------------
subroutine reinit(pkim)
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
double precision energy_at_cutoff
integer ier, idum

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_epsilon;   pointer(pmodel_epsilon,model_epsilon)
real*8  model_sigma;     pointer(pmodel_sigma,  model_sigma)
real*8  model_shift;     pointer(pmodel_shift,  model_shift)

! Get FREE parameters from KIM object
!
call kim_api_get_data_multiple_f(pkim, ier, &
     "cutoff",             pcutoff,        1, &
     "PARAM_FREE_cutoff",  pmodel_cutoff,  1, &
     "PARAM_FIXED_cutsq",  pmodel_cutsq,   1, &
     "PARAM_FREE_epsilon", pmodel_epsilon, 1, &
     "PARAM_FREE_sigma",   pmodel_sigma,   1, &
     "PARAM_FIXED_shift",  pmodel_shift,   1  &
     )
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
   return
endif

!
! Set new values in KIM object
!
cutoff      = model_cutoff
model_cutsq = model_cutoff**2
! calculate pair potential at r=cutoff with shift=0.0
model_shift = 0.d0
call calc_phi(model_epsilon, &
              model_sigma,   &
              model_shift,   &
              model_cutoff,  &
              model_cutoff,energy_at_cutoff)  
model_shift = -energy_at_cutoff

end subroutine reinit

!-------------------------------------------------------------------------------
!
! Model driver destroy routine
!
!-------------------------------------------------------------------------------
subroutine destroy(pkim)
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer ier, idum

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_epsilon;   pointer(pmodel_epsilon,model_epsilon)
real*8  model_sigma;     pointer(pmodel_sigma,  model_sigma)
real*8  model_shift;     pointer(pmodel_shift,  model_shift)

! Get all parameters added to KIM object and free memory 
!
call kim_api_get_data_multiple_f(pkim, ier, &
     "PARAM_FREE_cutoff",   pmodel_cutoff,  1, &
     "PARAM_FIXED_cutsq",   pmodel_cutsq,   1, &
     "PARAM_FREE_epsilon",  pmodel_epsilon, 1, &
     "PARAM_FREE_sigma",    pmodel_sigma,   1, &
     "PARAM_FIXED_shift",   pmodel_shift,   1  &
     )
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
   return
endif

call free(pmodel_cutoff)
call free(pmodel_cutsq)
call free(pmodel_epsilon)
call free(pmodel_sigma)
call free(pmodel_shift)

end subroutine destroy

end module model_driver_P_LJ

!-------------------------------------------------------------------------------
!
! Model driver initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_driver_P_LJ_init(pkim, byte_paramfile, len_paramfile)
use model_driver_P_LJ
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim
byte,                     intent(in) :: byte_paramfile(len_paramfile+1)
integer,                  intent(in) :: len_paramfile

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
character(len=len_paramfile) paramfile
integer i,ier, idum
! define variables for all model parameters to be read in
double precision in_cutoff
double precision in_epsilon
double precision in_sigma
double precision energy_at_cutoff

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_epsilon;   pointer(pmodel_epsilon,model_epsilon)
real*8  model_sigma;     pointer(pmodel_sigma,  model_sigma)
real*8  model_shift;     pointer(pmodel_shift,  model_shift)

! store function pointers in KIM object
call kim_api_set_data_multiple_f(pkim, ier, &
     "compute", one, loc(Compute_Energy_Forces), 1, &
     "reinit",  one, loc(reinit),                1, &
     "destroy", one, loc(destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_multiple_f", ier)
   stop
endif

! Read in model parameters from parameter file
!
do i=1,len_paramfile
   paramfile(i:i) = char(byte_paramfile(i))
enddo
read(paramfile,*,iostat=ier,err=100) in_cutoff,   &
                                     in_epsilon,  &
                                     in_sigma
goto 200
100 continue
! reading parameters failed
idum = kim_api_report_error_f(__LINE__, __FILE__, "Unable to read LJ parameters, kimerror = ",KIM_STATUS_FAIL)
stop

200 continue

! convert to appropriate units
in_cutoff = in_cutoff * kim_api_convert_unit_from_f(pkim, "A", "eV", "e", "K", "fs", &
                                                    1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, __FILE__, "kim_api_convert_unit_from_f", ier)
   stop
endif
in_epsilon = in_epsilon * kim_api_convert_unit_from_f(pkim, "A", "eV", "e", "K", "fs", &
                                                      0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, __FILE__, "kim_api_convert_unit_from_f", ier)
   stop
endif
in_sigma = in_sigma * kim_api_convert_unit_from_f(pkim, "A", "eV", "e", "K", "fs", &
                                                  1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, __FILE__, "kim_api_convert_unit_from_f", ier)
   stop
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
   stop
endif
cutoff = in_cutoff

! allocate memory for parameters
pmodel_cutoff = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_cutoff.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_cutsq = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_cutsq.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_epsilon = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_epsilon.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_sigma = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_sigma.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_shift = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_shift.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif

! store values in KIM object
call kim_api_set_data_multiple_f(pkim, ier, &
     "PARAM_FREE_cutoff",  one, pmodel_cutoff,  1, &
     "PARAM_FIXED_cutsq",  one, pmodel_cutsq,   1, &
     "PARAM_FREE_epsilon", one, pmodel_epsilon, 1, &
     "PARAM_FREE_sigma",   one, pmodel_sigma,   1, &
     "PARAM_FIXED_shift",  one, pmodel_shift,   1  &
     )
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_multiple_f", ier);
   stop
endif

! set value of parameters
model_cutoff  = in_cutoff
model_cutsq   = in_cutoff**2
model_epsilon = in_epsilon
model_sigma   = in_sigma
model_shift   = 0.d0
call calc_phi(model_epsilon, &
              model_sigma,   &
              model_shift,   &
              model_cutoff,  &
              model_cutoff,energy_at_cutoff)  
model_shift   = -energy_at_cutoff

end subroutine model_driver_P_LJ_init
