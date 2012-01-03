!****************************************************************************
!**
!**  MODULE model_Ar_P_MLJ_F90
!**
!**  Lennard-Jones pair potential model for Ar
!**
!**  Reference: Ashcroft and Mermin
!**
!**  Author: Ellad B. Tadmor
!**  Date  : August 3, 2011
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

module model_Ar_P_MLJ_F90

use KIMservice
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code
double precision, parameter :: model_cutoff  = 8.15d0 ! cutoff radius
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
double precision, parameter :: lj_epsilon = 0.0104d0
double precision, parameter :: lj_sigma   = 3.40d0
double precision, parameter :: lj_cutnorm = model_cutoff/lj_sigma
double precision, parameter :: lj_A = 12.d0*lj_epsilon*(-26.d0 + 7.d0*lj_cutnorm**6)/(lj_cutnorm**14*lj_sigma**2)
double precision, parameter :: lj_B = 96.d0*lj_epsilon*(7.d0-2.d0*lj_cutnorm**6)/(lj_cutnorm**13*lj_sigma)
double precision, parameter :: lj_C = 28.d0*lj_epsilon*(-13.d0+4.d0*lj_cutnorm**6)/(lj_cutnorm**12)

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
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = lj_sigma/r      !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
else 
   phi = 4.d0*lj_epsilon*(sor12-sor6) + lj_A*rsq + lj_B*r + lj_C
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
double precision rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = lj_sigma/r      !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
else 
   phi  = 4.d0*lj_epsilon*(sor12-sor6) + lj_A*rsq + lj_B*r + lj_C
   dphi = 24.d0*lj_epsilon*(-2.d0*sor12+sor6)/r  + 2.d0*lj_A*r + lj_B
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
integer :: i,j,jj,numnei,atom_ret,comp_force,comp_enepot,comp_virial,comp_energy
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message

!-- KIM variables
integer N;                  pointer(pN,N)
real*8  energy;             pointer(penergy,energy)
real*8  coordum(DIM,1);     pointer(pcoor,coordum)
real*8  forcedum(DIM,1);    pointer(pforce,forcedum)
real*8  enepotdum(1);       pointer(penepot,enepotdum)
real*8  boxlength(DIM);     pointer(pboxlength,boxlength)
real*8  Rij_list(DIM,1);    pointer(pRij_list,Rij_list)
integer numContrib;         pointer(pnumContrib,numContrib)
integer nei1atom(1);        pointer(pnei1atom,nei1atom)
integer atomTypes(1);       pointer(patomTypes,atomTypes)
real*8  virialGlobaldum(1); pointer(pvirialGlobal,virialGlobaldum)
character*64 NBC_Method;    pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:),virial_global(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib
integer idum

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
! energy and virial
!
call kim_api_get_compute_multiple_f(pkim, ier, &
     "energy",         comp_energy, 1, &
     "forces",         comp_force,  1, &
     "energyPerAtom",  comp_enepot, 1, &
     "virialGlobal",   comp_virial, 1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_compute_multiple_f", ier)
   return
endif

! Unpack data from KIM object
!
call kim_api_get_data_multiple_f(pkim, ier, &
     "numberOfAtoms",           pN,            1,                             &
     "atomTypes",               patomTypes,    1,                             &
     "coordinates",             pcoor,         1,                             &
     "numberContributingAtoms", pnumContrib,   merge(1,0,(HalfOrFull.eq.1)),  &
     "boxlength",               pboxlength,    merge(1,0,(NBC.eq.1)),         &
     "energy",                  penergy,       merge(1,0,(comp_energy.eq.1)), &
     "forces",                  pforce,        merge(1,0,(comp_force.eq.1)),  &
     "energyPerAtom",           penepot,       merge(1,0,(comp_enepot.eq.1)), &
     "virialGlobal",            pvirialGlobal, merge(1,0,(comp_virial.eq.1)))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
   return
endif

call toRealArrayWithDescriptor2d(coordum,coor,DIM,N)
if (comp_force.eq.1)  call toRealArrayWithDescriptor2d(forcedum,force,DIM,N)
if (comp_enepot.eq.1) call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N)
if (comp_virial.eq.1) call toRealArrayWithDescriptor1d(virialGlobaldum,virial_global,6)

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

! Initialize potential energies, forces, virial term
!
if (comp_enepot.eq.1) ene_pot(1:N)   = 0.d0
if (comp_energy.eq.1) energy         = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0
if (comp_virial.eq.1) virial_global  = 0.d0

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
      if (ier.lt.KIM_STATUS_OK) then     ! some sort of problem, exit
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
         if (comp_force.eq.1.or.comp_virial.eq.1) then
            call calc_phi_dphi(r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr = dphi                       !      double contribution
            else                                  ! FULL mode
               dEidr = 0.5d0*dphi                 !      regular contribution
            endif
         else
            call calc_phi(r,phi)                  ! compute just pair potential
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

         ! contribution to virial tensor, virial(i,j)=r(i)*r(j)*(dV/dr)/r
         !
         if (comp_virial.eq.1) then
            virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dEidr/r
            virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dEidr/r
            virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dEidr/r
            virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dEidr/r
            virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dEidr/r
            virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dEidr/r
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

end module model_Ar_P_MLJ_F90

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_Ar_P_MLJ_F90_init(pkim)
use model_Ar_P_MLJ_F90
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
integer ier, idum

!-- KIM variables
real*8 cutoff; pointer(pcutoff,cutoff)

! store pointer to compute function in KIM object
ier = kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
   stop
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
   stop
endif
cutoff = model_cutoff

end subroutine model_Ar_P_MLJ_F90_init

