!****************************************************************************
!**
!**  MODULE model_<FILL element name>_PF_<FILL model name>
!**
!**  <FILL model name> pair functional model for <FILL element name> 
!**
!**  Reference: <FILL>
!**
!**  Author: <FILL>
!**  Date  : <FILL>
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

module model_<FILL element name>_PF_<FILL model name>

use KIMservice
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code
double precision, parameter :: model_cutoff  = <FILL cutoff radius> ! cutoff radius
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
double precision, parameter :: <FILL paramter name> = <FILL parameter value>

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
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
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
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
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
double precision, intent(in)  :: r
double precision, intent(out) :: g

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   g = 0.d0
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
double precision, intent(in)  :: r
double precision, intent(out) :: dg

!-- Local variables
! <FILL place any local variable definitions here>

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   dg = 0.d0
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
double precision, intent(in)  :: rho
double precision, intent(out) :: U

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
double precision, intent(in)  :: rho
double precision, intent(out) :: U,dU

!-- Local variables
! <FILL place any local variable definitions here>

U  = !<FILL functional form of U(rho)>
dU = !<FILL functional form of dU(rho)>

end subroutine calc_U_dU


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
double precision :: r,Rsqij,phi,dphi,g,dg,dU,dphieff
double precision :: dphii,dUi,Ei,dphij,dUj,Ej
integer :: i,j,jj,numnei,comp_force,comp_enepot,comp_virial,comp_energy
double precision, allocatable :: rho(:),U(:),derU(:)
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message

!-- KIM variables
integer N;                 pointer(pN,N)
real*8 energy;             pointer(penergy,energy)
real*8 coordum(DIM,1);     pointer(pcoor,coordum)
real*8 forcedum(DIM,1);    pointer(pforce,forcedum)
real*8 enepotdum(1);       pointer(penepot,enepotdum)
real*8 boxlength(DIM);     pointer(pboxlength,boxlength)
real*8 Rij_list(DIM,1);    pointer(pRij_list,Rij_list)
integer numContrib;        pointer(pnumContrib,numContrib)
integer nei1atom(1);       pointer(pnei1atom,nei1atom)
integer atomTypes(1);      pointer(patomTypes,atomTypes)
real*8 virialGlobaldum(1); pointer(pvirialGlobal,virialGlobaldum)
character*64 NBC_Method;   pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:),virial_global(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib

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
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_nbc_method_f", ier)
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
   call kim_api_report_error_f(__LINE__, __FILE__, "Unknown NBC method", ier)
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
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_mode_f", ier)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      ier = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      call kim_api_report_error_f(__LINE__, __FILE__, error_message, ier)
      stop
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and virial
!
comp_energy = kim_api_isit_compute_f(pkim,"energy",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_force = kim_api_isit_compute_f(pkim,"forces",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_enepot = kim_api_isit_compute_f(pkim,"energyPerAtom",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_virial = kim_api_isit_compute_f(pkim,"virialGlobal",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

! Unpack data from KIM object
!
pN = kim_api_get_data_f(pkim,"numberOfAtoms",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

patomTypes = kim_api_get_data_f(pkim,"atomTypes",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

pcoor = kim_api_get_data_f(pkim,"coordinates",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

if (HalfOrFull.eq.1) then
   pnumContrib = kim_api_get_data_f(pkim,"numberContributingAtoms",ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   if (NBC.ne.0) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER cases
      numberContrib = N
   endif
endif

if (NBC.eq.1) then
   pboxlength = kim_api_get_data_f(pkim,"boxlength",ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
endif

if (comp_energy.eq.1) then
   penergy = kim_api_get_data_f(pkim,"energy",ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
endif

if (comp_force.eq.1) then
   pforce  = kim_api_get_data_f(pkim,"forces",ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor2d(forcedum,force,DIM,N)
endif

if (comp_enepot.eq.1) then
   penepot = kim_api_get_data_f(pkim,"energyPerAtom",ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N)
endif

if (comp_virial.eq.1) then
   pvirialGlobal = kim_api_get_data_f(pkim,"virialGlobal",ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor1d(virialGlobaldum,virial_global,6)
endif

call toRealArrayWithDescriptor2d(coordum,coor,DIM,N)

! Check to be sure that the atom types are correct
!
ier = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (atomTypes(i).ne.speccode) then
      call kim_api_report_error_f(__LINE__, __FILE__, "Unexpected species type detected", ier)
      return
   endif
enddo
ier = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces, virial term, electron density
!
if (comp_enepot.eq.1) ene_pot(1:N) = 0.d0
if (comp_energy.eq.1) energy = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0
if (comp_virial.eq.1) virial_global = 0.d0
allocate( rho(N) )  ! pair functional electron density
rho(1:N) = 0.d0
allocate( U(N) )    ! embedding energy
if (comp_force.eq.1.or.comp_virial.eq.1) allocate( derU(N) )  ! EAM embedded energy deriv

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.0) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

!
!  Compute energy and forces
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) &
   call reset_iterator(HalfOrFull,pkim,numnei,pnei1atom,pRij_list,ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "reset_iterator", ier)
   return
endif

!  Loop over particles in the neighbor list a first time,
!  to compute electron density (=coordination)
!
i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   call get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                   i,numnei,pnei1atom,pRij_list,ier)
   if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit  ! atom counter incremented past end of list
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "get_current_atom_neighbors", ier)
      return
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

      ! compute contribution to electron density
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?
         r = sqrt(Rsqij)                          ! compute distance
         call calc_g(r,g)                         ! compute electron density
         rho(i) = rho(i) + g                      ! accumulate electron density
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) &            ! HALF mode
            rho(j) = rho(j) + g                   !      (add contrib to j)
      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

!  Now that we know the electron densities, calculate embedding part of energy
!  U and its derivative U' (derU)
!
do i = 1,N
   if (comp_force.eq.1.or.comp_virial.eq.1) then
      call calc_U_dU(rho(i),U(i),dU)              ! compute embedding energy
                                                  !   and its derivative
      derU(i) = dU                                ! store dU for later use
   else
      call calc_U(rho(i),U(i) )                   ! compute just embedding energy
   endif

   ! accumulate the embedding energy contribution
   !
   ! Assuming U(rho=0) = 0.0d0
   !
   if (comp_enepot.eq.1) then                     ! accumulate embedding energy contribution
      ene_pot(i) = ene_pot(i) + U(i)
   elseif (comp_energy.eq.1) then
      energy = energy + U(i)
   endif

   if ((HalfOrFull.eq.1) .and. (i .gt. numberContrib)) exit
enddo

!  Loop over particles in the neighbor list a second time, to compute
!  the forces and complete energy calculation
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) &
   call reset_iterator(HalfOrFull,pkim,numnei,pnei1atom,pRij_list,ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "reset_iterator", ier)
   return
endif

i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   call get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                   i,numnei,pnei1atom,pRij_list,ier)

   if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit  ! atom counter incremented past end of list
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "get_current_atom_neighbors", ier)
      return
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
            call calc_dg(r,dg)                    ! compute elect dens first deriv
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dphii  = 0.5d0*dphi
               dphij  = 0.5d0*dphi
               dUi    = derU(i)*dg
               dUj    = derU(j)*dg
            else                                  ! FULL mode
               dphii  = 0.5d0*dphi
               dphij  = 0.0d0
               dUi    = derU(i)*dg
               dUj    = 0.0d0
            endif
            dphieff = dphii + dphij + dUi + dUj
         else
            call calc_phi(r,phi,irlast)           ! compute just pair potential
         endif
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) then         ! HALF mode
            Ei     = 0.5d0*phi
            Ej     = 0.5d0*phi
         else                                  ! FULL mode
            Ei     = 0.5d0*phi
            Ej     = 0.0d0
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + Ei          ! accumulate energy Ei
            ene_pot(j) = ene_pot(j) + Ej          ! accumulate energy Ej
         elseif (comp_energy.eq.1) then
            energy = energy + Ei                  ! accumulate energy
            energy = energy + Ej                  ! accumulate energy
         endif

         ! contribution to virial tensor
         !
         if (comp_virial.eq.1) then
            virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dphieff/r
            virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dphieff/r
            virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dphieff/r
            virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dphieff/r
            virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dphieff/r
            virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dphieff/r
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then                        ! Ei contribution
            force(:,i) = force(:,i) + dphieff*Rij/r ! accumulate force on atom i
            force(:,j) = force(:,j) - dphieff*Rij/r ! accumulate force on atom j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

if (comp_enepot.eq.1 .and. comp_energy.eq.1) &
   energy = sum(ene_pot(1:N))                       ! compute total energy

! Free temporary storage
!
if (NBC.eq.0) deallocate( nei1atom_substitute )
deallocate( rho )
deallocate( U )
if (comp_force.eq.1.or.comp_virial.eq.1) deallocate( derU )

! Everything is great
!
ier = KIM_STATUS_OK
return

end subroutine Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Reset iterator to begin from first atom in list
!
!-------------------------------------------------------------------------------
subroutine reset_iterator(HalfOrFull,pkim,numnei,pnei1atom,pRij_list,ier)
implicit none 

!-- Transferred variables
integer,                  intent(in)    :: HalfOrFull
integer(kind=kim_intptr), intent(in)    :: pkim
integer,                  intent(out)   :: numnei
integer,                  intent(out)   :: ier
integer nei1atom(1);    pointer(pnei1atom,nei1atom)
real*8 Rij_list(DIM,1); pointer(pRij_list,Rij_list)

!-- Local variables
integer atom_ret

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
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
   else
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
   endif
   ier = KIM_STATUS_FAIL
   return 
endif

return
end subroutine reset_iterator

!-------------------------------------------------------------------------------
!
! Get list of neighbors for current atom using all NBC methods
!
!-------------------------------------------------------------------------------
subroutine get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                      atom,numnei,pnei1atom,pRij_list,ier)
implicit none 

!-- Transferred variables
integer,                  intent(in)    :: IterOrLoca
integer,                  intent(in)    :: HalfOrFull
integer,                  intent(in)    :: NBC
integer,                  intent(in)    :: N
integer(kind=kim_intptr), intent(in)    :: pkim
integer,                  intent(inout) :: atom
integer,                  intent(out)   :: numnei
integer,                  intent(out)   :: ier
integer nei1atom(1);    pointer(pnei1atom,nei1atom)
real*8 Rij_list(DIM,1); pointer(pRij_list,Rij_list)

!-- Local variables
integer atom_ret, jj

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
   if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) then
                          ! past end of the list, terminate loop in
      return              ! calling routine
   endif
   if (ier.lt.KIM_STATUS_OK) then     ! some sort of problem, exit
      if (HalfOrFull.eq.1) then
         call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
      else
         call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
      endif
      return
   endif
   atom = atom_ret

else                         ! LOCATOR mode

   atom = atom + 1
   if (atom.gt.N) then                     ! incremented past end of list,
      ier = KIM_STATUS_NEIGH_ITER_PAST_END ! terminate loop in calling routine
      return
   endif

   if (HalfOrFull.eq.1) then ! HALF list
      if (NBC.eq.0) then     ! CLUSTER NBC method
         numnei = N - atom   ! number of neighbors in list atom+1, ..., N
         nei1atom(1:numnei) = (/ (atom+jj, jj = 1,numnei) /)
         ier = KIM_STATUS_OK
      else
         ier = kim_api_get_half_neigh_f(pkim,1,atom,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      endif
   else                      ! FULL list
      ier = kim_api_get_full_neigh_f(pkim,1,atom,atom_ret,numnei, &
                                     pnei1atom,pRij_list)
   endif
   if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
      if (HalfOrFull.eq.1) then
         call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
      else
         call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
      endif
      ier = KIM_STATUS_FAIL
      return
   endif

endif

return
end subroutine get_current_atom_neighbors

end module model_<FILL element name>_PF_<FILL model name>

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_<FILL element name>_PF_<FILL model name>_init(pkim)
use model_<FILL element name>_PF_<FILL model name>
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
integer ier

!-- KIM variables
real*8 cutoff; pointer(pcutoff,cutoff)

! store pointer to compute function in KIM object
ier = kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces))
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
   stop
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
   stop
endif
cutoff = model_cutoff

end subroutine model_<FILL element name>_PF_<FILL model name>_init

