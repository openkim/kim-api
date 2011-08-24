!
! Release: This file is part of the openkim-api.git repository.
!
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
! All rights reserved.
!
! Authors: Valeriu Smirichinski, Ryan S. Elliot, Ellad B. Tadmor
!


module model_Ne_P_LJ_NEIGH_PURE_H
  use  KIMservice
  implicit none
  
  save
  private
  public model_cutoff
  public calculate_wrap_f77
  public report_error

  !-- LJ parameters
  real*8, parameter :: model_cutoff  = 8.1500d0
  real*8, parameter :: sigma         = 2.7400d0
  real*8, parameter :: epsilon       = 0.0031d0

contains
  
  !-----------------------------------------------------------------------------
  !
  ! Computes energy and forces on atoms from the positions.
  ! (f90 wrapper that calls the actual f77 routine)
  !
  !-----------------------------------------------------------------------------
  subroutine calculate_wrap_f77(pkim,ier) ! compute routine with KIM interface
    use KIMservice
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in)  :: pkim
    integer,                  intent(out) :: ier

    !-- Local variables
    real*8 x(3,1);           pointer(px,x)                 ! position
    real*8 f(3,1);           pointer(pf,f)                 ! force
    real*8 ea(1);            pointer(pea,ea)               ! energy per atom
    real*8 potenergy;        pointer(ppotenergy,potenergy) ! total energy
    integer attypes(1);      pointer(pattypes,attypes)     ! atom types
    integer numberofatoms; pointer(pnumberofatoms,numberofatoms)
    integer i, f_flag, e_flag
    external calculate

    ! Unpack data from KIM object
    !
    pnumberofatoms = kim_api_get_data_f(pkim,"numberOfAtoms",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", ier)
       stop
    endif

    pattypes = kim_api_get_data_f(pkim,"atomTypes", ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier)
       stop
    endif
    do i=1,numberofatoms
       if (attypes(i).ne.1) then ! check for correct atom types Ne=1
          call report_error(__LINE__, "Wrong Atom Type", i);
          stop
       endif
    enddo

    px=kim_api_get_data_f(pkim,"coordinates",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", ier)
       stop
    endif

    pf=kim_api_get_data_f(pkim,"forces",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", ier)
       stop
    endif

    ppotenergy = kim_api_get_data_f(pkim,"energy",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", ier)
       stop
    endif

    pea = kim_api_get_data_f(pkim,"energyPerAtom",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", ier)
       stop
    endif

    ! Check to see if we have been asked to compute the forces, energyperatom,
    ! and virial
    !
    f_flag=kim_api_isit_compute_f(pkim,"forces",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_isit_compute_f", ier)
       stop
    endif

    e_flag=kim_api_isit_compute_f(pkim,"energyPerAtom",ier)
    if (ier.ne.1) then
       call report_error(__LINE__, "kim_api_isit_compute_f", ier)
       stop
    endif

    ! Call FORTRAN 77 code that does actual calculation
    !
    call calculate(model_cutoff,sigma,epsilon,pkim,x,f,ea,numberofatoms,potenergy,  &
                   f_flag,e_flag,kim_api_get_half_neigh_f,ier)

  end subroutine calculate_wrap_f77
  
  !-----------------------------------------------------------------------------
  !
  ! error reporting routine
  !
  !-----------------------------------------------------------------------------
  subroutine report_error(line, str, status)
    implicit none

    !-- Transferred variables
    integer,          intent(in) :: line
    character(len=*), intent(in) :: str
    integer,          intent(in) :: status

    !-- Local variables
    character(len=10000), parameter :: file = __FILE__

    !-- print the error message
    print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', status

  end subroutine report_error

end module model_Ne_P_LJ_NEIGH_PURE_H


!  Model Initiation routine
subroutine model_Ne_P_LJ_NEIGH_PURE_H_init(pkim)
  use model_Ne_P_LJ_NEIGH_PURE_H
  use KIMservice
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in) :: pkim

  !-- Local variables
  integer ier
  integer(kind=kim_intptr) one

  !-- KIM variables
  real*8 cutoff;  pointer(pcutoff,cutoff)  ! cutoff radius

  ! store pointer to compute function in KIM object
  one=1
  ier = kim_api_set_data_f(pkim,"compute",one,loc(calculate_wrap_f77))
  if (ier.ne.1)  then
     call report_error(__LINE__, "kim_api_set_data_f", ier)
     stop
  endif

  ! store model cutoff in KIM object
  pcutoff = kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.ne.1) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  cutoff = model_cutoff

end subroutine model_Ne_P_LJ_NEIGH_PURE_H_init
