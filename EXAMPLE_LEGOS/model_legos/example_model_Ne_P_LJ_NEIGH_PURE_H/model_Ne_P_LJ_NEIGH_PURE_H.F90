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
  public calculate_wrap_f77
  public report_error
  
contains
  
  ! calculates forces per atom and total energy (f90 wrapper that calls actual f77 routine)
  subroutine calculate_wrap_f77(pkim,kimerr) ! compute routine with KIM interface
    use KIMservice
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr) :: kim; pointer(pkim,kim)
    integer, intent(out)     :: kimerr

    !-- Local variables
    real*8 :: x(3,1);     pointer(px,x)                 ! position
    real*8 :: f(3,1);     pointer(af,f)                 ! force
    real*8 :: ea(1);      pointer(aea,ea)               ! energy per atom
    real*8 :: potenergy;  pointer(apotenergy,potenergy) ! total energy
    real*8 :: xcutof;     pointer(pcutoff,xcutof)       ! cutoff
    integer:: attypes(1); pointer(aattypes,attypes)     ! atom types
    integer*8::numberofatoms; pointer(anumberofatoms,numberofatoms)
    integer :: i, natom, f_flag, e_flag
    external calculate

    ! get data from kim object
    anumberofatoms = kim_api_get_data_f(pkim,"numberOfAtoms",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", kimerr)
       stop
    endif
    natom = numberofatoms

    aattypes = kim_api_get_data_f(pkim,"atomTypes", kimerr);
    if (kimerr.le.0) then
       call report_error(__LINE__, "kim_api_get_data", kimerr);
       stop
    endif
    do i=1,natom
       if (attypes(i).ne.1) then ! check for correct atom types Ne=1
          call report_error(__LINE__, "Wrong Atom Type", i);
          stop
       endif
    enddo

    px=kim_api_get_data_f(pkim,"coordinates",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", kimerr)
       stop
    endif

    af=kim_api_get_data_f(pkim,"forces",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", kimerr)
       stop
    endif

    apotenergy = kim_api_get_data_f(pkim,"energy",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", kimerr)
       stop
    endif

    aea = kim_api_get_data_f(pkim,"energyPerAtom",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", kimerr)
       stop
    endif

    pcutoff = kim_api_get_data_f(pkim,"cutoff",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_get_data_f", kimerr)
       stop
    endif

    ! check if requested to compute forces and energy per atom
    f_flag=kim_api_isit_compute_f(pkim,"forces",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_isit_compute_f", kimerr)
       stop
    endif

    e_flag=kim_api_isit_compute_f(pkim,"energyPerAtom",kimerr)
    if (kimerr.ne.1) then
       call report_error(__LINE__, "kim_api_isit_compute_f", kimerr)
       stop
    endif

    call calculate(pkim,x,f,ea,natom,potenergy,xcutof,f_flag,e_flag,kim_api_get_half_neigh,kimerr)
  end subroutine calculate_wrap_f77
  
  subroutine report_error(line, str, status)
    implicit none

    !-- Transferred variables
    integer,   intent(in) :: line
    character(len=*), intent(in) :: str
    integer,   intent(in) :: status

    !-- Local variables
    character(len=10000), parameter :: file = __FILE__

    !-- print the error message
    print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', status

  end subroutine report_error

end module model_Ne_P_LJ_NEIGH_PURE_H


!  Model Initiation routine (it calls actual initialization routine in the module model_Ne_P_LJ_NEIGH_PURE_H)
subroutine model_Ne_P_LJ_NEIGH_PURE_H_init(pkim)
        use model_Ne_P_LJ_NEIGH_PURE_H
        use KIMservice
        implicit none

        !-- Transferred variables
        integer(kind=kim_intptr) :: kim; pointer(pkim,kim) 

        !-- Local variables
        integer::kimerr
        integer(kind=kim_intptr) ::sz
        real*8 :: xcutof;    pointer(pcutoff,xcutof)       ! cutoff

        pcutoff = kim_api_get_data_f(pkim,"cutoff",kimerr)
        if (kimerr.ne.1) then
           call report_error(__LINE__, "kim_api_get_data_f", kimerr)
           stop
        endif
        xcutof = 8.15

        !setting pointer to compute method
        sz=1
        kimerr = kim_api_set_data_f(pkim,"compute",sz,loc(calculate_wrap_f77))
        if (kimerr.ne.1)  then
           call report_error(__LINE__, "kim_api_set_data_f", kimerr)
           stop
        endif

end subroutine model_Ne_P_LJ_NEIGH_PURE_H_init
