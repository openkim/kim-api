!*******************************************************************************
!**
!**  MODULE MODEL_NAME_STR
!**
!**  Lennard-Jones pair potential model for argon 
!**  (modified by adding a quadratic function to have a smooth cutoff)
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!*******************************************************************************

module MODEL_NAME_STR
  use KIMservice
  implicit none

  save
  private
  public Compute_Energy_Forces
  public ReInit
  public Destroy
  public report_error
  
contains
  
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
    integer, parameter :: DIM=3
    double precision r,Rsqij,phi,dphi,d2phi
