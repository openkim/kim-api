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
! Copyright (c) 2013--2015, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
!


!*******************************************************************************
!**
!**  MODULE MODEL_NAME_STR
!**
!**  Lennard-Jones pair potential model for argon
!**  (modified by adding a quadratic function to have a smooth cutoff)
!**
!**  Release: This file is part of the kim-api.git repository.
!**
!*******************************************************************************

#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__

module MODEL_NAME_STR
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  save
  private
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  public Compute_Energy_Forces
  public ReInit
  public Destroy

contains

!-------------------------------------------------------------------------------
!
! Compute energy and forces on particles from the positions.
!
!-------------------------------------------------------------------------------
  integer(c_int) function Compute_Energy_Forces(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in)  :: pkim

    !-- Local variables
    integer(c_int), parameter :: DIM=3
    real(c_double) r,Rsqij,phi,dphi,d2phi,dEidr
    integer(c_int) idum
