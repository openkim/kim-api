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
! Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
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
!**  Release: This file is part of the openkim-api.git repository.
!**
!*******************************************************************************

#include "KIM_API_status.h"

module MODEL_NAME_STR
  use KIM_API
  implicit none

  save
  private
  public Compute_Energy_Forces
  public ReInit
  public Destroy

contains

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
  integer function Compute_Energy_Forces(pkim)
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in)  :: pkim

    !-- Local variables
    integer, parameter :: DIM=3
    double precision r,Rsqij,phi,dphi,d2phi,dEidr
    integer idum
