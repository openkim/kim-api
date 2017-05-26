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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_unit_system_id_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    a_id, &
    bohr_id, &
    cm_id, &
    m_id, &
    nm_id, &

    amu_a2_per_ps2_id, &
    erg_id, &
    ev_id, &
    hartree_id, &
    j_id, &
    kcal_mol_id, &

    c_id, &
    e_id, &
    statc_id, &

    k_id, &

    fs_id, &
    ps_id, &
    ns_id, &
    s_id

  integer(c_int), parameter :: a_id = 1
  integer(c_int), parameter :: bohr_id = 2
  integer(c_int), parameter :: cm_id = 3
  integer(c_int), parameter :: m_id = 4
  integer(c_int), parameter :: nm_id = 5

  integer(c_int), parameter :: amu_a2_per_ps2_id = 1
  integer(c_int), parameter :: erg_id = 2
  integer(c_int), parameter :: ev_id = 3
  integer(c_int), parameter :: hartree_id = 4
  integer(c_int), parameter :: j_id = 5
  integer(c_int), parameter :: kcal_mol_id = 6

  integer(c_int), parameter :: c_id = 1
  integer(c_int), parameter :: e_id = 2
  integer(c_int), parameter :: statc_id = 3

  integer(c_int), parameter :: k_id = 1

  integer(c_int), parameter :: fs_id = 1
  integer(c_int), parameter :: ps_id = 2
  integer(c_int), parameter :: ns_id = 3
  integer(c_int), parameter :: s_id = 4
end module kim_unit_system_id_module
