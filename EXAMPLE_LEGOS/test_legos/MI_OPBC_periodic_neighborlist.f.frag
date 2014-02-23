!-------------------------------------------------------------------------------
!
! MI_OPBC_periodic_neighborlist : construct a half or full neighbor list using
!                                 the atom coordinates in coords()
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_periodic_neighborlist(half, numberOfParticles, coords,    &
                                         rcut, boxSideLengths, MiddleAtomID, &
                                         neighObject)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  logical,                intent(in)    :: half
  integer(c_int),         intent(in)    :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                          intent(in)    :: coords
  real(c_double),         intent(in)    :: rcut
  real(c_double), dimension(3), &
                          intent(in)    :: boxSideLengths
  integer(c_int),         intent(in)    :: MiddleAtomID
  type(neighObject_type), intent(inout) :: neighObject
  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) rcut2

  rcut2 = rcut**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        where (abs(dx) > 0.5_cd*boxSideLengths)  ! apply PBC
           dx = dx - sign(boxSideLengths,dx)
        endwhere
        r2 = dot_product(dx, dx)
        if (r2.le.rcut2) then
           ! atom j is a neighbor of atom i
           if (half) then
               if ( ((i.eq.MiddleAtomId) .or. (j.eq.MiddleAtomId)) .and. &
                    (i .lt. j) ) then
                  a = a+1
                  neighObject%neighborList(a,i) = j
               endif
           else
               if (i.eq.MiddleAtomId .and. i.ne.j) then
                  a = a+1
                  neighObject%neighborList(a,i) = j
               endif
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine MI_OPBC_periodic_neighborlist
