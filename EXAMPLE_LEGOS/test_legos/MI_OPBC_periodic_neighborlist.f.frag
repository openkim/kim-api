!-------------------------------------------------------------------------------
!
! MI_OPBC_periodic_neighborlist : construct a half or full neighbor list using the
!                                 atom coordinates in coords()
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_periodic_neighborlist(half, numberOfParticles, coords, rcut, boxSideLengths, MiddleAtomID, neighborList)
  use KIM_API
  implicit none

  !-- Transferred variables
  logical,                                                     intent(in)  :: half
  integer,                                                     intent(in)  :: numberOfParticles
  double precision, dimension(3,numberOfParticles),            intent(in)  :: coords
  double precision,                                            intent(in)  :: rcut
  double precision, dimension(3),                              intent(in)  :: boxSideLengths
  integer,                                                     intent(in)  :: MiddleAtomID
  integer,   dimension(numberOfParticles+1,numberOfParticles), intent(out) :: neighborList ! not memory efficient

  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision rcut2

  rcut2 = rcut**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        where (abs(dx) > 0.5d0*boxSideLengths)  ! apply PBC
           dx = dx - sign(boxSideLengths,dx)
        endwhere
        r2 = dot_product(dx, dx)
        if (r2.le.rcut2) then
           ! atom j is a neighbor of atom i
           if (half) then
               if ( ((i.eq.MiddleAtomId) .or. (j.eq.MiddleAtomId)) .and. (i .lt. j) ) then
                  a = a+1
                  neighborList(a,i) = j
               endif
           else
               if (i.eq.MiddleAtomId) then
                  a = a+1
                  neighborList(a,i) = j
               endif
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo

  return

end subroutine MI_OPBC_periodic_neighborlist
