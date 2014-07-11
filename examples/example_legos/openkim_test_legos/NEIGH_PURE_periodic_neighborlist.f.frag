!-------------------------------------------------------------------------------
!
! NEIGH_PURE_periodic_neighborlist  (only give middle particle neighbors)
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_periodic_neighborlist(half, numberOfParticles, coords, &
                                            cutoff, MiddlePartId, neighObject)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  logical,                intent(in)    :: half
  integer(c_int),         intent(in)    :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                          intent(in)    :: coords
  real(c_double),         intent(in)    :: cutoff
  integer(c_int),         intent(in)    :: MiddlePartId
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2

  cutoff2 = cutoff**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! part j is a neighbor of part i
           if (half) then
               if ( ((i.eq.MiddlePartId) .or. (j.eq.MiddlePartId)) .and. &
                    (i .lt. j) ) then
                  a = a+1
                  neighObject%neighborList(a,i) = j
               endif
           else
               if (i.eq.MiddlePartId .and. i.ne.j) then
                  a = a+1
                  neighObject%neighborList(a,i) = j
               endif
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_PURE_periodic_neighborlist
