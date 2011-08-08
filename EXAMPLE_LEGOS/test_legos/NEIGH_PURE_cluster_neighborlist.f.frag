!-------------------------------------------------------------------------------
!
! NEIGH_PURE_cluster_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_cluster_neighborlist(half, numberOfAtoms, coords, cutoff, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  logical,                                             intent(in)  :: half
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
  integer,   dimension(numberOfAtoms+1,numberOfAtoms), intent(out) :: neighborList ! not memory efficient
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2
  
  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
     do j=1,numberOfAtoms
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! atom j is a neighbor of atom i
           if ( (j .gt. i) .OR. ((.not. half) .AND. (i.ne.j)) ) then
               a = a+1
               neighborList(a,i) = j
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_PURE_cluster_neighborlist
