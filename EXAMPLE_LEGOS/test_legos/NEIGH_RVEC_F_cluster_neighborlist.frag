!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_F_cluster_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_F_cluster_neighborlist(numberOfAtoms, coords, cutoff, NN, neighborList, RijList)
  use KIMservice
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),               intent(in)  :: coords
  double precision,                                           intent(in)  :: cutoff
  integer,                                                    intent(in)  :: NN
  integer, dimension(NN+1,numberOfAtoms),                     intent(out) :: neighborList
  double precision, dimension(3,NN+1,numberOfAtoms),          intent(out) :: RijList
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2

  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
     do j=1,numberOfAtoms
        dx = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           if (i.ne.j) then
              ! atom j is a neighbor of atom i
              a = a+1
              neighborList(a,i) = j
              RijList(:,a-1,i) = dx
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine NEIGH_RVEC_F_cluster_neighborlist
