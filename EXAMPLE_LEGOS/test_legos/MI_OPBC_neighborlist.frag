!-------------------------------------------------------------------------------
!
! MI_OPBC_neighborlist : construct a half or full neighbor list using the
!                        atom coordinates in coords()
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_neighborlist(half, numberOfAtoms, coords, rcut, boxlength, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  logical,                                             intent(in)  :: half
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: rcut
  double precision, dimension(3),                      intent(in)  :: boxlength
  integer,   dimension(numberOfAtoms+1,numberOfAtoms), intent(out) :: neighborList ! not memory efficient
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision rcut2
  
  rcut2 = rcut**2
  
  do i=1,numberOfAtoms
     a = 1
     do j=1,numberOfAtoms
        dx = coords(:, j) - coords(:, i)
        where (abs(dx) > 0.5d0*boxlength)  ! apply PBC
           dx = dx - sign(boxlength,dx)
        endwhere
        r2 = dot_product(dx, dx)
        if (r2.le.rcut2) then
           if (i.ne.j) then
              if ( (j .gt. i) .or. (.not. half) ) then
                  ! atom j is a neighbor of atom i
                  a = a+1
                  neighborList(a,i) = j
              endif
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine MI_OPBC_neighborlist
