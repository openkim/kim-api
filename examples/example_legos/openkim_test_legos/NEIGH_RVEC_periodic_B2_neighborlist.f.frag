!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_periodic_B2_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_periodic_B2_neighborlist(half,CellsPerHalfSide, cutoff,  &
                                               B2spacing, neighObject, coords)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  logical,                             intent(in)    :: half
  integer(c_int),                      intent(in)    :: CellsPerHalfSide
  real(c_double),                      intent(in)    :: cutoff
  real(c_double),                      intent(in)    :: B2spacing
  type(neighObject_type),              intent(inout) :: neighObject
  real(c_double), dimension(3,2),      intent(out)   :: coords

  !-- Local variables
  real(c_double) dx(3)
  real(c_double) cutoff2
  real(c_double) B2shifts(3,2)
  real(c_double) latVec(3)
  integer(c_int) part, a, i, j, k, m

  cutoff2 = cutoff**2

  ! Cubic B2 cell positions ----------------------------------------------------
  B2shifts(1,1) = 0.0_cd
  B2shifts(2,1) = 0.0_cd
  B2shifts(3,1) = 0.0_cd
  B2shifts(1,2) = 0.5_cd*B2spacing
  B2shifts(2,2) = 0.5_cd*B2spacing
  B2shifts(3,2) = 0.5_cd*B2spacing

  ! set coords
  coords(:,1) = B2shifts(1,1)
  coords(:,2) = B2shifts(1,2)

  if (half) then
     ! Each part gets half of its own neighbor-self images
     do part = 2,1,-1  ! have part 1 be last in the loop
        a = 1
        do i = 0, CellsPerHalfSide
           latVec(1) = i*B2spacing
           do j = -CellsPerHalfSide, CellsPerHalfSide
              latVec(2) = j*B2spacing
              do k = -CellsPerHalfSide, CellsPerHalfSide
                 latVec(3) = k*B2spacing
                 if (i.gt.0 .or. (i.eq.0 .and. (j.gt.0 .or. &
                                                  (j.eq.0 .and. k.gt.0)))) then
                    dx = latVec
                    if (dot_product(dx,dx).lt.cutoff2) then
                       ! we have a neighbor
                       a = a + 1
                       neighObject%neighborList(a,part) = part
                       neighObject%RijList(:,a-1,part)  = dx
                    endif
                 endif
              enddo
           enddo
        enddo
        ! this part has a-1 neighbors (so far)
        neighObject%neighborList(1,part) = a - 1
     enddo

     ! Part 1 gets all images of part 2; part 2 gets no part 1 images
     do i = -CellsPerHalfSide, CellsPerHalfSide
        latVec(1) = i*B2spacing
        do j = -CellsPerHalfSide, CellsPerHalfSide
           latVec(2) = j*B2spacing
           do k = -CellsPerHalfSide, CellsPerHalfSide
              latVec(3) = k*B2spacing
              dx = (latVec + B2shifts(:,2)) - B2shifts(:,1)
              if (dot_product(dx,dx).lt.cutoff2) then
                 ! we have a neighbor
                 a = a + 1
                 neighObject%neighborList(a,part) = 2
                 neighObject%RijList(:,a-1,part)  = dx
              endif
           enddo
        enddo
     enddo

     ! part 1 has a-1 neighbors
     neighObject%neighborList(1,1) = a - 1
  else
     do part = 1,2
        a = 1
        do i=-CellsPerHalfSide,CellsPerHalfSide
           latVec(1) = i*B2spacing
           do j=-CellsPerHalfSide,CellsPerHalfSide
              latVec(2) = j*B2spacing
              do k=-CellsPerHalfSide,CellsPerHalfSide
                 latVec(3) = k*B2spacing
                 do m=1,2
                    dx = (latVec + B2shifts(:,m)) - B2shifts(:,part)
                    if (dot_product(dx,dx).lt.cutoff2) then
                       if (.not.( (i.eq.0) .and. (j.eq.0) .and. (k.eq.0) .and. &
                                  (m.eq.part) )) then
                          ! we have a neighbor
                          a = a+1
                          neighObject%neighborList(a,part) = m
                          neighObject%RijList(:,a-1,part)  = dx
                       endif
                    endif
                 enddo
              enddo
           enddo
        enddo
        ! part 1 has a-1 neighbors
        neighObject%neighborList(1,part) = a-1
     enddo
  endif

  return

end subroutine NEIGH_RVEC_periodic_B2_neighborlist
