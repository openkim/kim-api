!-------------------------------------------------------------------------------
!
! get_neigh_no_Rij neighbor list access function 
!
! This function implements Locator and Iterator modes
!
!-------------------------------------------------------------------------------
integer function get_neigh_no_Rij(pkim,mode,request,atom,numnei,pnei1atom,pRij)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim
  integer,                  intent(in)  :: mode
  integer,                  intent(in)  :: request
  integer,                  intent(out) :: atom
  integer,                  intent(out) :: numnei
  integer nei1atom(1); pointer(pnei1atom, nei1atom) ! actual cray pointer associated with nei1atom
  real*8  Rij(3,1);    pointer(pRij, Rij)
  
  !-- Local variables
  integer, save :: iterVal = 0
  integer  atomToReturn
  integer  neighborListdum(1); pointer(pneighborListdum, neighborListdum)
  integer, pointer :: neighborList(:,:)
  integer  ier
  integer  numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer  N

  ! unpack neighbor list object
  pneighborListdum = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  N = numberOfAtoms
  call toIntegerArrayWithDescriptor2d(neighborListdum, neighborlist, N+1, N)

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_neigh_no_Rij = 2
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_neigh_no_Rij = 0
           return
        else
           atomToReturn = iterVal
        endif
     else
        call report_error(__LINE__, "Invalid request in get_neigh_no_Rij", -6)
        get_neigh_no_Rij = -6 ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        call report_error(__LINE__, "Invalid request in get_neigh_no_Rij", -1)
        get_neigh_no_Rij = -1
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     call report_error(__LINE__, "Invalid mode in get_neigh_no_Rij", -2)
     get_neigh_no_Rij = -2
     return
  endif
  
  ! set the returned atom
  atom = atomToReturn
  
  ! set the returned number of neighbors for the returned atom
  numnei = neighborList(1,atom)
  
  ! set the location for the returned neighbor list
  pnei1atom = loc(neighborList(2,atom))
  
  ! set pointer to Rij to NULL
  pRij = 0
  
  get_neigh_no_Rij = 1
  return
end function get_neigh_no_Rij
