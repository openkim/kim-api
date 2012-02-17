!-------------------------------------------------------------------------------
!
! get_neigh_no_Rij neighbor list access function 
!
! This function implements Locator and Iterator modes
!
!-------------------------------------------------------------------------------
integer function get_neigh_no_Rij(pkim,mode,request,atom,numnei,pnei1atom,pRij)
  use KIM_API
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
  integer  ier, idum
  integer  numberOfParticles; pointer(pnAtoms, numberOfParticles)
  integer  N

  ! unpack neighbor list object
  pneighborListdum = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfParticles", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  N = numberOfParticles
  call KIM_to_F90_int_array_2d(neighborListdum, neighborlist, N+1, N)

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_neigh_no_Rij = KIM_STATUS_NEIGH_ITER_INIT_OK
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_neigh_no_Rij = KIM_STATUS_NEIGH_ITER_PAST_END
           return
        else
           atomToReturn = iterVal
        endif
     else
        idum = kim_api_report_error_f(__LINE__, __FILE__, "Invalid request in get_neigh_no_Rij", KIM_STATUS_NEIGH_INVALID_REQUEST)
        get_neigh_no_Rij = KIM_STATUS_NEIGH_INVALID_REQUEST
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "Invalid atom ID in get_neigh_no_Rij", KIM_STATUS_PARTICLE_INVALID_ID)
        get_neigh_no_Rij = KIM_STATUS_PARTICLE_INVALID_ID
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     idum = kim_api_report_error_f(__LINE__, __FILE__, "Invalid mode in get_neigh_no_Rij", KIM_STATUS_NEIGH_INVALID_MODE)
     get_neigh_no_Rij = KIM_STATUS_NEIGH_INVALID_MODE
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
  
  get_neigh_no_Rij = KIM_STATUS_OK
  return
end function get_neigh_no_Rij
