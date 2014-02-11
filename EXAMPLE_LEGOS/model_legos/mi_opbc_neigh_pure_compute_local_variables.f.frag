    integer(c_int) i,j,jj,numnei,atom,atom_ret
    character(len=KIM_KEY_STRING_LENGTH), pointer :: NBC_Method; type(c_ptr) :: pNBC_Method
    integer(c_int) HalfOrFull ! 1 - half, 2 - full
    real(c_double), dimension(DIM) :: Rij
    type(c_ptr) :: pRij
