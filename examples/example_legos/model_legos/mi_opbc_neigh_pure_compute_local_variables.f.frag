    integer(c_int) i,j,jj,numnei,part,part_ret
    character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method
    integer(c_int) HalfOrFull ! 1 - half, 2 - full
    real(c_double), dimension(DIM) :: Rij
    type(c_ptr) :: pRij
