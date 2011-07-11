    integer :: i,j,jj,numnei,atom,atom_ret
    character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
    integer :: nbc                               ! 0 - half, 1 - full
    double precision, dimension(DIM) :: Rij
