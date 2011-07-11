    ! determine which NBC scenerio to use
    pNBC_Method = kim_api_get_nbc_method(pkim, ier); if (ier.le.0) return
    if (index(NBC_Method,"MI-OPBC-H").eq.1) then
       nbc = 0
    elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
       nbc = 1
    else
       ier = 0
       call report_error(__LINE__, "Unknown NBC type", ier)
       return
    endif
    call free(pNBC_Method) ! don't forget to release the memory...

    !  Compute energy and forces
    !
    do i = 1,numberOfAtoms
       
       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       
       if (nbc.eq.0) then
          ier = kim_api_get_half_neigh(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       else
          ier = kim_api_get_full_neigh(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       endif
       if (ier.le.0) then
          call report_error(__LINE__, "kim_api_get_*_neigh", ier);
          return
       endif
       
       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rij = coor(:,i) - coor(:,j)                   ! distance vector between i j
          where ( abs(Rij) > 0.5d0*boxlength )          ! periodic boundary conditions
             Rij = Rij - sign(boxlength,Rij)            ! applied where needed.
          end where                                     ! 
          Rsqij = dot_product(Rij,Rij)                  ! compute square distance
          if ( Rsqij < model_cutsq ) then               ! particles are interacting?
             r = sqrt(Rsqij)                            ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)                     ! compute pair potential
             if (comp_enepot.eq.1) then                 !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi     ! accumulate energy
                if (nbc.eq.0) then                      !
                   ene_pot(j) = ene_pot(j) + 0.5d0*phi  ! (i and j share it)
                endif                                   !
             else                                       !
                if (nbc.eq.0) then                      !
                   energy = energy + phi                ! half neigh case
                else                                    !
                   energy = energy + 0.5d0*phi          ! full neigh case
                endif                                      !
             endif
             if (comp_virial.eq.1) then                 !
                if (nbc.eq.0) then                      !
                   virial = virial + r*dphi             ! accumul. virial=sum r(dV/dr)
                else                                    !
                   virial = virial + 0.5d0*r*dphi       !
                endif                                   !
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) - dphi*Rij/r    ! accumulate forces
                if (nbc.eq.0) then                      !
                   force(:,j) = force(:,j) + dphi*Rij/r ! (Fji = -Fij)
                endif                                   !
             endif
          endif
       enddo
    enddo
