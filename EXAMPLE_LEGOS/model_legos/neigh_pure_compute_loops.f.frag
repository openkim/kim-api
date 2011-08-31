    ! determine whether half or full neighbor lists are being used
    pNBC_Method = kim_api_get_nbc_method_f(pkim, ier)
    if (ier.lt.KIM_STATUS_OK) then
       call report_error(__LINE__, "kim_api_get_nbc_method_f", ier)
       return
    endif
    if (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
       HalfOrFull = 1
    elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
       HalfOrFull = 2
    else
       ier = KIM_STATUS_FAIL
       call report_error(__LINE__, "Unsupported NBC type", ier)
       return
    endif
    call free(pNBC_Method) ! don't forget to release the memory...

    !  Compute energy and forces
    !
    do i = 1,numberOfAtoms
       
       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       
       if (HalfOrFull.eq.1) then
          ier = kim_api_get_half_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       else
          ier = kim_api_get_full_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       endif
       if (ier.lt.KIM_STATUS_OK) then
          call report_error(__LINE__, "kim_api_get_*_neigh", ier)
          return
       endif
       
       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rij(:) = coor(:,j) - coor(:,i)                ! distance vector between i j
          Rsqij = dot_product(Rij,Rij)                  ! compute square distance
          if ( Rsqij < model_cutsq ) then               ! particles are interacting?
             r = sqrt(Rsqij)                            ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)                     ! compute pair potential
             if (comp_enepot.eq.1) then                 !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi     ! accumulate energy
                if (HalfOrFull.eq.1) then               !
                   ene_pot(j) = ene_pot(j) + 0.5d0*phi  ! (i and j share it)
                endif                                   !
             else                                       !
                if (HalfOrFull.eq.1) then               !
                   energy = energy + phi                ! half neigh case
                else                                    !
                   energy = energy + 0.5d0*phi          ! full neigh case
                endif                                   !
             endif
             if (comp_virial.eq.1) then                 !
                if (HalfOrFull.eq.1) then               !
                   virial = virial + r*dphi             ! accumul. virial=sum r(dV/dr)
                else                                    !
                   virial = virial + 0.5d0*r*dphi       !
                endif                                   !
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) + dphi*Rij/r    ! accumulate forces
                if (HalfOrFull.eq.1) then               !
                   force(:,j) = force(:,j) - dphi*Rij/r ! (Fji = -Fij)
                endif                                   !
             endif
          endif
       enddo
    enddo
