    !  Compute energy and forces
    !
    do i = 1,numberOfParticles

       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       Compute_Energy_Forces = kim_api_get_neigh(pkim,1,atom,atom_ret,numnei, &
                                                 pnei1atom,pRij)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_neigh",      &
                                      Compute_Energy_Forces)
          return
       endif
       call c_f_pointer(pnei1atom, nei1atom, [numnei])
       call c_f_pointer(pRij,      Rij,      [DIM,numnei])

       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rsqij = dot_product(Rij(:,jj),Rij(:,jj))  ! compute square distance
          if ( Rsqij < model_cutsq ) then           ! particles are interacting?
             r = sqrt(Rsqij)                        ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)                 ! compute pair potential
             dEidr = 0.5_cd*dphi
             if (comp_enepot.eq.1) then
                enepot(i) = enepot(i) + 0.5_cd*phi  ! accumulate energy
             endif
             if (comp_energy.eq.1) then
                energy = energy + 0.5_cd*phi                ! full neigh case
             endif
             if (comp_virial.eq.1) then                     ! accumul. virial
                virial(1) = virial(1) + Rij(1,jj)*Rij(1,jj)*dEidr/r
                virial(2) = virial(2) + Rij(2,jj)*Rij(2,jj)*dEidr/r
                virial(3) = virial(3) + Rij(3,jj)*Rij(3,jj)*dEidr/r
                virial(4) = virial(4) + Rij(2,jj)*Rij(3,jj)*dEidr/r
                virial(5) = virial(5) + Rij(1,jj)*Rij(3,jj)*dEidr/r
                virial(6) = virial(6) + Rij(1,jj)*Rij(2,jj)*dEidr/r
             endif
             if (comp_force.eq.1) then
                force(:,i) = force(:,i) + dEidr*Rij(:,jj)/r ! accumulate f on j
                force(:,j) = force(:,j) - dEidr*Rij(:,jj)/r ! accumulate f on i
             endif
          endif
       enddo
    enddo
