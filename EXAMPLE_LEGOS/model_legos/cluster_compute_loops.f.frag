    !  Compute energy and forces
    !
    ! We'll use a half list approach
    ! Don't need to consider the last atom since all its interactions
    ! are accounted for earlier in the loop
    do i = 1, numberOfAtoms-1
       ! Loop over atoms > i
       do j = i+1, numberOfAtoms
          Rij(:) = coor(:,j) - coor(:,i)                ! distance vector between i j
          Rsqij = dot_product(Rij,Rij)                  ! compute square distance
          if ( Rsqij < model_cutsq ) then               ! particles are interacting?
             r = sqrt(Rsqij)                            ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)                     ! compute pair potential
             dEidr = dphi                               ! compute dEidr -- double contribution
             if (comp_enepot.eq.1) then                 !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi     ! accumulate energy
                ene_pot(j) = ene_pot(j) + 0.5d0*phi     ! (i and j share it)
             elseif (comp_energy.eq.1) then             !
                energy = energy + phi                   ! half neigh case
             endif                                      !
             if (comp_virial.eq.1) then                 !
                virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dEidr/r
                virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dEidr/r
                virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dEidr/r
                virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dEidr/r
                virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dEidr/r
                virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dEidr/r
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) + dEidr*Rij/r   ! accumulate force on atom i
                force(:,j) = force(:,j) - dEidr*Rij/r   ! accumulate force on atom j
             endif
          endif
       enddo
    enddo
