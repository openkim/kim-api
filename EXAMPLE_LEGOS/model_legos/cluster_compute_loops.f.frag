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
             if (comp_enepot.eq.1) then                 !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi     ! accumulate energy
                ene_pot(j) = ene_pot(j) + 0.5d0*phi     ! (i and j share it)
             else                                       !
                energy = energy + phi                   ! half neigh case
             endif                                      !
             if (comp_virial.eq.1) then                 !
                virial = virial + r*dphi                ! accumul. virial=sum r(dV/dr)
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) + dphi*Rij/r    ! accumulate forces
                force(:,j) = force(:,j) - dphi*Rij/r    ! (Fji = -Fij)
             endif
          endif
       enddo
    enddo
