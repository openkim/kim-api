    !  Compute energy and forces
    !
    ! We'll use a half list approach
    ! Don't need to consider the last atom since all its interactions
    ! are accounted for earlier in the loop
    do i = 1, numberOfParticles-1
       ! Loop over atoms > i
       do j = i+1, numberOfParticles
          Rij(:) = coor(:,j) - coor(:,i)    ! distance vector between i j
          Rsqij = dot_product(Rij,Rij)      ! compute square distance
          if ( Rsqij < model_cutsq ) then   ! particles are interacting?
             r = sqrt(Rsqij)                ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)         ! compute pair potential
             dEidr = dphi                   ! compute dEidr -- double contribute
             if (comp_enepot.eq.1) then
                enepot(i) = enepot(i) + 0.5_cd*phi ! accumulate energy
                enepot(j) = enepot(j) + 0.5_cd*phi ! (i and j share it)
             endif
             if (comp_energy.eq.1) then
                energy = energy + phi              ! half neigh case
             endif
             if (comp_virial.eq.1) then
                virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r
                virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r
                virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r
                virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r
                virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r
                virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r
             endif
             if (comp_force.eq.1) then
                force(:,i) = force(:,i) + dEidr*Rij/r  ! accumulate force atom i
                force(:,j) = force(:,j) - dEidr*Rij/r  ! accumulate force atom j
             endif
          endif
       enddo
    enddo
