    ! determine whether half or full neighbor lists are being used
    Compute_Energy_Forces = kim_api_get_nbc_method(pkim, NBC_Method)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,   &
                                   "kim_api_get_nbc_method", &
                                   Compute_Energy_Forces)
       return
    endif
    if (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
       HalfOrFull = 1
       ! get numberContributingParticles
       pnumContrib = kim_api_get_data(pkim,"numberContributingParticles", &
                                      Compute_Energy_Forces)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_data", Compute_Energy_Forces)
          return
       endif
       call c_f_pointer(pnumContrib, numContrib)
    elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
       HalfOrFull = 2
       allocate( numContrib )
    else
       Compute_Energy_Forces = KIM_STATUS_FAIL
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "Unsupported NBC type",   &
                                   Compute_Energy_Forces)
       return
    endif

    !  Compute energy and forces
    !
    do i = 1,numberOfParticles

       ! Get neighbors for particle i
       !
       part = i ! request neighbors for particle i

       Compute_Energy_Forces = kim_api_get_neigh(pkim,1,part,part_ret,numnei, &
                                                 pnei1part,pRij)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_neigh",      &
                                      Compute_Energy_Forces)
          return
       endif
       call c_f_pointer(pnei1part, nei1part, [numnei])

       ! Loop over the neighbors of particle i
       !
       do jj = 1, numnei
          j = nei1part(jj)
          Rij(:) = coor(:,j) - coor(:,i)    ! distance vector between i j
          Rsqij = dot_product(Rij,Rij)      ! compute square distance
          if ( Rsqij < model_cutsq ) then   ! particles are interacting?
             r = sqrt(Rsqij)                ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)         ! compute pair potential
             if ((HalfOrFull.eq.1) .and. &
                 (j .le. numContrib)) then  ! HALF mode
                dEidr = dphi                !      double contribution
             else                           ! FULL mode
                dEidr = 0.5_cd*dphi         !      regular contribution
             endif
             if (comp_enepot.eq.1) then
                enepot(i) = enepot(i) + 0.5_cd*phi ! accumulate energy
                if ((HalfOrFull.eq.1) .and. &
                     (j .le. numContrib)) then
                   enepot(j) = enepot(j) + 0.5_cd*phi  ! (i and j share it)
                endif
             endif
             if (comp_energy.eq.1) then
               if ((HalfOrFull.eq.1) .and. &
                   (j .le. numContrib)) then           ! HALF mode
                   energy = energy + phi               ! half neigh case
                else
                   energy = energy + 0.5_cd*phi        ! full neigh case
                endif
             endif
             if (comp_virial.eq.1) then                ! accumul. virial
                virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r
                virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r
                virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r
                virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r
                virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r
                virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r
             endif
             if (comp_force.eq.1) then
                force(:,i) = force(:,i) + dEidr*Rij/r  ! accumulate forces on i
                force(:,j) = force(:,j) - dEidr*Rij/r  ! accumulate forces on j
             endif
          endif
       enddo
    enddo

    if (HalfOrFull.eq.2) deallocate( numContrib )