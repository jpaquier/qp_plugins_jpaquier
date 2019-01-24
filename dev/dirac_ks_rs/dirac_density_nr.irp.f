program dirac_density_nr
 implicit none
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: e_x,v_x,ex_ab,vx_a,vx_b
 double precision, allocatable :: rho(:), rhoa(:),rhob(:)
 allocate(rho(N_states), rhoa(N_states),rhob(N_states))
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i)
   rhoa(istate) = one_e_dm_alpha_at_r(i,istate)
   rhob(istate) = one_e_dm_beta_at_r(i,istate)
   call ex_LDA_sr(mu_erf,rhoa(istate),rhob(istate),ex_ab,vx_a,vx_b)
   print*,r(1),r(2),r(3), rhoa(istate), rhob(istate)
   open (12, file='density_X_nr')
   write(12,*)  rhoa(istate),rhob(istate)
  enddo
 enddo

end
