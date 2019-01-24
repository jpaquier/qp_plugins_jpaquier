program dirac_density
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
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   call dirac_ex_LDA_sr(mu_erf,rho(istate),e_x,v_x)
   print*,r(1),r(2),r(3), rho(istate)
   open (10, file='density_X',position ='append')
   write(10,*)  r(1),r(2),r(3),weight,rho(istate)
  enddo
 enddo

end
