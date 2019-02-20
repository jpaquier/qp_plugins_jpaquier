program dirac_density
 implicit none
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: f16,f13,f23,f43
 double precision :: e_x,v_x,ex_ab,vx_a,vx_b,kF
 double precision, allocatable :: rho(:), rhoa(:),rhob(:)
 allocate(rho(N_states), rhoa(N_states),rhob(N_states))
 f16 = 0.16666666666666666d0
 f13 = 0.3333333333333333d0
 f23 = 0.6666666666666667d0
 f43 = 1.3333333333333333d0
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
  !write(10,*)  r(1),r(2),r(3),rho(istate)
   write(10,*)  rho(istate), 3.0936677262801355d0*(rho**f13),9.384732821581892d0*(rho**4/(3.371549979772564d+9*rho**f23 + 1.719237205899231d+6*rho**f43 +389.6363641360097d0*rho**2 - 3.52275361d+8*(5.152090145859064d0*dsqrt(27069.58218509975d0 + 13.80345334341147d0*rho**f23)*rho**f13 - 18769.d0*dlog(dsqrt(1.d0 + 0.0005099248761589486d0*rho**f23) + 0.02258151625021997d0*rho**f13))*dlog(dsqrt(1.d0 + 0.0005099248761589486d0*rho**f23) + 0.02258151625021997d0*rho**f13)))**f16
  enddo
 enddo

end
