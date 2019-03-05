program dirac_density
 implicit none
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: f16,f13,f120,f23,f25,f43
 double precision :: c,ckf,tmp_c(N_states),tmp_kF(N_states)
 double precision :: e_x,v_x,ex_ab,vx_a,vx_b,kF(N_states)
 double precision :: dm(N_states),tr_dm(N_states),tr_gamma_2(N_states),rho(N_states),rho_lda(N_states)
 complex*16 :: large_aos_array(large_ao_num),large_aos_array_bis(large_ao_num)
 complex*16 :: small_aos_array(small_ao_num),small_aos_array_bis(small_ao_num),u_dotc_v
 f16 = 0.16666666666666666d0
 f13 = 0.3333333333333333d0
 f23 = 0.6666666666666667d0
 f43 = 1.3333333333333333d0
 ckf = 3.0936677262801355d0
 c = speed_of_light
 do istate = 1, N_states
 !do i = 1, n_points_final_grid
  do i = 1, 5000 
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i)
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   kF(istate) = ckf*(rho(istate)**f13)
  !call dirac_ex_LDA_sr(mu_erf,rho(istate),e_x,v_x)
  !print*,r(1),r(2),r(3), rho(istate)
   open (10, file='density_X',position ='append')
  !write(10,*)  r(1),r(2),r(3),rho(istate)
  !write(10,*)  rho(istate), 3.0936677262801355d0*(rho**f13),9.384732821581892d0*(rho**4/(3.371549979772564d+9*rho**f23 + 1.719237205899231d+6*rho**f43 +389.6363641360097d0*rho**2 - 3.52275361d+8*(5.152090145859064d0*dsqrt(27069.58218509975d0 + 13.80345334341147d0*rho**f23)*rho**f13 - 18769.d0*dlog(dsqrt(1.d0 + 0.0005099248761589486d0*rho**f23) + 0.02258151625021997d0*rho**f13))*dlog(dsqrt(1.d0 + 0.0005099248761589486d0*rho**f23) + 0.02258151625021997d0*rho**f13)))**f16
   tr_gamma_2(istate) = dirac_one_body_tr_dm_at_r(i,istate)
   rho_lda(istate) = dsqrt(2.d0*tr_gamma_2(istate))
   tmp_kF(istate) = ckf*(rho_lda(istate)**f13)
   tmp_c(istate) = c/tmp_kF(istate)
   do j = 1, 10
    tmp_kF = 4.375106855981304d0*(rho_lda*dsqrt(-1.d0/(-4.d0 - 9.d0*tmp_c**2 - 9.d0*tmp_c**4 + 9.d0*tmp_c**4*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)* (2.d0*dsqrt(1.d0 + tmp_c**2) - tmp_c**2*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)))))**f13
    tmp_c = c/tmp_kF
   write(11,*)i,j,kF(istate),tr_gamma_2(istate),tmp_kF(istate)
   enddo
   write(10,*) kF(istate),tr_gamma_2(istate),tmp_kF(istate)
  enddo
 enddo

end
