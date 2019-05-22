program dirac_density
 implicit none
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: f16,f13,f120,f23,f25,f43
 double precision :: c,ckf,tmp_c(N_states),tmp_kF(N_states),kF_bis(N_states)
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
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i)
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   kF(istate) = ckf*(rho(istate)**f13)
   tr_gamma_2(istate) = dirac_one_body_tr_dm_2_at_r(i,istate)
   rho_lda(istate) = dsqrt(2.d0*tr_gamma_2(istate))
   kF_bis(istate) = ckf*(rho_lda(istate)**f13)
   tmp_c(istate) = c/kF_bis(istate)
   if (tr_gamma_2(istate) .gt. 1d-5) then
    do j = 1, 4
     tmp_kF = 4.375106855981304d0*(rho_lda*dsqrt(-1.d0/(-4.d0 - 9.d0*tmp_c**2 - 9.d0*tmp_c**4 + 9.d0*tmp_c**4*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)* (2.d0*dsqrt(1.d0 + tmp_c**2) - tmp_c**2*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)))))**f13
     tmp_c = c/tmp_kF
    enddo
   else
    tmp_kF = kF_bis
   endif
   open (10, file='density_Z')
   write(10,*) r(1),r(2),r(3), rho(istate), kF(istate),tr_gamma_2(istate),kF_bis(istate),tmp_kF(istate)
  enddo
 enddo

end
