program dirac_ks_rs
 implicit none
 integer :: istate,i,j,k
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: f16,f13,f120,f23,f25,f43
 double precision :: c,ckf,tmp_c(N_states),tmp_kF(N_states),kF_bis(N_states),kF_bis_moyen(N_states),n_moyen(N_states)
 double precision :: e_x,v_x,ex_ab,vx_a,vx_b,kF(N_states),kF_moyen(N_states)
 double precision :: dm(N_states),tr_dm(N_states),tr_gamma_2(N_states),rho(N_states),rho_lda(N_states)
 complex*16 :: large_aos_array(large_ao_num),large_aos_array_bis(large_ao_num)
 complex*16 :: small_aos_array(small_ao_num),small_aos_array_bis(small_ao_num),u_dotc_v
!f16 = 0.16666666666666666d0
!f13 = 0.3333333333333333d0
!f23 = 0.6666666666666667d0
!f43 = 1.3333333333333333d0
!ckf = 3.0936677262801355d0
!c = speed_of_light
!kF_moyen = 0.d0
!kF_bis_moyen = 0.d0
!n_moyen = 0.d0
!do istate = 1, N_states
! do i = 1, n_points_final_grid
!  r(1) = final_grid_points(1,i)
!  r(2) = final_grid_points(2,i)
!  r(3) = final_grid_points(3,i)
!  weight=final_weight_at_r_vector(i)
!  rho(istate) = dirac_one_body_dm_at_r(i,istate)
!  kF(istate) = ckf*(rho(istate)**f13)
!  tr_gamma_2(istate) = dirac_one_body_tr_dm_2_at_r(i,istate)
!  call dirac_ex_LDA_sr(mu_erf,rho(istate),tr_gamma_2(istate),e_x,v_x)
!  rho_lda(istate) = dsqrt(2.d0*tr_gamma_2(istate))
!  kF_bis(istate) = ckf*(rho_lda(istate)**f13)
!  tmp_c(istate) = c/kF_bis(istate)
!  if (tr_gamma_2(istate) .gt. 1d-5) then
!   do j = 1, 4
!    tmp_kF = 4.375106855981304d0*(rho_lda*dsqrt(-1.d0/(-4.d0 - 9.d0*tmp_c**2 - 9.d0*tmp_c**4 + 9.d0*tmp_c**4*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)* (2.d0*dsqrt(1.d0 + tmp_c**2) - tmp_c**2*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)))))**f13
!    tmp_c = c/tmp_kF
!   enddo
!  else
!   tmp_kF = kF_bis
!  endif
!  n_moyen (istate) += weight*rho(istate)
!  kF_moyen(istate) += weight*0.0337737d0* kF(istate)**4  
!  kF_bis_moyen(istate) += weight*0.0337737d0* tmp_kF(istate)**4
!  
!  open (10, file='exchange_energy_density')
!  write(10,*) r(1),r(2),r(3), tmp_kF(istate),e_x
! enddo
! open (11, file='kF_moyen')
! write(11,*) "elec_num =", n_moyen(istate)
! write(11,*) "kF_moyen =", kF_moyen(istate), kF_moyen(istate)/n_moyen(istate)
! write(11,*) "kF_moyen_bis=", kF_bis_moyen(istate), kF_bis_moyen(istate)/n_moyen(istate)
!enddo

 do j = 1,2*dirac_ao_num
  do i = 1,2*dirac_ao_num
   write(25,*), i,j,dirac_ao_bi_elec_C_Exchange_integral(i,j)
   write(35,*), i,j,dirac_HF_two_electron_c_ex_int_mu_of_r(i,j)
  enddo
 enddo
 
end
