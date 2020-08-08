 BEGIN_PROVIDER[double precision, dirac_energy_x_LDA, (N_states) ]
 &BEGIN_PROVIDER[double precision, dirac_energy_c_LDA, (N_states) ]
 implicit none
 BEGIN_DOC
 ! exchange/correlation energy with the relativistic short range LDA functional
 END_DOC
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: e_c,v_c,e_x,v_x

 double precision :: vc_a,vc_b

 double precision, allocatable :: rho(:),tr_gamma_2(:),mu_of_r
 double precision :: f13, ckf
 allocate(rho(N_states),tr_gamma_2(N_states))
 dirac_energy_x_LDA = 0.d0
 dirac_energy_c_LDA = 0.d0
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i) 
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   tr_gamma_2(istate) = dirac_one_body_tr_dm_2_at_r(i,istate)
   if ( dirac_mu == "mu_erf" ) then
    call dirac_ex_LDA_sr(mu_erf,rho(istate),tr_gamma_2(istate),e_x,v_x)
   elseif( dirac_mu == "mu_of_r" ) then
    mu_of_r = mu_of_r_for_ints_vector(i)
    call dirac_ex_LDA_sr(mu_of_r,rho(istate),tr_gamma_2(istate),e_x,v_x)
   endif
   dirac_energy_x_LDA(istate) += weight * e_x
  !!dirac_energy_c_LDA(istate) += weight * e_c

   call ec_lda_sr(mu_erf,rho(istate)/2,rho(istate)/2,e_c,vc_a,vc_b) 
   open (10, file='correlation_lda_Z.dat',position ='append')
   write(10,*) rho(istate), e_c/rho(istate)


  enddo
 enddo
 END_PROVIDER 

 BEGIN_PROVIDER[complex*16, dirac_aos_vc_LDA_w, (n_points_final_grid,2*dirac_ao_num,N_states)]
 &BEGIN_PROVIDER[complex*16, dirac_aos_vx_LDA_w, (n_points_final_grid,2*dirac_ao_num,N_states)]
 implicit none
 BEGIN_DOC
 ! dirac_aos_vxc_LDA_w(j,i) = dirac_ao_i(r_j) * (v^x_alpha(r_j) + v^c_alpha(r_j)) * W(r_j)
 END_DOC
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: e_c,v_c,e_x,v_x
 double precision, allocatable :: rho(:)
 allocate(rho(N_states))
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i)
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   call dirac_ec_LDA_sr(mu_erf,rho(istate),e_c,v_c)
   call dirac_ex_LDA_sr(mu_erf,rho(istate),e_x,v_x)
   do j =1, 2*dirac_ao_num
    dirac_aos_vc_LDA_w(i,j,istate) = (1.d0,0.d0)* v_c * two_dirac_aos_in_r_array(j,i)*weight
    dirac_aos_vx_LDA_w(i,j,istate) = (1.d0,0.d0)* v_x * two_dirac_aos_in_r_array(j,i)*weight
   enddo
  enddo
 enddo
 END_PROVIDER 


 BEGIN_PROVIDER [complex*16, dirac_potential_x_ao_LDA,(2*dirac_ao_num,2*dirac_ao_num,N_states)]
 &BEGIN_PROVIDER [complex*16, dirac_potential_c_ao_LDA,(2*dirac_ao_num,2*dirac_ao_num,N_states)]
 implicit none
 BEGIN_DOC 
 ! short range exchange/correlation alpha/beta potentials with LDA functional on the AO basis
 END_DOC
 integer :: istate
 double precision :: wall_1,wall_2
!call wall_time(wall_1)
 do istate = 1, N_states 
  call zgemm('C','N',2*dirac_ao_num,2*dirac_ao_num,n_points_final_grid,(1.d0,0.d0),two_dirac_aos_in_r_array_transp,2*dirac_ao_num,dirac_aos_vc_LDA_w(1,1,istate),n_points_final_grid,(0.d0,0.d0),dirac_potential_c_ao_LDA(1,1,istate),2*dirac_ao_num)
  call zgemm('C','N',2*dirac_ao_num,2*dirac_ao_num,n_points_final_grid,(1.d0,0.d0),two_dirac_aos_in_r_array_transp,2*dirac_ao_num,dirac_aos_vx_LDA_w(1,1,istate),n_points_final_grid,(0.d0,0.d0),dirac_potential_x_ao_LDA(1,1,istate),2*dirac_ao_num)
 enddo
!call wall_time(wall_2)
!print*,'time to provide dirac_potential_x/c_ao_LDA = ',wall_2 - wall_1
 END_PROVIDER 


 BEGIN_PROVIDER[double precision, dirac_energy_x_PBE, (N_states) ]
 &BEGIN_PROVIDER[double precision, dirac_energy_c_PBE, (N_states) ]
 implicit none
 BEGIN_DOC
 ! exchange/correlation energy with the relativistic short range PBE functional
 END_DOC
 integer :: istate,i,j,k
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: e_c,v_c,e_x,v_x
 double precision, allocatable :: rho(:),tr_gamma_2(:),grad_rho(:,:),grad_rho_on_top(:,:),grad_rho_2(:),grad_rho_on_top_2(:),mu_of_r
 allocate(rho(N_states),tr_gamma_2(N_states),grad_rho(3,N_states),grad_rho_on_top(3,N_states),grad_rho_2(N_states),grad_rho_on_top_2(N_states))
 dirac_energy_x_PBE = 0.d0
 dirac_energy_c_PBE = 0.d0
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i) 
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   tr_gamma_2(istate) = dirac_one_body_tr_dm_2_at_r(i,istate)
   do k = 1,3 
    grad_rho_on_top(k,istate) = dirac_grad_dm_on_top_at_r(k,i,istate)
   enddo
   grad_rho_2(istate) = dirac_grad_dm_2_at_r(i,istate)
   grad_rho_on_top_2(istate) = dirac_grad_dm_on_top_2_at_r(i,istate)
   mu_of_r = mu_of_r_for_ints_vector(i)
   call dirac_ex_PBE_sr(mu_erf,rho(istate),tr_gamma_2(istate),grad_rho_on_top(1,istate),grad_rho_on_top(2,istate),grad_rho_on_top(3,istate),grad_rho_2(istate),grad_rho_on_top_2(istate),e_x,v_x)
   dirac_energy_x_PBE(istate) += weight * e_x
  !dirac_energy_c_PBE(istate) += weight * e_c
   write(14,*) weight * e_x, r(1),r(2),r(3)
  enddo
 enddo
 END_PROVIDER 

 BEGIN_PROVIDER[double precision, dirac_energy_x_RGGA, (N_states) ]
 &BEGIN_PROVIDER[double precision, dirac_energy_c_RGGA, (N_states) ]
 implicit none
 BEGIN_DOC
 ! exchange/correlation energy with the relativistic short range RGGA functional
 END_DOC
 integer :: istate,i,j,k
 double precision :: r(3)
 double precision :: mu,weight
 double precision :: e_c,v_c,e_x,v_x
 double precision, allocatable :: rho(:),tr_gamma_2(:),grad_rho(:,:),grad_rho_on_top(:,:),grad_rho_2(:),grad_rho_on_top_2(:),mu_of_r
 allocate(rho(N_states),tr_gamma_2(N_states),grad_rho(3,N_states),grad_rho_on_top(3,N_states),grad_rho_2(N_states),grad_rho_on_top_2(N_states))
 dirac_energy_x_RGGA = 0.d0
 dirac_energy_c_RGGA = 0.d0
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   weight=final_weight_at_r_vector(i) 
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   tr_gamma_2(istate) = dirac_one_body_tr_dm_2_at_r(i,istate)
   do k = 1,3 
    grad_rho_on_top(k,istate) = dirac_grad_dm_on_top_at_r(k,i,istate)
   enddo
   grad_rho_2(istate) = dirac_grad_dm_2_at_r(i,istate)
   grad_rho_on_top_2(istate) = dirac_grad_dm_on_top_2_at_r(i,istate)
   mu_of_r = mu_of_r_for_ints_vector(i)
   call dirac_ex_RGGA_sr(mu_erf,rho(istate),tr_gamma_2(istate),grad_rho_on_top(1,istate),grad_rho_on_top(2,istate),grad_rho_on_top(3,istate),grad_rho_2(istate),grad_rho_on_top_2(istate),e_x,v_x)
   dirac_energy_x_RGGA(istate) += weight * e_x
  !dirac_energy_c_RGGA(istate) += weight * e_c
  enddo
 enddo
 END_PROVIDER 
