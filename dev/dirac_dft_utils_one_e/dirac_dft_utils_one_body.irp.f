program dirac_dft_utils_one_body
  implicit none
  BEGIN_DOC
! TODO
  END_DOC
  integer :: i,j
  double precision :: r(3),dm(N_states),grad_dm(3,N_states) 
  complex*16 :: grad_dm_complex(3,N_states)
  double precision :: dm_a(N_states),dm_b(N_states),grad_dm_a(3,N_states),grad_dm_b(N_states),aos_array(ao_num),grad_aos_array(3,ao_num)
  r= 0.01d0

  call dirac_dm_dft_at_r(r,dm) 
  call density_and_grad_alpha_beta_and_all_aos_and_grad_aos_at_r(r,dm_a,dm_b, grad_dm_a, grad_dm_b, aos_array, grad_aos_array)
  call dirac_grad_dm_dft_at_r(r,grad_dm,grad_dm_complex)
   print*,'***************************'
   write(31,*) "density",dm_a(1),dm_b(1),dm(1)
   write(31,*) "x",grad_dm_a(1,1),grad_dm(1,1),grad_dm_complex(1,1)
   write(31,*) "y",grad_dm_a(2,1),grad_dm(2,1),grad_dm_complex(2,1)
   write(31,*) "z",grad_dm_a(3,1),grad_dm(3,1),grad_dm_complex(3,1)   
   end
