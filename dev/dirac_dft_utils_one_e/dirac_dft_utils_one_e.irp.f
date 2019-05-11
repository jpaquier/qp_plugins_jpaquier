program dirac_dft_utils_one_e
  implicit none
  BEGIN_DOC
! TODO
  END_DOC
  integer :: i,j
  double precision :: r(3),tr_dm_2(N_states),dm(N_states),grad_dm(3,N_states),grad_dm_2(N_states),grad_dm_abs(N_states),grad_dm_on_top(3,N_states),grad_dm_on_top_2(N_states),grad_dm_on_top_abs(N_states)
  double precision :: mu_of_r
  complex*16 :: grad_dm_complex(3,N_states),grad_tr_dm_2(3,N_states)
  double precision :: dm_a(N_states),dm_b(N_states),grad_dm_a(3,N_states),grad_dm_b(3,N_states),aos_array(ao_num),grad_aos_array(3,ao_num)
  r = 0.d0
  !r(1) = 0.3372762d0
  !do i = 1, n_points_final_grid
  ! r(1) = final_grid_points(1,i)
  ! r(2) = final_grid_points(2,i)
  ! r(3) = final_grid_points(3,i)
  do i = 1,10000
   call dirac_tr_dm_2_dft_at_r(r,tr_dm_2)
   call dirac_dm_dft_at_r(r,dm)
  !call density_and_grad_alpha_beta_and_all_aos_and_grad_aos_at_r(r,dm_a,dm_b, grad_dm_a, grad_dm_b, aos_array, grad_aos_array)
   call dirac_grad_dm_dft_at_r(r,grad_dm,grad_dm_2,grad_dm_abs)
   call dirac_grad_dm_on_top_dft_at_r(r,grad_dm_on_top,grad_dm_on_top_2,grad_dm_on_top_abs)
   call dirac_mu_of_r (r,mu_of_r)
   call dirac_grad_tr_dm_2_dft_at_r(r,grad_tr_dm_2)
  !open (31, file='mu_of_r_compared_Z')
  !write(31,*),'***************************'
  !write(31,*), i, r(1),r(2),r(3)
  !write(31,*) "density",dm_a(1)+dm_b(1),dm(1), 3.0936677262801355d0*(dm(1)**0.3333333333333333d0)
  !write(31,*) "x",grad_dm_a(1,1)+grad_dm_b(1,1),grad_dm(1,1)
  !write(31,*) "y",grad_dm_a(2,1)+grad_dm_b(2,1),grad_dm(2,1)
  !write(31,*) "z",grad_dm_a(3,1)+grad_dm_b(3,1),grad_dm(3,1)
  !Write(31,*) "mu_of_r_for_ints_vector =",mu_of_r
   open (32, file='mu_of_r_Z')
   write(32,*),'***************************'
   write(32,*), i, r(1),r(2),r(3)
   write(32,*) "density =",dm(1), dsqrt(2*tr_dm_2(1)), "kF =",3.0936677262801355d0*(dm(1)**0.3333333333333333d0),3.0936677262801355d0*( dsqrt(2*tr_dm_2(1))**0.3333333333333333d0)
   write(32,*) "grad_abs =", grad_dm_abs,grad_dm_on_top_abs
   write(32,*) "grad_x =",grad_dm(1,1),grad_dm_on_top(1,1)
   write(32,*) "grad_y =",grad_dm(2,1),grad_dm_on_top(2,1)
   write(32,*) "grad_z =",grad_dm(3,1),grad_dm_on_top(3,1)
   Write(32,*) "mu_of_r_for_ints_vector =",mu_of_r
   open (28, file='density_of_r_Z.dat')
   write(28,*) r(1),dm(1),3.0936677262801355d0*(dm(1)**0.3333333333333333d0)
   open (29, file='density_grad_of_r_Z.dat')
   write(29,*) r(1),grad_dm_abs
   open (30, file='mu_of_r_Z.dat')
   write(30,*) r(1), mu_of_r
   open (33, file='ecart.dat')
   write(33,*) r(1), 100*dsqrt((grad_dm_abs- grad_dm_on_top_abs)**2)/grad_dm_abs 
   r(1)+=0.000005d0
  !r(1)+=0.0005d0
  !r(1)+=0.001d0
  enddo
   end

