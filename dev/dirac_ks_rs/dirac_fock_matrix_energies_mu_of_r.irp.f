 BEGIN_PROVIDER [ complex*16, dirac_HF_two_electron_c_ex_int_mu_of_r, (2*dirac_ao_num, 2*dirac_ao_num) ]
  use map_module
  implicit none
  BEGIN_DOC
  !Array of the bi-electronic Fock matrix for the Coulomb interaction
  ! in Dirac AO basis set
  END_DOC
  PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map
  integer                        :: i,j,k,l,k1 
  double precision               :: integral
  complex*16, allocatable        :: dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(:,:)
  integer*8                      :: i8
  integer                        :: ii(4), jj(4), kk(4), ll(4), k2, k_test
  integer(cache_map_size_kind)   :: n_elements_max, n_elements
  integer(key_kind), allocatable :: keys(:)
  double precision, allocatable  :: values(:)
  dirac_HF_two_electron_c_ex_int_mu_of_r = (0.d0,0.d0)
!!$OMP PARALLEL DEFAULT(NONE)                                      &
!!$OMP PRIVATE(i,j,l,k1,k,integral,ii,jj,kk,ll,i8,keys,values,n_elements_max, &
!!$OMP  n_elements,dirac_HF_two_electron_c_ex_int_mu_of_r_tmp)&
!!$OMP SHARED(dirac_ao_num,dirac_SCF_density_matrix_ao,&
!!$OMP dirac_ao_ints_erf_mu_of_r_map, dirac_HF_two_electron_c_ex_int_mu_of_r) 
  k_test = 0
  call get_cache_map_n_elements_max(dirac_ao_ints_erf_mu_of_r_map,n_elements_max)
  allocate(keys(n_elements_max), values(n_elements_max))
  allocate(dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(2*dirac_ao_num,2*dirac_ao_num))
  dirac_HF_two_electron_c_ex_int_mu_of_r_tmp = (0.d0,0.d0) 
!!$OMP DO SCHEDULE(dynamic,64)
!!DIR$ NOVECTOR
  do i8=0_8,dirac_ao_ints_erf_mu_of_r_map%map_size
   n_elements = n_elements_max
   call get_cache_map(dirac_ao_ints_erf_mu_of_r_map,i8,keys,values,n_elements)
   do k1=1,n_elements
    if (k_test == keys(k1)) then
     cycle
    endif 
    k_test = keys(k1)
    call index_reverse_two_e_no_sym(kk,ii,ll,jj,dirac_ao_num,keys(k1))
    do k2=1,4
     if (kk(k2)==0) then
      cycle
     endif
      i = ii(k2) ! electron 1
      j = jj(k2) ! electron 1
      k = kk(k2) ! electron 2
      l = ll(k2) ! electron 2
      ! values(k1) = (ij|kl) <=> <ik|jl>
      integral = values (k1)
     
       double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
      !      1 2 1 2                                                     
      write(34,*),i,k,j,l,integral,get_dirac_ao_bielec_integral_erf_mu_of_r(i,k,j,l,dirac_ao_ints_erf_mu_of_r_map)
     
      if ((i .le. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
          (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num)) then
       !L_alpha L_alpha .or. S_alpha S_alpha
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,1),dirac_inverse_list(l,1)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,1),dirac_inverse_list(k,1))) * integral
       !L_beta L_beta .or. S_beta S_beta
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,2),dirac_inverse_list(l,2)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,2),dirac_inverse_list(k,2))) * integral 
       !L_beta L_alpha .or. S_beta S_alpha
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,2),dirac_inverse_list(l,1)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,2),dirac_inverse_list(k,1))) * integral
       !L_alpha L_beta .or. S_alpha S_beta
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,1),dirac_inverse_list(l,2)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,1),dirac_inverse_list(k,2))) * integral
      elseif((i .le. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or. &
             (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num))then
       !L_alpha S_alpha .or S_alpha L_alpha
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,1),dirac_inverse_list(l,1)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,1),dirac_inverse_list(k,1))) * integral
       !L_beta S_beta .or. S_beta L_beta
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,2),dirac_inverse_list(l,2)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,2),dirac_inverse_list(k,2))) * integral
       !L_beta S_alpha .or. S_beta L_alpha
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,2),dirac_inverse_list(l,1)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,2),dirac_inverse_list(k,1))) * integral
       !L_alpha S_beta .or. S_alpha L_beta
       dirac_HF_two_electron_c_ex_int_mu_of_r_tmp(dirac_inverse_list(i,1),dirac_inverse_list(l,2)) -= (dirac_SCF_density_matrix_ao(dirac_inverse_list(j,1),dirac_inverse_list(k,2))) * integral
      endif
    enddo
   enddo
  enddo
!!$OMP END DO NOWAIT
!!$OMP CRITICAL
  dirac_HF_two_electron_c_ex_int_mu_of_r += dirac_HF_two_electron_c_ex_int_mu_of_r_tmp
!!$OMP END CRITICAL
  deallocate(keys,values,dirac_HF_two_electron_c_ex_int_mu_of_r_tmp)
!!$OMP END PARALLEL
 END_PROVIDER

 BEGIN_PROVIDER [ complex*16, dirac_HF_two_electron_c_ex_energy_mu_of_r_complex]
 &BEGIN_PROVIDER [ double precision, dirac_HF_two_electron_c_ex_energy_mu_of_r] 
  implicit none
  BEGIN_DOC
  !Two-electrons energy of the Coulomb ee interaction
  !The energy is supposed to be a real, thus we check for its complex part to be
  ! a VERY small artifact and take only its real part
  END_DOC
  integer :: i,j
  dirac_HF_two_electron_c_ex_energy_mu_of_r_complex = (0.d0,0.d0)
  do j=1, 2*dirac_ao_num
   do i=1, 2*dirac_ao_num
    dirac_HF_two_electron_c_ex_energy_mu_of_r_complex += 0.5d0* (dirac_HF_two_electron_c_ex_int_mu_of_r(i,j)) * dirac_SCF_density_matrix_ao(j,i)
   enddo
  enddo
  dirac_HF_two_electron_c_ex_energy_mu_of_r = real(dirac_HF_two_electron_c_ex_energy_mu_of_r_complex)
  if (aimag(dirac_HF_two_electron_c_ex_energy_mu_of_r_complex) .gt. 1.d-10) then
  print*, 'Warning! The energy is not real'
  print*, 'dirac_HF_two_electron_c_ex_energy_mu_of_r_complex =', dirac_HF_two_electron_c_ex_energy_mu_of_r_complex
  STOP
  endif
 END_PROVIDER


