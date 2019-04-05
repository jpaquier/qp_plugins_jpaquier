program dirac_two_e_ints

  use map_module
  implicit none
  BEGIN_DOC
  ! Tests
  END_DOC
  integer                        :: i,j,k,l,k1 
  integer                        :: r,s,p,q
  double precision               :: dirac_ao_bielec_integral, local_threshold
  double precision               :: integral
  double precision               :: integral_erf
  complex*16, allocatable        :: dirac_ao_bi_elec_C_integral_tmp(:,:)
  integer(omp_lock_kind)         :: lck(dirac_ao_num)
  integer*8                      :: i8
  integer                        :: ii(8), jj(8), kk(8), ll(8), k2
  integer(cache_map_size_kind)   :: n_elements_max, n_elements
  integer(cache_map_size_kind)   :: n_elements_max_erf, n_elements_erf
  integer(key_kind), allocatable :: keys(:)
  integer(key_kind), allocatable :: keys_erf(:)
  double precision, allocatable  :: values(:)
  double precision, allocatable  :: values_erf(:)
  PROVIDE dirac_ao_bielec_integrals_erf_in_map
   call get_cache_map_n_elements_max(dirac_ao_integrals_erf_map,n_elements_max_erf)
   allocate(keys_erf(n_elements_max_erf), values_erf(n_elements_max_erf))
   allocate(dirac_ao_bi_elec_C_integral_tmp(2*dirac_ao_num,2*dirac_ao_num))
   do i8=0_8,dirac_ao_integrals_erf_map%map_size
    n_elements_erf = n_elements_max_erf
    call get_cache_map(dirac_ao_integrals_erf_map,i8,keys_erf,values_erf,n_elements_erf)
    do k1=1,n_elements_erf
     call two_e_integrals_index_reverse(kk,ii,ll,jj,keys_erf(k1))
     do k2=1,8
      if (kk(k2)==0) then
       cycle
      endif
      i = ii(k2) ! electron 1
      j = jj(k2) ! electron 1
      k = kk(k2) ! electron 2
      l = ll(k2) ! electron 2
      ! values_erf(k1) = (ij|kl) <=> <ik|jl>
      integral_erf = values_erf (k1)
     print*, i,j,k,l,integral_erf
    enddo
   enddo 
  enddo

 end
