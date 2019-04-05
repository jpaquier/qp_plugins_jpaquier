 subroutine compute_dirac_ao_ints_erf_mu_of_r_jl(j,l,n_ints,buffer_i,buffer_value)
 implicit none
 use map_module
 BEGIN_DOC
 !  Parallel client for AO ints
 END_DOC
 integer, intent(in)            :: j,l
 integer,intent(out)            :: n_ints
 integer(key_kind),intent(out)  :: buffer_i(dirac_ao_num*dirac_ao_num)
 real(integral_kind),intent(out) :: buffer_value(dirac_ao_num*dirac_ao_num)
 integer                        :: i,k
 double precision               :: cpu_1,cpu_2,wall_1, wall_2
 double precision               :: integral, wall_0
 double precision               :: thr
 integer                        :: kk, m, j1, i1
 thr = dirac_ao_integrals_threshold
 n_ints = 0
 double precision :: ints_matrix(dirac_ao_num,dirac_ao_num)
 call give_all_dirac_erf_mu_of_r_kl(j,l,ints_matrix)
 j1 = j+ishft(l*l-l,-1)
 do k = 1, dirac_ao_num           ! r1
  i1 = ishft(k*k-k,-1)
  do i = 1, dirac_ao_num
   i1 += 1
   integral = ints_matrix(k,i)  ! i,k : r1    j,l : r2
   if (abs(integral) < thr) then
    cycle
   endif
   n_ints += 1
  !!DIR$ FORCEINLINE
   call index_two_e_no_sym(i,j,k,l,dirac_ao_num,buffer_i(n_ints))
   buffer_value(n_ints) = integral
  enddo
 enddo
 end
 
