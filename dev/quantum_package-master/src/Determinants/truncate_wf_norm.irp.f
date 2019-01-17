program s2_eig_restart
 implicit none
 read_wf = .True.
 call routine
end
subroutine routine
 implicit none
 integer :: ndet_max
 integer(bit_kind), allocatable :: psi_det_tmp(:,:,:)
 double precision, allocatable :: psi_coef_tmp(:,:)
 
 integer :: i,j
 double precision :: accu(N_states)
 accu = 0.d0
 ndet_max = 1
 do i = 1, N_det
  accu(1) += psi_coef_sorted(i,1) **2
  if(accu(1).gt.norm_truncated)then
   exit
  endif
  ndet_max += 1
 enddo
 allocate(psi_det_tmp(N_int,2,ndet_max),psi_coef_tmp(ndet_max, N_states))
 print*,'accu,norm = ',accu(1),norm_truncated
 print*,'det_max = ',ndet_max
 accu = 0.d0
 do i = 1, ndet_max
  do j = 1, N_int
   psi_det_tmp(j,1,i) = psi_det_sorted(j,1,i)
   psi_det_tmp(j,2,i) = psi_det_sorted(j,2,i)
  enddo
  do j = 1, N_states
   psi_coef_tmp(i,j) = psi_coef_sorted(i,j)
   accu(j) += psi_coef_tmp(i,j) **2
  enddo
 enddo
 do j = 1, N_states
  accu(j) = 1.d0/dsqrt(accu(j))
 enddo
 do j = 1, N_states
  do i = 1, ndet_max
   psi_coef_tmp(i,j) = psi_coef_tmp(i,j) * accu(j)
  enddo
 enddo

 call save_wavefunction_general(ndet_max,N_states,psi_det_tmp,N_det_max,psi_coef_tmp)
 
end
