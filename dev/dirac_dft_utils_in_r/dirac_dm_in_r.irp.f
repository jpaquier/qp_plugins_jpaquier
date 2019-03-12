 complex*16 function u_dotc_v(u,v,sze)
  implicit none
  BEGIN_DOC
  ! Compute <u|v> in complex formalism
  END_DOC
  integer, intent(in)      :: sze
  complex*16, intent(in)   :: u(sze),v(sze)
  complex*16, external     :: zdotc
  u_dotc_v = zdotc(sze,u,1,v,1)
 end


 subroutine dirac_dm_dft_at_r(r,dm)
 implicit none
 BEGIN_DOC
 ! input: r(1) ==> r(1) = x, r(2) = y, r(3) = z
 ! output : dm = density evaluated at r(3)
 END_DOC
 double precision, intent(in) :: r(3)
 complex*16 :: dm_complex(N_states)
 double precision :: dm_im
 double precision, intent(out) :: dm(N_states)
 integer :: istate
 complex*16  :: dirac_aos_array(2*dirac_ao_num),dirac_aos_array_bis(2*dirac_ao_num),u_dotc_v
 call give_all_dirac_aos_at_r(r,dirac_aos_array)
 do istate = 1, N_states
  dirac_aos_array_bis = dirac_aos_array
  call zgemv('N',2*dirac_ao_num,2*dirac_ao_num,(1.d0,0.d0),dirac_one_body_dm_ao_for_dft(1,1,istate),2*dirac_ao_num,dirac_aos_array,1,(0.d0,0.d0),dirac_aos_array_bis,1)
  dm_complex(istate) = u_dotc_v(dirac_aos_array,dirac_aos_array_bis,2*dirac_ao_num)
  dm = real(dm_complex)
  dm_im = aimag(dm_complex(1))/dm(1)
  if (dm(1) .gt. 1.d0) then
   if (dm_im .gt. 1.d-10) then
    print*, 'Warning! The electronic density is not real'
    print*, 'dm_complex =',dm_complex
    stop
   endif
  else
   if (aimag(dm_complex(1)) .gt. 1.d-10) then
    print*, 'Warning! The electronic density is not real'
    print*, 'dm_complex =',dm_complex
    stop
   endif
  endif
 enddo
 end

 BEGIN_PROVIDER [double precision, dirac_one_body_dm_at_r, (n_points_final_grid,N_states) ]
 implicit none
 BEGIN_DOC
 ! dirac_one_body_dm_at_r(i,istate) = n(r_i,istate)
 ! where r_i is the ith point of the grid and istate is the state number
 END_DOC
 integer :: i,istate
 double precision :: r(3)
 double precision, allocatable :: dm(:)
 allocate(dm(N_states))
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   call dirac_dm_dft_at_r(r,dm)
   dirac_one_body_dm_at_r(i,istate) = dm(istate)
  enddo
 enddo
 END_PROVIDER 

 subroutine dirac_tr_dm_2_dft_at_r(r,tr_dm_2)
 implicit none
 BEGIN_DOC
 ! input: r(1) ==> r(1) = x, r(2) = y, r(3) = z
 ! output : tr_dm_2 = trace of the squared density matrix evaluated at r(3)
 END_DOC
 integer :: i,j,istate
 double precision, intent(in) :: r(3)
 complex*16 :: tr_dm_2_complex(N_states)
 complex*16, Allocatable :: tr_dm_2_complex_tmp_part1(:),tr_dm_2_complex_tmp_part2(:)
 double precision :: tr_dm_2_im
 double precision, intent(out) :: tr_dm_2(N_states)
 complex*16 :: large_aos_array(large_ao_num),large_aos_array_bis(large_ao_num)
 complex*16 :: small_aos_array(small_ao_num),small_aos_array_bis(small_ao_num),u_dotc_v
 complex*16, Allocatable :: LL_one_body_dm_ao_for_dft_tmp(:,:,:),SS_one_body_dm_ao_for_dft_tmp(:,:,:)
 complex*16, Allocatable :: LS_one_body_dm_ao_for_dft_tmp(:,:,:),SL_one_body_dm_ao_for_dft_tmp(:,:,:)
 Allocate(tr_dm_2_complex_tmp_part1(N_states),tr_dm_2_complex_tmp_part2(N_states),&  
        LL_one_body_dm_ao_for_dft_tmp(large_ao_num,large_ao_num,N_states),SS_one_body_dm_ao_for_dft_tmp(small_ao_num,small_ao_num,N_states),    & 
        LS_one_body_dm_ao_for_dft_tmp(large_ao_num,small_ao_num,N_states),SL_one_body_dm_ao_for_dft_tmp(small_ao_num,large_ao_num,N_states))
 tr_dm_2_complex = (0.d0,0.d0) 
 call give_all_large_aos_at_r(r,large_aos_array)
 call give_all_small_aos_at_r(r,small_aos_array)
 do istate = 1, N_states
  large_aos_array_bis = large_aos_array
  small_aos_array_bis = small_aos_array
  !L_alpha L_alpha* x L_alpha L_alpha*
  do j = 1, large_ao_num
   do i = 1, large_ao_num
    LL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i,j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,large_ao_num,(1.d0,0.d0),LL_one_body_dm_ao_for_dft_tmp(1,1,istate),large_ao_num,large_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo  
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, large_ao_num
    LL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i,j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,large_ao_num,(1.d0,0.d0),LL_one_body_dm_ao_for_dft_tmp(1,1,istate),large_ao_num,large_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part2(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo  
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num) 
  tr_dm_2_complex += tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !L_beta L_beta* x L_beta L_beta*
  do j = 1, large_ao_num
   do i = 1, large_ao_num
    LL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(large_ao_num+i,large_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,large_ao_num,(1.d0,0.d0),LL_one_body_dm_ao_for_dft_tmp(1,1,istate),large_ao_num,large_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo  
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, large_ao_num
    LL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(large_ao_num+i,large_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,large_ao_num,(1.d0,0.d0),LL_one_body_dm_ao_for_dft_tmp(1,1,istate),large_ao_num,large_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part2(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo  
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num) 
  tr_dm_2_complex += tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !S_alpha S_alpha* x S_alpha S_alpha*
  do j = 1, small_ao_num
   do i = 1, small_ao_num
    SS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+i,2*large_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, small_ao_num 
     small_aos_array_bis(i) += SS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,small_ao_num,(1.d0,0.d0),SS_one_body_dm_ao_for_dft_tmp(1,1,istate),small_ao_num,small_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part1(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num)
  do j = 1, small_ao_num
   do i = 1, small_ao_num
    SS_one_body_dm_ao_for_dft_tmp(i,j,istate) =dirac_one_body_dm_ao_for_dft(2*large_ao_num+i,2*large_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, small_ao_num 
     small_aos_array_bis(i) += SS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,small_ao_num,(1.d0,0.d0),SS_one_body_dm_ao_for_dft_tmp(1,1,istate),small_ao_num,small_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num)
  tr_dm_2_complex += tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !S_beta S_beta* x S_beta S_beta*
  do j = 1, small_ao_num
   do i = 1, small_ao_num
    SS_one_body_dm_ao_for_dft_tmp(i,j,istate) =dirac_one_body_dm_ao_for_dft(2*large_ao_num+small_ao_num+i,2*large_ao_num+small_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, small_ao_num 
     small_aos_array_bis(i) += SS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,small_ao_num,(1.d0,0.d0),SS_one_body_dm_ao_for_dft_tmp(1,1,istate),small_ao_num,small_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part1(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num)
  do j = 1, small_ao_num
   do i = 1, small_ao_num
    SS_one_body_dm_ao_for_dft_tmp(i,j,istate) =dirac_one_body_dm_ao_for_dft(2*large_ao_num+small_ao_num+i,2*large_ao_num+small_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, small_ao_num 
     small_aos_array_bis(i) += SS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,small_ao_num,(1.d0,0.d0),SS_one_body_dm_ao_for_dft_tmp(1,1,istate),small_ao_num,small_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num)
  tr_dm_2_complex += tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !L_alpha L_beta* x L_beta L_alpha*
  do j = 1, large_ao_num
   do i = 1, large_ao_num
    LL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i,large_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,large_ao_num,(1.d0,0.d0),LL_one_body_dm_ao_for_dft_tmp(1,1,istate),large_ao_num,large_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo 
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, large_ao_num
    LL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(large_ao_num+i,j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,large_ao_num,(1.d0,0.d0),LL_one_body_dm_ao_for_dft_tmp(1,1,istate),large_ao_num,large_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part2(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo 
  !tr_d2m_complex_tmp_part(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num) 
  tr_dm_2_complex += 2*tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !S_alpha S_beta* x S_beta S_alpha*
  do j = 1, small_ao_num
   do i = 1, small_ao_num
    SS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+i,2*large_ao_num+small_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, small_ao_num 
     small_aos_array_bis(i) += SS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,small_ao_num,(1.d0,0.d0),SS_one_body_dm_ao_for_dft_tmp(1,1,istate),small_ao_num,small_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part1(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num)
  do j = 1, small_ao_num
   do i = 1, small_ao_num
    SS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+small_ao_num+i,2*large_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, small_ao_num 
     small_aos_array_bis(i) += SS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,small_ao_num,(1.d0,0.d0),SS_one_body_dm_ao_for_dft_tmp(1,1,istate),small_ao_num,small_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num) 
  tr_dm_2_complex += 2*tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !L_alpha S_alpha* x S_alpha L_alpha*
  do j = 1, small_ao_num
   do i = 1, large_ao_num
    LS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i,2*large_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,small_ao_num,(1.d0,0.d0),LS_one_body_dm_ao_for_dft_tmp(1,1,istate),size(LS_one_body_dm_ao_for_dft_tmp,1),small_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, small_ao_num
    SL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+i,j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, small_ao_num
     small_aos_array_bis(i) += SL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,large_ao_num,(1.d0,0.d0),SL_one_body_dm_ao_for_dft_tmp(1,1,istate),size(SL_one_body_dm_ao_for_dft_tmp,1),large_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num)
  tr_dm_2_complex += 2*tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !L_beta S_beta* x S_beta L_beta*
  do j = 1, small_ao_num
   do i = 1, large_ao_num
    LS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i+large_ao_num,2*large_ao_num+small_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,small_ao_num,(1.d0,0.d0),LS_one_body_dm_ao_for_dft_tmp(1,1,istate),size(LS_one_body_dm_ao_for_dft_tmp,1),small_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, small_ao_num
    SL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+small_ao_num+i,large_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, small_ao_num
     small_aos_array_bis(i) += SL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,large_ao_num,(1.d0,0.d0),SL_one_body_dm_ao_for_dft_tmp(1,1,istate),size(SL_one_body_dm_ao_for_dft_tmp,1),large_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num) 
  tr_dm_2_complex += 2*tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !L_alpha S_beta* x S_beta L_alpha*
  do j = 1, small_ao_num
   do i = 1, large_ao_num
    LS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i,2*large_ao_num+small_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,small_ao_num,(1.d0,0.d0),LS_one_body_dm_ao_for_dft_tmp(1,1,istate),size(LS_one_body_dm_ao_for_dft_tmp,1),small_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, small_ao_num
    SL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+small_ao_num+i,j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, small_ao_num
     small_aos_array_bis(i) += SL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,large_ao_num,(1.d0,0.d0),SL_one_body_dm_ao_for_dft_tmp(1,1,istate),size(SL_one_body_dm_ao_for_dft_tmp,1),large_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num) 
  tr_dm_2_complex += 2*tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  !L_beta S_alpha* x S_alpha L_beta*
  do j = 1, small_ao_num
   do i = 1, large_ao_num
    LS_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(i+large_ao_num,2*large_ao_num+j,istate)
   enddo
  enddo
   large_aos_array_bis = (0.d0,0.d0)
   do j = 1, small_ao_num
    do i  = 1, large_ao_num 
     large_aos_array_bis(i) += LS_one_body_dm_ao_for_dft_tmp(i,j,istate)*small_aos_array(j)
    enddo
   enddo
  !call zgemv('N',large_ao_num,small_ao_num,(1.d0,0.d0),LS_one_body_dm_ao_for_dft_tmp(1,1,istate),size(LS_one_body_dm_ao_for_dft_tmp,1),small_aos_array,1,(0.d0,0.d0),large_aos_array_bis,1)
   tr_dm_2_complex_tmp_part1 =(0.d0,0.d0)
   do i  = 1, large_ao_num
    tr_dm_2_complex_tmp_part1(istate) += large_aos_array(i)*large_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part1(istate) = u_dotc_v(large_aos_array,large_aos_array_bis,large_ao_num)
  do j = 1, large_ao_num
   do i = 1, small_ao_num
    SL_one_body_dm_ao_for_dft_tmp(i,j,istate) = dirac_one_body_dm_ao_for_dft(2*large_ao_num+i,large_ao_num+j,istate)
   enddo
  enddo
   small_aos_array_bis = (0.d0,0.d0)
   do j = 1, large_ao_num
    do i  = 1, small_ao_num
     small_aos_array_bis(i) += SL_one_body_dm_ao_for_dft_tmp(i,j,istate)*large_aos_array(j)
    enddo
   enddo
  !call zgemv('N',small_ao_num,large_ao_num,(1.d0,0.d0),SL_one_body_dm_ao_for_dft_tmp(1,1,istate),size(SL_one_body_dm_ao_for_dft_tmp,1),large_aos_array,1,(0.d0,0.d0),small_aos_array_bis,1)
   tr_dm_2_complex_tmp_part2 =(0.d0,0.d0)
   do i  = 1, small_ao_num
    tr_dm_2_complex_tmp_part2(istate) += small_aos_array(i)*small_aos_array_bis(i)
   enddo
  !tr_dm_2_complex_tmp_part2(istate) = u_dotc_v(small_aos_array,small_aos_array_bis,small_ao_num) 
  tr_dm_2_complex += 2*tr_dm_2_complex_tmp_part1*tr_dm_2_complex_tmp_part2
  tr_dm_2 = real(tr_dm_2_complex)
  tr_dm_2_im = aimag(tr_dm_2_complex(1))/tr_dm_2(1)
  if (tr_dm_2(1) .gt. 1.d0) then
   if (tr_dm_2_im .gt. 1.d-10) then
    print*, 'Warning! The on-top pair density is not real'
    print*, 'tr_dm_2_complex =',tr_dm_2_complex
    stop
   endif
  else
   if (aimag(tr_dm_2_complex(1)) .gt. 1.d-10) then
    print*, 'Warning! The on-top pair density is not real'
    print*, 'tr_dm_2_complex =',tr_dm_2_complex
    stop
   endif
  endif
 enddo
 deallocate(tr_dm_2_complex_tmp_part1,tr_dm_2_complex_tmp_part2, &
          LL_one_body_dm_ao_for_dft_tmp,   &
          SS_one_body_dm_ao_for_dft_tmp,   &
          LS_one_body_dm_ao_for_dft_tmp,   &
          SL_one_body_dm_ao_for_dft_tmp)
 end

 BEGIN_PROVIDER [double precision, dirac_one_body_tr_dm_2_at_r, (n_points_final_grid,N_states) ]
 implicit none
 BEGIN_DOC
 ! dirac_one_body_tr_dm_2_at_r(i,istate) = tr_dm_2(r_i,i_state) 
 ! where r_i is the ith point of the grid and istate is the state number
 END_DOC
 integer :: i,istate
 double precision :: r(3)
 double precision, allocatable :: tr_dm_2(:)
 allocate(tr_dm_2(N_states))
 do istate = 1, N_states
  do i = 1, n_points_final_grid
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   call dirac_tr_dm_2_dft_at_r(r,tr_dm_2)
   dirac_one_body_tr_dm_2_at_r(i,istate) = tr_dm_2(istate)
  enddo
 enddo
 END_PROVIDER 
