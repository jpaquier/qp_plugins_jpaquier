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

 subroutine dirac_tr_dm_dft_at_r(r,tr_dm)
 implicit none
 BEGIN_DOC
 ! input: r(1) ==> r(1) = x, r(2) = y, r(3) = z
 ! output : tr_dm = trace of the squared density matrix evaluated at r(3)
 END_DOC
 double precision, intent(in) :: r(3)
 complex*16 :: tr_dm_complex(N_states)
 double precision :: tr_dm_im
 double precision, intent(out) :: tr_dm(N_states)
 integer :: istate
 complex*16 :: large_aos_array(large_ao_num),large_aos_array_bis(large_ao_num)
 complex*16 :: small_aos_array(small_ao_num),small_aos_array_bis(small_ao_num),u_dotc_v
 call give_all_large_aos_at_r(r,large_aos_array)
 call give_all_small_aos_at_r(r_bis,large_aos_array)
 do istate = 1, N_states
  large_aos_array_bis = large_aos_array
  small_aos_array_bis = small_aos_array
  !L_alpha L_alpha* x L_alpha L_alpha* 
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

