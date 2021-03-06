 BEGIN_PROVIDER [complex*16,dirac_mo_coef_electronic_for_dft, (2*dirac_ao_num,elec_num)]
 implicit none
 BEGIN_DOC
 !This is the matrix of the eigenvectors of the Fock matrix 
 ! corresponding to filled electronic states in the ao basis
 END_DOC
 integer :: i,j,j_plus
 do j =1, elec_num
  do i = 1, 2*dirac_ao_num
   dirac_mo_coef_electronic_for_dft(i,j) = dirac_mo_coef(i,j + 2*small_ao_num)
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [complex*16, dirac_one_body_dm_ao_for_dft, (2*dirac_ao_num,2*dirac_ao_num,N_states) ]
   implicit none
   BEGIN_DOC
   !Density matrix in the AOs
   ! C.C* 
   ! with C the dirac_mo_coef_electronic_for_dft 
   END_DOC
   complex*16, Allocatable    :: dirac_one_body_dm_ao_for_dft_tmp(:,:,:)
   Allocate (dirac_one_body_dm_ao_for_dft_tmp(2*dirac_ao_num,2*dirac_ao_num,N_states))
   call zgemm('N','C',2*dirac_ao_num,2*dirac_ao_num,elec_num,(1.d0,0.d0), &
        dirac_mo_coef_electronic_for_dft, size(dirac_mo_coef_electronic_for_dft,1), &
        dirac_mo_coef_electronic_for_dft, size(dirac_mo_coef_electronic_for_dft,1), (0.d0,0.d0), &
        dirac_one_body_dm_ao_for_dft_tmp, size(dirac_one_body_dm_ao_for_dft_tmp,1))
  dirac_one_body_dm_ao_for_dft = (dirac_one_body_dm_ao_for_dft_tmp)
  deallocate(dirac_one_body_dm_ao_for_dft_tmp)
 END_PROVIDER

