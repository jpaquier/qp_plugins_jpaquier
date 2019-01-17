BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_alpha, (ao_num,ao_num) ]
   implicit none
   BEGIN_DOC
   ! S^-1 x Alpha density matrix in the AO basis x S^-1
   END_DOC
     
!  call dgemm('N','T',ao_num,ao_num,elec_alpha_num,1.d0, &
!       mo_coef, size(mo_coef,1), &
!       mo_coef, size(mo_coef,1), 0.d0, &
!       HF_density_matrix_ao_alpha, size(HF_density_matrix_ao_alpha,1))
   integer :: i,j,k,l
   double precision :: test_alpha
   HF_density_matrix_ao_alpha = 0.d0
   do i = 1, mo_tot_num
    do j = 1, mo_tot_num
     if(dabs(mo_general_density_alpha(i,j)).le.1.d-10)cycle
     do k = 1, ao_num
      do l = 1, ao_num
       HF_density_matrix_ao_alpha(k,l) += mo_coef(k,i) * mo_coef(l,j) * mo_general_density_alpha(i,j)
      enddo
     enddo
    enddo
   enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_beta,  (ao_num,ao_num) ]
   implicit none
   BEGIN_DOC
   ! S^-1 Beta density matrix in the AO basis x S^-1
   END_DOC
   
!  call dgemm('N','T',ao_num,ao_num,elec_beta_num,1.d0, &
!       mo_coef, size(mo_coef,1), &
!       mo_coef, size(mo_coef,1), 0.d0, &
!       HF_density_matrix_ao_beta, size(HF_density_matrix_ao_beta,1))
   integer :: i,j,k,l
   double precision :: test_beta
   HF_density_matrix_ao_beta = 0.d0
   do i = 1, mo_tot_num
    do j = 1, mo_tot_num
     do k = 1, ao_num
      do l = 1, ao_num
       HF_density_matrix_ao_beta(k,l) += mo_coef(k,i) * mo_coef(l,j) * mo_general_density_beta(i,j)
      enddo
     enddo
    enddo
   enddo

END_PROVIDER
 
BEGIN_PROVIDER [ double precision, HF_density_matrix_ao, (ao_num,ao_num) ]
   implicit none
   BEGIN_DOC
   ! S^-1 Density matrix in the AO basis S^-1
   END_DOC
   ASSERT (size(HF_density_matrix_ao,1) == size(HF_density_matrix_ao_alpha,1))
   if (elec_alpha_num== elec_beta_num) then
     HF_density_matrix_ao = HF_density_matrix_ao_alpha + HF_density_matrix_ao_alpha
   else
     ASSERT (size(HF_density_matrix_ao,1) == size(HF_density_matrix_ao_beta ,1))
     HF_density_matrix_ao = HF_density_matrix_ao_alpha + HF_density_matrix_ao_beta
   endif
   integer :: i
  !do i = 1, ao_num
  ! write(33,'(1000(F16.10,X))') HF_density_matrix_ao(i,:)
  !enddo
   
END_PROVIDER
 
