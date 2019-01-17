 BEGIN_PROVIDER [double precision, ao_potential_alpha_xc, (ao_num, ao_num)] 
&BEGIN_PROVIDER [double precision, ao_potential_beta_xc, (ao_num, ao_num)] 
 implicit none
 integer :: i,j,k,l
 ao_potential_alpha_xc = 0.d0
 ao_potential_beta_xc = 0.d0
  do i = 1, ao_num
   do j = 1, ao_num
    ao_potential_alpha_xc(i,j) =  potential_c_alpha_ao(i,j,1) + potential_x_alpha_ao(i,j,1)
    ao_potential_beta_xc(i,j)  =  potential_c_beta_ao(i,j,1)  + potential_x_beta_ao(i,j,1)

!   ao_potential_alpha_xc(i,j) =  potential_c_alpha_ao_new(i,j,1) + potential_x_alpha_ao_new(i,j,1)
!   ao_potential_beta_xc(i,j)  =  potential_c_beta_ao_new(i,j,1)  + potential_x_beta_ao_new(i,j,1)
   enddo
  enddo
END_PROVIDER 

BEGIN_PROVIDER [double precision, e_exchange_dft]
 implicit none
  e_exchange_dft = energy_x(1)

! e_exchange_dft = energy_x_new(1)
 
END_PROVIDER 

BEGIN_PROVIDER [double precision, e_correlation_dft]
 implicit none
  e_correlation_dft = energy_c(1)

! e_correlation_dft = energy_c_new(1)
 
END_PROVIDER 
