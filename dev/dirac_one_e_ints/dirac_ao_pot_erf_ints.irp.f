 double precision function NAI_pol_mult_erf_dirac_ao(i_dirac_ao,j_dirac_ao,mu_in,C_center)
 implicit none
 BEGIN_DOC
 ! Computes the following integral :
 ! $\int_{-\infty}^{infty} dr \chi_i(r) \chi_j(r) \frac{\erf(\mu | r - R_C | )}{ | r - R_C | }$.
 END_DOC
 integer, intent(in)            :: i_dirac_ao,j_dirac_ao
 double precision, intent(in)   :: mu_in, C_center(3)
 integer                        :: i,j,num_A,num_B, power_A(3), power_B(3), n_pt_in
 double precision               :: A_center(3), B_center(3),integral, alpha,beta
 double precision               :: NAI_pol_mult_erf
 num_A = dirac_ao_nucl(i_dirac_ao)
 power_A(1:3)= dirac_ao_power(i_dirac_ao,1:3)
 A_center(1:3) = nucl_coord(num_A,1:3)
 num_B = dirac_ao_nucl(j_dirac_ao)
 power_B(1:3)= dirac_ao_power(j_dirac_ao,1:3)
 B_center(1:3) = nucl_coord(num_B,1:3)
 n_pt_in = n_pt_max_integrals
 NAI_pol_mult_erf_dirac_ao = 0.d0
 do i = 1, dirac_ao_prim_num(i_dirac_ao)
  alpha = dirac_ao_expo_ordered_transp(i,i_dirac_ao)
  do j = 1, dirac_ao_prim_num(j_dirac_ao)
    beta = dirac_ao_expo_ordered_transp(j,j_dirac_ao)
    integral = NAI_pol_mult_erf(A_center,B_center,power_A,power_B,alpha,beta,C_center,n_pt_in,mu_in)
    NAI_pol_mult_erf_dirac_ao += integral * dirac_ao_coef_normalized_ordered_transp(j,j_dirac_ao)*dirac_ao_coef_normalized_ordered_transp(i,i_dirac_ao)
  enddo
 enddo
 end

