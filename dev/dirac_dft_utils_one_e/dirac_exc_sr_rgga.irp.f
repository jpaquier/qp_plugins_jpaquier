 subroutine dirac_ex_rgga_sr(mu,rho,tr_gamma_2,grad_rho_x,grad_rho_y,grad_rho_z,grad_rho_2,grad_rho_on_top_2,e_x,v_x)
 include 'constants.include.F'
 implicit none 
 integer :: i,j,k
 double precision, intent(out) ::  e_x,v_x
 double precision, intent(in)  ::  mu,rho,tr_gamma_2,grad_rho_x,grad_rho_y,grad_rho_z,grad_rho_2,grad_rho_on_top_2
 double precision :: rho_lda, rho_new, grad_rho_2_lda,grad_rho_x_new, grad_rho_y_new, grad_rho_z_new
 double precision :: e_x_0
 double precision :: z1,z2,z3,c10
 double precision :: f13,ckf,kF,kF_3,kF_4,c,tmp_c,tmp_c_2,tmp_c_4,tmp_c_m,tmp_c_m_2,tmp_c_m_4,sq,sq_2
 double precision :: mu_2,mu_3,tmp_mu,tmp_mu_2,tmp_mu_3
 f13 = 0.3333333333333333d0
 z1 = 1.d0
 z2 = 2.d0
 z3 = 3.d0
 ckf = 3.0936677262801355d0
 c10 = 0.001343813934716645d0
 c = speed_of_light
!if (dirac_rho == "rho") then
!!!! To use the usual electronic density
  rho_lda = rho
  grad_rho_2_lda = grad_rho_2
!elseif (dirac_rho == "rho_on_top") then
!!!! To use the electronic density obtained from the on-top pair density 
! rho_lda = dsqrt(2.d0*tr_gamma_2)
! grad_rho_2_lda = grad_rho_on_top_2
! if (dirac_effective_rho == "yes") then
!  !!! To use the effective electronic density obtained from the on-top pair density
!  if (tr_gamma_2 .gt. 1d-5) then
!   grad_rho_x_new = grad_rho_x
!   grad_rho_y_new = grad_rho_y
!   grad_rho_z_new = grad_rho_z    
!   rho_new = rho_lda
!   tmp_c = c/(ckf*(rho_new**f13))
!   do j = 1, 4
!    rho_new = rho_lda*coef(tmp_c) 
!    tmp_c = c/(ckf*(rho_new**f13)) 
!    grad_rho_x_new = coef(tmp_c)*grad_rho_x + coef_derivative(rho_new,tmp_c)*grad_rho_x_new*rho_lda 
!    grad_rho_y_new = coef(tmp_c)*grad_rho_y + coef_derivative(rho_new,tmp_c)*grad_rho_y_new*rho_lda 
!    grad_rho_z_new = coef(tmp_c)*grad_rho_z + coef_derivative(rho_new,tmp_c)*grad_rho_z_new*rho_lda 
!   enddo
!   rho_lda = rho_new
!   grad_rho_2_lda = grad_rho_x_new**2 + grad_rho_y_new**2 + grad_rho_z_new**2
!  endif
! endif
!endif
 if (rho_lda .gt. 1d-10 ) then
  kF = ckf*(rho_lda**f13)
  kF_3 = kF*kF*kF
  kF_4 = kF_3*kF
  tmp_c = c/kF
  tmp_c_2 = tmp_c*tmp_c
  tmp_c_4 = tmp_c_2*tmp_c_2
  tmp_c_m = 1.d0/tmp_c
  tmp_c_m_2 = 1.d0/tmp_c_2
  tmp_c_m_4 = 1.d0/tmp_c_4
  mu_2 = mu*mu
  mu_3 = mu_2*mu
  tmp_mu = mu/kF
  tmp_mu_2 = tmp_mu*tmp_mu
  tmp_mu_3 = tmp_mu_2*tmp_mu
  sq=grad_rho_2_lda*2.6121172985233599567768d-2*rho_lda**(-8d0/3d0)
  sq_2 = sq*sq
  !!Non-relativistic equations
  if (tmp_c .gt. 2d+1) then
   e_x_0 = 0.002687627869433291d0*kF_4*(-3.d0) + 0.002687627869433291d0*kF_4*(-3.d0)*((0.3402*sq + 5.9955*sq_2)/(1 + 27.5026*sq + 5.7728*sq_2))
   e_x = (e_x_0 + kF_3*0.019054785467912103d0*mu)/(1 + (e_x_0 + kF_3*0.019054785467912103d0*mu)*(-mu_2/(1.57079603267948966d0*tr_gamma_2)))
  !e_x = (e_x_0 )/(1 + (e_x_0 )*(-mu_2/(1.57079603267948966d0*tr_gamma_2)))

  !!Relativistic equations
  else
   e_x_0 = kF_4*(c10*(-z2 + tmp_c_2 + z2*(z1 + tmp_c_2)**2*dlog(z1 + z1/tmp_c_2) -                                       &
         z2*dsqrt(z1 + tmp_c_2)*(z2 + z3*tmp_c_2)*dlog(dsqrt(z1 + tmp_c_m_2)                                             &
         + z1/tmp_c) + z3*tmp_c_4*dlog(dsqrt(z1 + tmp_c_m_2) + z1/tmp_c)**2))                                            &    
         + 0.002687627869433291d0*kF_4*(-3.d0) * ((0.3402*sq + 5.9955*sq_2)/(1 + 27.5026*sq + 5.7728*sq_2)) *            &        
         ((1 + 2.21255*tmp_c_m_2 + 0.66915*tmp_c_m_4)/(1 + 1.32998*tmp_c_m_2 + 0.794803*tmp_c_m_4)) 
   e_x = (e_x_0 + kF_3*0.019054785467912103d0*mu)/(1 + (e_x_0 + kF_3*0.019054785467912103d0*mu)*(-mu_2/(1.57079603267948966d0*tr_gamma_2)))  
  !e_x = (e_x_0 )/(1 + (e_x_0 )*(-mu_2/(1.57079603267948966d0*tr_gamma_2)))  
  endif
! write(16,*) e_x_0, kF_3*0.019054785467912103d0*mu, (e_x_0 + kF_3*0.019054785467912103d0*mu), -mu_2/(1.57079603267948966d0*tr_gamma_2),-1.57079603267948966d0*tr_gamma_2/mu_2
 else 
  e_x = 0.d0
 endif 
 v_x =0.d0
 
 end 

