 subroutine dirac_ex_rgga_sr(mu,rho,tr_gamma_2,grad_rho_x,grad_rho_y,grad_rho_z,grad_rho_2,grad_rho_on_top_2,e_x,v_x)
 include 'constants.include.F'
 implicit none 
 integer :: i,j,k
 double precision, intent(out) ::  e_x,v_x
 double precision, intent(in)  ::  mu,rho,tr_gamma_2,grad_rho_x,grad_rho_y,grad_rho_z,grad_rho_2,grad_rho_on_top_2
 double precision :: rho_lda, rho_new, grad_rho_2_lda,grad_rho_x_new, grad_rho_y_new, grad_rho_z_new
 double precision :: e_x_lda, v_x_lda,e_x_lda_nr,v_x_lda_nr
 double precision :: z1, z2, z3, z21, z140
 double precision :: c1,c5,c35
 double precision :: tmp_mu, tmp_mu_2, tmp_mu_4, tmp_mu_6
 double precision :: f13,f23,ckf,kF,kF_HF,kF_HF_3,c,tmp_c,tmp_c_2,tmp_c_4,tmp_c_6 
 double precision :: tmp_c_m, tmp_c_m_2, tmp_c_m_4, tmp_c_m_6, mu_m_c
 double precision :: kappa, sq,kx,berf_bis
 double precision :: coef_first, coef_second,coef_derivative_first,coef_derivative_second
 call dirac_ex_lda_sr(mu,rho,tr_gamma_2,e_x_lda,v_x_lda) 
 z1 = 1.d0
 z2 = 2.d0
 z3 = 3.d0
 z21 = 21.d0
 z140 = 140.d0
 f13 = 0.3333333333333333d0
 f23 = 0.6666666666666667d0
 ckf = 3.0936677262801355d0
 c = speed_of_light
 c1  = 0.008062883608299872d0
 c5  = 2.363271801207355d0
 c35 = 0.000006399113974841168d0
 mu_m_c = mu / c
 kF_HF= ckf*(rho**f13)
 kF_HF_3 = kF_HF*kF_hF*kF_HF
 if (dirac_rho == "rho") then
 !!! To use the usual electronic density
  rho_lda = rho
  grad_rho_2_lda = grad_rho_2
 elseif (dirac_rho == "rho_on_top") then
 !!! To use the electronic density obtained from the on-top pair density 
  !! First way: n_{2,x} = n_eff^2*g(n_eff)
 !rho_lda = dsqrt(2.d0*tr_gamma_2)
  !! Second way : n_2{2,x} = n^{HF}*n_eff*g(n_eff)
  rho_lda = 2.d0*tr_gamma_2/rho
  grad_rho_2_lda = grad_rho_on_top_2
  if (tr_gamma_2 .gt. 1d-5) then
   grad_rho_x_new = grad_rho_x
   grad_rho_y_new = grad_rho_y
   grad_rho_z_new = grad_rho_z    
   rho_new = rho_lda
   tmp_c = c/(ckf*(rho_new**f13))
  !!! Autocoherence for the first way
  !do j = 1, 4
  ! rho_new = rho_lda*coef_first(tmp_c) 
  ! tmp_c = c/(ckf*(rho_new**f13)) 
  ! grad_rho_x_new = coef_first(tmp_c)*grad_rho_x + coef_derivative_first(rho_new,tmp_c)*grad_rho_x_new*rho_lda 
  ! grad_rho_y_new = coef_first(tmp_c)*grad_rho_y + coef_derivative_first(rho_new,tmp_c)*grad_rho_y_new*rho_lda 
  ! grad_rho_z_new = coef_first(tmp_c)*grad_rho_z + coef_derivative_first(rho_new,tmp_c)*grad_rho_z_new*rho_lda 
  !enddo
   !! Autocoherence for the second way
   do j = 1, 10
    rho_new = rho_lda*coef_second(tmp_c) 
    tmp_c = c/(ckf*(rho_new**f13)) 
    grad_rho_x_new = coef_second(tmp_c)*grad_rho_x + coef_derivative_second(rho_new,tmp_c)*grad_rho_x_new*rho_lda 
    grad_rho_y_new = coef_second(tmp_c)*grad_rho_y + coef_derivative_second(rho_new,tmp_c)*grad_rho_y_new*rho_lda 
    grad_rho_z_new = coef_second(tmp_c)*grad_rho_z + coef_derivative_second(rho_new,tmp_c)*grad_rho_z_new*rho_lda 
   enddo
   rho_lda = rho_new
   grad_rho_2_lda = grad_rho_x_new**2 + grad_rho_y_new**2 + grad_rho_z_new**2
  endif
 endif
 kF=(ckf*(rho_lda**f13))
 tmp_c = c/kF
 tmp_c_2 = tmp_c*tmp_c
 tmp_c_4 = tmp_c_2*tmp_c_2
 tmp_c_6 = tmp_c_4*tmp_c_2
 tmp_c_m = 1.d0/tmp_c
 tmp_c_m_2 = 1.d0/tmp_c_2
 tmp_c_m_4 = 1.d0/tmp_c_4
 tmp_c_m_6 = 1.d0/tmp_c_6
 tmp_mu = mu/kF
 tmp_mu_2 = tmp_mu*tmp_mu
 tmp_mu_4 = tmp_mu_2*tmp_mu_2
 tmp_mu_6 = tmp_mu_4*tmp_mu_2
!!!Non-relativistic equations to use RGGA_P6_nr
!! Quadratic range-separation for very low values of tmp_mu
!if (tmp_mu .lt. 1.d-2) then
! e_x_lda_nr = 0.002687627869433291d0*kF_HF_3*kF*(-3.d0 + 7.089815403622064d0*tmp_mu - 6.d0*tmp_mu_2)
!! Medium values of tmp_mu
!elseif (tmp_mu .le. 1.d+2) then
! e_x_lda_nr =  -c1*kF_HF_3*kF*(z1 + f23*tmp_mu_2*(z3 - z1*tmp_mu_2 + (-z2 + tmp_mu_2)/dexp(z1/tmp_mu_2)) - c5*tmp_mu*derf(z1/tmp_mu)) 
!! Inverse quadratic/quartic/hexuple for large values of tmp_mu
!elseif (tmp_mu .lt. 1.d+9) then
! e_x_lda_nr = (-c35*kF_HF_3*kF*(z3 - z21*tmp_mu_2 + z140*tmp_mu_4))/tmp_mu_6
!! Limit for large tmp_mu 
!else
! e_x_lda_nr = 0.d0
!endif

 kappa=0.804d0
 if (rho_lda .gt. 1d-10 ) then
  sq=grad_rho_2_lda*2.6121172985233599567768d-2*rho_lda**(-8d0/3d0)
  kx=kappa - kappa/(1.d0+berf_bis(mu/(2*ckf*(rho_lda**f13)))*sq/kappa)
 !!! 
 !! x_test11
 !e_x = e_x_lda +  e_x_lda_nr*kx*((1.d0 + 2.5d0*tmp_c_m_2*(2.d0-derf(2.d0-derf(1.5d0*tmp_mu))) + 1.d0*tmp_c_m_4*(1-derf(3*tmp_mu)))/(1.d0 + 2.5d0*tmp_c_m_2*(2.d0-derf(2.d0-derf(1.5d0*tmp_mu))) + 2.5d0*tmp_c_m_4*(1/(1.d0-0.8d0*derf(tmp_mu)))))
 !! 0 parameters test
 if (dirac_rgga == "P6_nr_0p") then
  e_x = e_x_lda + e_x_lda_nr*kx
 elseif (dirac_rgga == "P6_P6_0p") then
  e_x = e_x_lda + e_x_lda*kx
!!!! 3 parameters for mu_erf = 0
! e_x = e_x_lda + e_x_lda*kx*((1.d0 + dirac_a1_3p*tmp_c_m_2 + dirac_a2_3p*tmp_c_m_4)/(1.d0 + dirac_a1_3p*tmp_c_m_2 + dirac_b2_3p*tmp_c_m_4))
!!!! 3+3 parameters for general mu_erf
! e_x = e_x_lda + e_x_lda*kx*((1.d0 + dirac_a1_3p*(1.d0-dirac_a1_bis_6p*derf(tmp_mu))*tmp_c_m_2 +               &
!       dirac_a2_3p*(1.d0-dirac_a2_bis_6p*derf(tmp_mu))*tmp_c_m_4)/(1.d0 + dirac_a1_3p*(1.d0 -                  &
!       dirac_a1_bis_6p*derf(tmp_mu))*tmp_c_m_2 + dirac_b2_3p*(1.d0 + dirac_b2_bis_6p*derf(tmp_mu))*tmp_c_m_4))
!!!! 3+3 parameters for general mu/c
 elseif (dirac_rgga == "P6_P6_6p") then
  e_x = e_x_lda + e_x_lda*kx*((1.d0 + dirac_a1_3p*(1.d0+dirac_a1_bis_6p*derf(mu_m_c))*tmp_c_m_2 +               &     
        dirac_a2_3p*(1.d0+dirac_a2_bis_6p*derf(mu_m_c))*tmp_c_m_4)/(1.d0 + dirac_a1_3p*(1.d0 +                  &
        dirac_a1_bis_6p*derf(mu_m_c))*tmp_c_m_2 + dirac_b2_3p*(1.d0 + dirac_b2_bis_6p*derf(mu_m_c))*tmp_c_m_4))
 endif
 else
  e_x = 0.d0
 endif 
 v_x =0.d0

 end 

!!!Due to a weird bug I put a berf_bis function here, it is exactly equal to the berf function
!-------------------------------------------
      function berf_bis(a)
!-------------------------------------------
!  Second-order exchange gradient expansion coefficient for erf
!  interaction
!  a = mu/(2*kF)
!
!  Author : J. Toulouse
!  Date   : 10-03-04
!-------------------------------------------
      implicit none
      include 'constants.include.F'
      double precision a
      double precision eta,fak,berf_bis,berf_bis_dexp


      eta=19.0d0
      fak=2.540118935556d0*dexp(-eta*a*a)

      if(a .lt. 0.075d0) then
      !expansion for small mu to avoid numerical problems
      !denominator becomes zero for a approximately 0.4845801308
      !(and for one negative and two complex values of a)
       berf_bis = (-7d0+72.d0*a*a)/(27.d0*(-3d0-24.d0*a*a+32.d0*a**4+8d0*dsqrt(pi)*a))
      else if(a .gt. 50.d0) then
       berf_bis = 1.d0/(72.d0*a*a)-1.d0/(17280.d0*a**4) - 23.d0/(358400.d0*a**6)
      else
      !Code generated by Mathematica
       berf_bis_dexp=dexp(2.5d-1/a**2)
       berf_bis = (1.851851851851851851851852d-2*(-1.d0 + 1.44d2*a**4*(-1.d0  &
         + berf_bis_dexp) - 2.d0*a**2*(1.1d1 + 7.d0*berf_bis_dexp                 &
        )))/(a**2*(3.2d1*a**4*(-1.d0 + berf_bis_dexp) - 3.d0*berf_bis_dexp        &
        + 1.417963080724412821838534d1*a*derf(5.d-1/a)*berf_bis_dexp         &
        - 8.d0*a**2*(-2.d0 + 3.d0*berf_bis_dexp)))
      endif
      berf_bis=berf_bis*fak
      return
      end

