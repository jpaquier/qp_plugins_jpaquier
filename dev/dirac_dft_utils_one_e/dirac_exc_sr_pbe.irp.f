 subroutine dirac_ex_pbe_sr(mu,rho,tr_gamma_2,grad_rho_x,grad_rho_y,grad_rho_z,grad_rho_2,grad_rho_on_top_2,e_x,v_x)
 include 'constants.include.F'
 implicit none 
 integer :: i,j,k
 double precision, intent(out) ::  e_x,v_x
 double precision, intent(in)  ::  mu,rho,tr_gamma_2,grad_rho_x,grad_rho_y,grad_rho_z,grad_rho_2,grad_rho_on_top_2
 double precision :: rho_lda, rho_new, grad_rho_2_lda,grad_rho_x_new, grad_rho_y_new, grad_rho_z_new
 double precision :: e_x_lda, v_x_lda
 double precision :: f13,ckf,c,tmp_c,kappa, sq,fx
 double precision :: berf,coef,coef_derivative
 call dirac_ex_lda_sr(mu,rho,tr_gamma_2,e_x_lda,v_x_lda) 
 f13 = 0.3333333333333333d0
 ckf = 3.0936677262801355d0
 c = speed_of_light
 if (dirac_rho == "rho") then
 !!! To use the usual electronic density
  rho_lda = rho
  grad_rho_2_lda = grad_rho_2
 elseif (dirac_rho == "rho_on_top") then
 !!! To use the electronic density obtained from the on-top pair density 
  rho_lda = dsqrt(2.d0*tr_gamma_2)
  grad_rho_2_lda = grad_rho_on_top_2
  if (dirac_effective_rho == "yes") then
   !!! To use the effective electronic density obtained from the on-top pair density
   if (tr_gamma_2 .gt. 1d-5) then
    grad_rho_x_new = grad_rho_x
    grad_rho_y_new = grad_rho_y
    grad_rho_z_new = grad_rho_z    
    rho_new = rho_lda
    tmp_c = c/(ckf*(rho_new**f13))
    do j = 1, 4
     rho_new = rho_lda*coef(tmp_c) 
     tmp_c = c/(ckf*(rho_new**f13)) 
     grad_rho_x_new = coef(tmp_c)*grad_rho_x + coef_derivative(rho_new,tmp_c)*grad_rho_x_new*rho_lda 
     grad_rho_y_new = coef(tmp_c)*grad_rho_y + coef_derivative(rho_new,tmp_c)*grad_rho_y_new*rho_lda 
     grad_rho_z_new = coef(tmp_c)*grad_rho_z + coef_derivative(rho_new,tmp_c)*grad_rho_z_new*rho_lda 
    enddo
    rho_lda = rho_new
    grad_rho_2_lda = grad_rho_x_new**2 + grad_rho_y_new**2 + grad_rho_z_new**2
   endif
  endif
 endif
 kappa=0.804d0
 if (rho_lda .gt. 1d-10 ) then
  sq=grad_rho_2_lda*2.6121172985233599567768d-2*rho_lda**(-8d0/3d0)
  fx=1.d0+ kappa - kappa/(1.d0+berf(mu/(2*ckf*(rho_lda**f13)))*sq/kappa)
  e_x = e_x_lda*fx
 else 
  e_x = 0.d0
 endif 
 v_x =0.d0
 
 end 

 function coef(b) 
 ! b = tmp_c
 implicit none 
 include 'constants.include.F'
 double precision :: b 
 double precision :: coef
     coef = 2.82842712474619*dsqrt(-1.d0/(-4.d0 - 9.d0*b**2 - 9.d0*b**4 +     &
        9.d0*b**4*dlog(dsqrt(1.d0 + b**(-2)) + 1.d0/b)*(2.d0*dsqrt(1.d0 +     &
        b**2) - 1.d0*b**2*dlog(dsqrt(1.d0 + b**(-2)) + 1.d0/b)))) 
   return
  end

 function coef_derivative(a,b)
 ! a = rho_new
 ! b = tmp_c
 implicit none
 include 'constants.include.F'
 double precision :: a,b
 double precision :: coef_derivative
    coef_derivative = (a**2*b**2*(1.d0/                                                                  &                            
             (a**2*(0.44444444444444425d0 + 0.9999999999999996d0*b**2 +                                  &
                 0.9999999999999996d0*b**4 -                                                             &
                 1.999999999999999d0*b**4*dsqrt(1.d0 + 1.0000000000000002d0*b**2)*                       &
                  dlog(dsqrt(1.d0 + 1.d0/b**2) + 1.d0/b) +                                               &
                 1.d0*b**6*dlog(dsqrt(1.d0 + 1.d0/b**2) + 1.d0/b)**2)))**1.5d0*                          &
          (0.31426968052735427d0*dsqrt(1.d0 + 1.d0/b**2)*b**5 +                                          &
            0.31426968052735427d0*dsqrt(1.d0 + 1.0000000000000002d0*b**2) +                              &
            0.3142696805273542d0*dsqrt(1.d0 + 1.d0/b**2)*b*dsqrt(1.d0 + 1.0000000000000002d0*b**2) +     &
            b**4*(0.3142696805273543d0 + 0.6285393610547085d0*dsqrt(1.d0 + 1.0000000000000002d0*b**2)) + &
            b**2*(0.31426968052735427d0 + 0.9428090415820631d0*dsqrt(1.d0 + 1.0000000000000002d0*b**2)) +& 
            0.6285393610547085d0*dsqrt(1.d0 + 1.d0/b**2)*b**3*                                           &
             (0.4999999999999999d0 + 1.d0*dsqrt(1.d0 + 1.0000000000000002d0*b**2)) +                     &
            b**2*dlog(dsqrt(1.d0 + 1.d0/b**2) + 1.d0/b)*                                                 &
             (-1.2570787221094173d0 + b*(-1.257078722109417d0*dsqrt(1.d0 + 1.d0/b**2) +                  &
                  b*(-2.828427124746189d0 - 1.571348402636772d0*b**2 -                                   &
                     0.3142696805273543d0*dsqrt(1.d0 + 1.0000000000000002d0*b**2) -                      &
                     0.31426968052735427d0*dsqrt(1.d0 + 1.d0/b**2)*b*                                    &
                      (5.000000000000002d0 + 1.d0*dsqrt(1.d0 + 1.0000000000000002d0*b**2)))) +           &
               b**2*(0.9428090415820631d0 + 0.9428090415820629d0*dsqrt(1.d0 + 1.d0/b**2)*b +             &
                  0.9428090415820629d0*b**2)*dsqrt(1.d0 + 1.0000000000000002d0*b**2)*                    &
                dlog(dsqrt(1.d0 + 1.d0/b**2) + 1.d0/b))))/                                               &
        ((1.0000000000000002d0 + 1.d0*dsqrt(1.d0 + 1.d0/b**2)*b + 1.d0*b**2)*                            &
          dsqrt(1.d0 + 1.0000000000000002d0*b**2))                                                        
   return
  end
        
!-------------------------------------------
      function berf(a)
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
      double precision eta,fak,berf,berf_dexp


      eta=19.0d0
      fak=2.540118935556d0*dexp(-eta*a*a)

      if(a .lt. 0.075d0) then
      !expansion for small mu to avoid numerical problems
      !denominator becomes zero for a approximately 0.4845801308
      !(and for one negative and two complex values of a)
       berf = (-7d0+72.d0*a*a)/(27.d0*(-3d0-24.d0*a*a+32.d0*a**4+8d0*dsqrt(pi)*a))
      else if(a .gt. 50.d0) then
       berf = 1.d0/(72.d0*a*a)-1.d0/(17280.d0*a**4) - 23.d0/(358400.d0*a**6)
      else
      !Code generated by Mathematica
       berf_dexp=dexp(2.5d-1/a**2)
       berf = (1.851851851851851851851852d-2*(-1.d0 + 1.44d2*a**4*(-1.d0  &
         + berf_dexp) - 2.d0*a**2*(1.1d1 + 7.d0*berf_dexp                 &
        )))/(a**2*(3.2d1*a**4*(-1.d0 + berf_dexp) - 3.d0*berf_dexp        &
        + 1.417963080724412821838534d1*a*derf(5.d-1/a)*berf_dexp         &
        - 8.d0*a**2*(-2.d0 + 3.d0*berf_dexp)))
      endif
      berf=berf*fak
      return
      end

