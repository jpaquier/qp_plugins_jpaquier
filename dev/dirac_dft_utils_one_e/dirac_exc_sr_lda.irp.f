 subroutine dirac_ec_lda_sr(mu,rho,e_c,v_c)
      implicit none
 include 'constants.include.F'
      double precision, intent(out) ::  e_c
      double precision, intent(out) ::  v_c
      double precision, intent(in)  ::  mu,rho
     e_c = 0.d0 
     v_c = 0.d0
 end


 subroutine dirac_ex_lda_sr(mu,rho,tr_gamma_2,e_x,v_x)
 include 'constants.include.F'
 implicit none 
 integer :: i,j
 double precision, intent(out) ::  e_x
 double precision, intent(out) ::  v_x
 double precision, intent(in)  ::  rho,tr_gamma_2,mu
 double precision :: rho_lda, kF, c, tmp_c, tmp_mu
 double precision :: kF_4,kF_6,tmp_c_2,tmp_c_3,tmp_c_4,tmp_c_5,tmp_c_6,tmp_c_7,tmp_c_8,tmp_c_m,tmp_c_m_2
 double precision :: tmp_mu_2,tmp_mu_3,tmp_mu_4,tmp_mu_6,tmp_mu_8,tmp_mu_10,tmp_mu_12,tmp_mu_14,tmp_mu_16
 double precision :: tmp_mu_m,tmp_mu_m_2
 double precision :: z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z12,z13,z14,z15,z16,z17,z18,z20,z21,z22,z23,z24,z25,z27,z28
 double precision :: z30,z32,z35,z36,z44,z45,z48,z50,z56,z60,z64,z70,z72,z74,z80,z96,z105,z112,z120,z132,z140,z144,z149
 double precision :: z160,z163,z164,z175,z180,z216,z288
 double precision :: z313,z315,z320,z328,z420,z492,z504,z525,z540,z559,z592,z648,z672,z693,z735,z756,z768,z770,z86,z864,z906,z945
 double precision :: z960,z984,z1466,z1680,z1715,z2100,z2304,z2625,z3280,z6300,z8064
 double precision :: z8694,z13533,z14000,z19680,z28704,z80640
 double precision :: f16,f13,f120,f23,f25,f43
 double precision :: ckf,kF_HF,kF_HF_3 
 double precision :: c0,c1,c2,c2bis,c2ter,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15
 double precision :: c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30
 double precision :: c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44
 double precision :: c45,c46,c47,c48,c49,c50,c51,c52
 
 z1  = 1.d0
 z2  = 2.d0
 z3 = 3.d0
 z4 = 4.d0
 z5 = 5.d0
 z6 = 6.d0
 z7 = 7.d0
 z8 = 8.d0
 z9 = 9.d0
 z12 = 12.d0
 z13 = 13.d0
 z14 = 14.d0
 z15 = 15.d0
 z16 = 16.d0
 z17 = 17.d0
 z18 = 18.d0
 z20 = 20.d0
 z21 = 21.d0
 z24 = 24.d0
 z25 = 25.d0
 z27 = 27.d0
 z28 = 28.d0
 z30 = 30.d0
 z32 = 32.d0
 z35 = 35.d0
 z36 = 36.d0
 z44 = 44.d0
 z45 = 45.d0
 z48 = 48.d0
 z50 = 50.d0
 z56 = 56.d0
 z60 = 60.d0
 z64 = 64.d0
 z70 = 70.d0
 z72 = 72.d0
 z74 = 74.d0
 z80 = 80.d0
 z96 = 96.d0
 z105 = 105.d0
 z112 = 112.d0
 z120 = 120.d0
 z132 = 132.d0
 z140 = 140.d0
 z144 = 144.d0
 z149 = 149.d0
 z160 = 160.d0
 z163 = 163.d0
 z164 = 164.d0
 z175 = 175.d0
 z180 = 180.d0
 z216 = 216.d0
 z288 = 288.d0
 z313 = 313.d0
 z315 = 315.d0
 z320 = 320.d0      
 z328 = 328.d0 
 z420 = 420.d0
 z492 = 492.d0
 z504 = 504.d0
 z525 = 525.d0
 z540 = 540.d0
 z559 = 559.d0
 z592 = 592.d0
 z648 = 648.d0
 z672 = 672.d0
 z693 = 693.d0
 z735 = 735.d0
 z756 = 756.d0
 z768 = 768.d0
 z770 = 770.d0
 z864 = 864.d0
 z906 = 906.d0
 z945 = 945.d0
 z960 = 960.d0
 z984 = 984.d0
 z1466 = 1466.d0
 z1680 = 1680.d0
 z1715 = 1715.d0
 z2100 = 2100.d0
 z2304 = 2304.d0
 z2625 = 2625.d0
 z3280 = 3280.d0
 z6300 = 6300.d0 
 z8064 = 8064.d0
 z8694 = 8694.d0
 z13533 = 13533.d0
 z14000 = 14000.d0
 z19680 = 19680.d0
 z28704 = 28704.d0
 z80640 = 80640.d0
 f16 = 0.16666666666666666d0
 f13 = 0.3333333333333333d0
 f120 = 0.05d0
 f23 = 0.6666666666666667d0
 f25 = 0.4d0
 f43 = 1.3333333333333333d0
 ckf = 3.0936677262801355d0
 c1  = 0.008062883608299872d0
 c0  = 0.01905478546791209d0
 c2bis = 0.005375255738866582d0
 c2ter = 3.544907701811032d0
 c3  = 0.3183098861837907d0
 c4  = 0.5641895835477563d0
 c5  = 2.363271801207355d0
 c6  = 0.3183098861837907d0
 c7  = 1.772453850905516d0
 c8 = 0.0008958759564777636d0
 c9 = 0.05305164769729845d0
 c10 = 0.001343813934716645d0
 c11 = 0.1061032953945969d0
 c12 = 0.00004479379782388818d0
 c13 = 7.089815403622064d0
 c14 = 10.6347231054331d0
 c15 = 21.26944621086619d0
 c16 = 50.26548245743669d0
 c17 = 42.53889242173238d0
 c18 = 0.0002239689891194409d0
 c19 = 53.17361552716548d0
 c20 = 26.58680776358274d0
 c21 = 0.001473656880480512d0
 c22 = 255.2333545303943d0
 c23 = 31.90416931629929d0
 c24 = 14.17963080724413d0
 c25 = 150.7964473723101d0
 c26 = 28.35926161448826d0
 c27 = 56.71852322897651d0 
 c28 = 16.75516081914556d0
 c29 = 0.0000003499515454991264d0       
 c30 = 0.0001119844945597204d0
 c31 = 0.01326291192432461d0
 c32 = 0.00008289319952702882d0
 c33 = 0.000000004166089827370552d0
 c34 = 0.0000001644706339822d0
 c35 = 0.000006399113974841168d0
 c36 = 0.01061032953945969d0
 c37 = 0.0003789403406949889d0
 c38 = 0.0353677651315323d0
 c39 = 9.384732821581892d0
 c40 = 3.371549979772564d+9
 c41 = 1.719237205899231d+6
 c42 = 389.6363641360097d0
 c43 = 3.52275361d+8
 c44 = 5.152090145859064d0
 c45 = 27069.58218509975d0
 c46 = 13.80345334341147d0
 c47 = 18769.d0
 c48 = 0.0005099248761589486d0
 c49 = 0.02258151625021997d0
 c = speed_of_light
 kF_HF= ckf*(rho**f13)
 kF_HF_3 = kF_HF*kF_hF*kF_HF
 if (dirac_rho == "rho") then
 !!! To use the usual electronic density
  kF = ckf*(rho**f13)
 elseif (dirac_rho == "rho_on_top") then
  !! To use the electronic density obtained from the on-top pair density 
  !! First way: n_{2,x} = n_eff^2*g(n_eff)
 !rho_lda = dsqrt(2.d0*tr_gamma_2)
 !kF = ckf*(rho_lda**f13)
  !! Second way : n_2{2,x} = n^{HF}*n_eff*g(n_eff)
  rho_lda = 2.d0*tr_gamma_2/rho
  kF = ckf*(rho_lda**f13)
  if (dirac_effective_rho == "yes") then
   !!! To use the effective electronic density obtained from the on-top pair density
   if (tr_gamma_2 .gt. 1d-5) then
    tmp_c = c/kF
    do j = 1,6 
    !! Autocoherence for the first way
    !kF = 4.375106855981304d0*(rho_lda*dsqrt(-1.d0/(-4.d0 - 9.d0*tmp_c**2 -       &    
    !    9.d0*tmp_c**4 + 9.d0*tmp_c**4*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)*  &     
    !    (2.d0*dsqrt(1.d0 + tmp_c**2) - tmp_c**2*dlog(dsqrt(1.d0 + tmp_c**(-2)) +     &
    !    1.d0/tmp_c)))))**f13
    !! Autocoherence for the second way
     kF =  7.795554179441508d0*((-1.d0*rho_lda)/(-4.d0 - 9.d0*tmp_c**2 - 9.d0*tmp_c**4 +                        &
           9.d0*tmp_c**4*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c)*                                          &
           (2.d0*dsqrt(1.d0 + tmp_c**2) - 1.d0*tmp_c**2*dlog(dsqrt(1.d0 + tmp_c**(-2)) + 1.d0/tmp_c))))**f13 
     tmp_c = c/kF
    enddo
   endif
  endif
!kF = ((kF + ckf*(rho**f13)) + (kF - ckf*(rho**f13))*derf(mu/kF))/2
 endif
 kF_4 = kF*kF*kF*kF
 kF_6 = kF_4*kF*kF
 tmp_c = c/kF
 tmp_c_2 = tmp_c*tmp_c
 tmp_c_3 = tmp_c_2*tmp_c
 tmp_c_4 = tmp_c_3*tmp_c
 tmp_c_5 = tmp_c_4*tmp_c
 tmp_c_6 = tmp_c_5*tmp_c
 tmp_c_7 = tmp_c_6*tmp_c
 tmp_c_8 = tmp_c_7*tmp_c
 tmp_c_m = 1.d0/tmp_c
 tmp_c_m_2 = 1.d0/tmp_c_2
 tmp_mu = mu/kF
 tmp_mu_2 = tmp_mu*tmp_mu
 tmp_mu_3 = tmp_mu_2*tmp_mu
 tmp_mu_4 = tmp_mu_3*tmp_mu
 tmp_mu_6 = tmp_mu_4*tmp_mu_2
 tmp_mu_8 = tmp_mu_6*tmp_mu_2
 tmp_mu_10 = tmp_mu_8*tmp_mu_2
 tmp_mu_12 = tmp_mu_10*tmp_mu_2
 tmp_mu_14 = tmp_mu_12*tmp_mu_2
 tmp_mu_16 = tmp_mu_14*tmp_mu_2
 tmp_mu_m = 1.d0/tmp_mu
 tmp_mu_m_2 = 1.d0/tmp_mu_2


 ! Non-relativistic equations
 if (dirac_approximant == "non-relativistic" .or. tmp_c .gt. 2d+1) then
    
 ! Linear/quadratic range-separation for very low values of tmp_mu
  if (tmp_mu .lt. 1.d-2) then

  !e_x = 0.002687627869433291d0*kF_4*(-3.d0 + 7.089815403622064d0*tmp_mu)
  !e_x = 0.002687627869433291d0*kF_4*(-3.d0 + 7.089815403622064d0*tmp_mu - 6.d0*tmp_mu_2)
   !! To integrate on the real density of the system
   e_x = 0.002687627869433291d0*kF_HF_3*kF*(-3.d0 + 7.089815403622064d0*tmp_mu - 6.d0*tmp_mu_2)

  !v_x = 0.3183098861837907d0*kF*(-1.d0 + 1.772453850905516d0*tmp_mu)
  !v_x = 0.3183098861837907d0*kF*(-1.d0 + (1.772453850905516d0 - 1.d0*tmp_mu)*tmp_mu)
  
  ! Medium values of tmp_mu
  elseif (tmp_mu .le. 1.d+2) then
 
  !e_x =  -c1*kF_4*(z1 + f23*tmp_mu_2*(z3 - z1*tmp_mu_2 + (-z2 + tmp_mu_2)/dexp(z1/tmp_mu_2)) - c5*tmp_mu*derf(z1/tmp_mu)) 
   e_x =  -c1*kF_HF_3*kF*(z1 + f23*tmp_mu_2*(z3 - z1*tmp_mu_2 + (-z2 + tmp_mu_2)/dexp(z1/tmp_mu_2)) - c5*tmp_mu*derf(z1/tmp_mu)) 

  !v_x = c6*kF*(-z1 + (-z1 + dexp(-z1/tmp_mu_2))*tmp_mu_2 + c7*tmp_mu*derf(z1/tmp_mu)) 
  
  ! Inverse quadratic/quartic/hexuple for large values of tmp_mu
  elseif (tmp_mu .lt. 1.d+9) then
  
  !e_x = (-c8*kF_4)/tmp_mu_2 
  !e_x = (c12*kF_4*(z3 - z20*tmp_mu_2))/tmp_mu_4 
  !e_x = (-c35*kF_4*(z3 - z21*tmp_mu_2 + z140*tmp_mu_4))/tmp_mu_6
   e_x = (-c35*kF_HF_3*kF*(z3 - z21*tmp_mu_2 + z140*tmp_mu_4))/tmp_mu_6

  !v_x = (-c9*kF)/tmp_mu_2
  !v_x = (c36*(kF - z5*kF*tmp_mu_2))/tmp_mu_4
  !v_x = (-c37*kF*(z5 - z28*tmp_mu_2 + z140*tmp_mu_4))/tmp_mu_6

  ! Limit for large tmp_mu 
  else

   e_x = 0.d0
   v_x = 0.d0

  endif

 ! Relativistic equations
 else
 
  ! For the Coulomb ee interaction
  if (dirac_interaction == "Coulomb") then
 
   ! Linear/quadratic range-separation for very low values of tmp_mu
   if (tmp_mu .lt. 1.d-1) then 
 ! if (tmp_mu .lt. 6.d-2) then

  ! e_x = kF_4*(c10*(-z2 + tmp_c_2 + z2*(z1 + tmp_c_2)**2*dlog(z1 + z1/tmp_c_2) - z2*dsqrt(z1 + tmp_c_2)*(z2 + z3*tmp_c_2)*dlog(dsqrt(z1 + tmp_c_m_2)    & 
  !     + z1/tmp_c) + z3*tmp_c_4*dlog(dsqrt(z1 + tmp_c_m_2) + z1/tmp_c)**2) + c2*tmp_mu)                                                                                
   !e_x =  kF_4*(c10*(-z2 + tmp_c_2 + z2*(z1 + tmp_c_2)**2*dlog(z1 + z1/tmp_c_2) - z2*dsqrt(z1 + tmp_c_2)*(z2 + z3*tmp_c_2)*dlog(dsqrt(z1 + tmp_c_m_2)    & 
    e_x =  kF_HF_3*kF*(c10*(-z2 + tmp_c_2 + z2*(z1 + tmp_c_2)**2*dlog(z1 + z1/tmp_c_2) - z2*dsqrt(z1 + tmp_c_2)*(z2 + z3*tmp_c_2)*dlog(dsqrt(z1 + tmp_c_m_2)    & 
        + z1/tmp_c) + z3*tmp_c_4*dlog(dsqrt(z1 + tmp_c_m_2) + z1/tmp_c)**2) +c2bis*(c2ter - z3*tmp_mu)*tmp_mu)

 ! v_x =  kF*((c11*(-z1 - z1*tmp_c_2 + (z1 + tmp_c_2)**2*dlog(z1 + z1/tmp_c_2) +  dsqrt(z1 + tmp_c_2)*(z2 + z3*tmp_c_2)*dlog(tmp_c/(z1 +                 & 
  !     dsqrt(z1 + tmp_c_2)))))/(z1 + tmp_c_2) + c4*tmp_mu)
   !v_x =  kF*((c11*(-z1 - z1*tmp_c_2 + (z1 + tmp_c_2)**2*dlog(z1 + z1/tmp_c_2) +  dsqrt(z1 + tmp_c_2)*(z2 + z3*tmp_c_2)*dlog(tmp_c/(z1 +                 & 
   !    dsqrt(z1 + tmp_c_2)))))/(z1 + tmp_c_2) + c3*(c7 - tmp_mu)*tmp_mu)

   ! Medium values of tmp_mu
  !elseif (tmp_mu .le. 1d+1) then
  !elseif (tmp_mu .le. 1.25d0) then
   elseif (tmp_mu .le. 1.0d0) then

    if (dirac_approximant == "dirac_pade_order_2") then

    !e_x = (c12*kF_4*(z60*(-z3 + z2*tmp_mu_2*(-z3 + tmp_mu_2 - (z1*(-z2 + tmp_mu_2))*dexp(-tmp_mu_m_2)) +                       &           
     e_x = (c12*kF_HF_3*kF*(z60*(-z3 + z2*tmp_mu_2*(-z3 + tmp_mu_2 - (z1*(-z2 + tmp_mu_2))*dexp(-tmp_mu_m_2)) +                       &           
        c13*tmp_mu*derf(tmp_mu_m)) + (z20*(z1 + z6*tmp_mu_4*(z3 - z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)*dexp(-tmp_mu_m_2)) -        &
        c14*tmp_mu_3*derf(tmp_mu_m))**2 - (z3*(-z2*tmp_mu_2*(-z2 + tmp_mu_2) + dexp(tmp_mu_m_2)*(-z3 - z6*tmp_mu_2 +                &
        z2*tmp_mu_4 + c13*tmp_mu*derf(tmp_mu_m)))*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 -    &
        z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8 + c15*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))))*dexp(-z2/tmp_mu_2))/    &
        (tmp_c_2*(z1 + z6*tmp_mu_4*(z3- z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)*dexp(-tmp_mu_m_2)) - c14*tmp_mu_3*derf(tmp_mu_m)))))/    &
        (z1 + (f120*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 +          & 
        z648*tmp_mu_8 + c15*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))))/(tmp_c_2*(z6*tmp_mu_4 - z12*tmp_mu_6 +                    &
        dexp(tmp_mu_m_2)*(-z1 - z18*tmp_mu_4 + z12*tmp_mu_6 + c14*tmp_mu_3*derf(tmp_mu_m))))) 
  
 
    v_x = (0.0353677651315323d0*kF*(864.d0*tmp_mu**10*(96.d0 + 559.d0*tmp_mu_2 - 882.d0*tmp_mu_4 - 1932.d0*tmp_mu_6 + 2734.d0*tmp_mu_8 + 150.d0*tmp_c_4*(z1 -    &
        z2*tmp_mu_2)**2 + 30.d0*tmp_c_2*(z8 + z5*tmp_mu_2 - 86.d0*tmp_mu_4 + 88.d0*tmp_mu_6)) - 432.d0*dexp(z1/tmp_mu_2)*tmp_mu_6*(52.d0 +              & 
        296.d0*tmp_mu_2 + 100.d0*tmp_c_4*(z1 + tmp_mu_2 + 9.d0*tmp_mu_4 - 48.d0*tmp_mu_6 + 36.d0*tmp_mu_8) + tmp_mu_4*(2879.d0 + 4.d0*tmp_mu_2*(2791.d0 -    &
        5520.d0*tmp_mu_2 - 2895.d0*tmp_mu_4 + 4101.d0*tmp_mu_6)) + 5.*tmp_c_2*(29.d0 + 4.d0*tmp_mu_2*(23.d0 + 254.d0*tmp_mu_2 - 247.d0*tmp_mu_4 -               &
        978.d0*tmp_mu_6 + 792.d0*tmp_mu_8))) + 9.d0*dexp(z2/tmp_mu_2)*tmp_mu_2*(169.d0 + 400.d0*tmp_c_4*(-z1 - z18*tmp_mu_4 + z12*tmp_mu_6)*(-z1 -    &
        z12*tmp_mu_2 - z6*tmp_mu_4 + 36.d0*tmp_mu_6) + z8*tmp_mu_2*(247.d0 + tmp_mu_2*(2911.d0 + z6*tmp_mu_2*(2018.d0 + 16613.d0*tmp_mu_2 +             &
        28718.d0*tmp_mu_4 - 55296.d0*tmp_mu_6 - 11568.d0*tmp_mu_8 + 16404.d0*tmp_mu**10))) + 40.d0*tmp_c_2*(z13 + z8*tmp_mu_2*(18.d0 + tmp_mu_2*(143.d0 +    &
        3.d0*tmp_mu_2*(185.d0 + 774.d0*tmp_mu_2 - 923.d0*tmp_mu_4 - 1182.d0*tmp_mu_6 + 792.d0*tmp_mu_8))))) - z1*dexp(z3/tmp_mu_2)*(1261.d0 +                &
        3600.d0*tmp_c_4*(z1 + tmp_mu_2)*(z1 + z18*tmp_mu_4 - z12*tmp_mu_6)**2 + 120.d0*tmp_c_2*(-z1 - z18*tmp_mu_4 + z12*tmp_mu_6)*(-34.d0 +      &
        3.d0*tmp_mu_2*(-z13 - 500.d0*tmp_mu_2 - 1520.d0*tmp_mu_4 - 132.d0*tmp_mu_6 + 528.d0*tmp_mu_8)) + 9.d0*tmp_mu_2*(169.d0 + z8*tmp_mu_2*(1455.d0 +     &
        tmp_mu_2*(4165.d0 + z6*tmp_mu_2*(5629.d0 + 24834.d0*tmp_mu_2 + 29620.d0*tmp_mu_4 - 34980.d0*tmp_mu_6 - 3852.d0*tmp_mu_8 + 5468.d0*tmp_mu**10))))) +    &
        15.95208465814964d0*dexp(z1/tmp_mu_2)*tmp_mu*derf(z1/tmp_mu)* (144.d0*tmp_mu_8*(12.d0*(z4 + z5*tmp_c_2)**2 + z2*(726.d0 + 525.d0*tmp_c_2 -        &
        400.d0*tmp_c_4)*tmp_mu_2 + (113.d0 + 400.d0*tmp_c_2*(-11.d0 + tmp_c_2))*tmp_mu_4 + 4.d0*(-1357.d0 + 490.d0*tmp_c_2)*tmp_mu_6 + 2736.d0*tmp_mu_8) -     &
        48.d0*dexp(z1/tmp_mu_2)*tmp_mu_4*(104.d0 + 738.d0*tmp_mu_2 + 200.d0*tmp_c_4*(z1 + z2*tmp_mu_2 + 15.d0*tmp_mu_4 - 36.d0*tmp_mu_6 +                 &
        z12*tmp_mu_8) + tmp_mu_4*(6881.d0 + z6*tmp_mu_2*(5646.d0 - 1731.d0*tmp_mu_2 - 8164.d0*tmp_mu_4 + 2736.d0*tmp_mu_6)) + 10.d0*tmp_c_2*(29.d0 +        &
        3.d0*tmp_mu_2*(43.d0 + 410.d0*tmp_mu_2 + 214.d0*tmp_mu_4 - 1272.d0*tmp_mu_6 + 392.d0*tmp_mu_8)) + 5.317361552716548d0*tmp_mu_3*(-12.d0*(z4 +           &
        z5*tmp_c_2)**2 + z2*(-893.d0 - 900.d0*tmp_c_2 + 200.d0*tmp_c_4)*tmp_mu_2 + (-2501.d0 + 2680.d0*tmp_c_2)*tmp_mu_4 +                                   &
        4320.d0*tmp_mu_6)*derf(z1/tmp_mu)) + dexp(z2/tmp_mu_2)*(169.d0 + 400.d0*tmp_c_4*(-z1 - z18*tmp_mu_4 + z12*tmp_mu_6)*(-z1 +                     &
        z6*tmp_mu_2*(-z2 - z5*tmp_mu_2 + z2*tmp_mu_4)) + 4.d0*tmp_mu_2*(509.d0 + 3.d0*tmp_mu_2*(2323.d0 + 4.d0*tmp_mu_2*(2575.d0 + 21734.d0*tmp_mu_2 +  &
        56868.d0*tmp_mu_4 - 2517.d0*tmp_mu_6 - 32700.d0*tmp_mu_8 + 8208.d0*tmp_mu**10))) + 40.d0*tmp_c_2*(z13 + tmp_mu_2*(149.d0 + z12*tmp_mu_2*(124.d0 +    &
        3.d0*tmp_mu_2*(157.d0 + 798.d0*tmp_mu_2 + 305.d0*tmp_mu_4 - 832.d0*tmp_mu_6 + 196.d0*tmp_mu_8)))) - 85.07778484346477d0*tmp_mu_3*derf(z1/tmp_mu)*(52.d0+&
        442.d0*tmp_mu_2 + 3.d0*tmp_mu_4*(1354.d0 + 7925.d0*tmp_mu_2 + 6821.d0*tmp_mu_4 - 4320.d0*tmp_mu_6) - 100.d0*tmp_c_4*(-z1 - 3.d0*tmp_mu_2 -            &
        21.d0*tmp_mu_4 + z12*tmp_mu_6) + z5*tmp_c_2*(29.d0 + z2*tmp_mu_2*(83.d0 + 732.d0*tmp_mu_2 + 1344.d0*tmp_mu_4 - 804.d0*tmp_mu_6)) -                &
        3.544907701811032d0*tmp_mu_3*(z6*(z4 + z5*tmp_c_2)**2 + z5*(212.d0 + 255.d0*tmp_c_2)*tmp_mu_2 + 2700.d0*tmp_mu**4)*derf(z1/tmp_mu))))))/          &
        (dexp(z1/tmp_mu_2)*(-z24*tmp_mu**4*(-4.d0 - z13*tmp_mu_2 + z27*tmp_mu**4 + z5*tmp_c_2*(-z1 + z2*tmp_mu_2)) + dexp(z1/tmp_mu_2)*(-z13 -  & 
        20.d0*tmp_c_2 - 180.d0*(z3 + z2*tmp_c_2)*tmp_mu**4 + 240.d0*(-4.d0 + tmp_c_2)*tmp_mu_6 + z648*tmp_mu_8 + c15*tmp_mu_3*(z8 +      &
        10.d0*tmp_c_2 + z45*tmp_mu**2)*derf(z1/tmp_mu)))**2)

    elseif (dirac_approximant == "dirac_pade_order_4") then

    !e_x =  (c18*kF_4*tmp_c_4*(z7*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8) +          &
     e_x =  (c18*kF_HF_3*kF*tmp_c_4*(z7*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8) +          &
        c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))**2 - z8*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 - z2*tmp_mu_2 + (-z1 +                         &
        z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) +                           &
        dexp(tmp_mu_m_2)*(z163 - z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) - c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 +                          &
        z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m)))*(z6300*(-z3 + z2*tmp_mu_2*(-z3 + tmp_mu_2 - (z1*(-z2 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                         &
        c13*tmp_mu*derf(tmp_mu_m)) + (z2100*(z1 + (z18 - z6/dexp(z1/tmp_mu_2))*tmp_mu_4 + z12*(-z1 + dexp(-z1/tmp_mu_2))*tmp_mu_6 - c14*tmp_mu_3*derf(tmp_mu_m) -       &
        (f25*(-z2*tmp_mu_2*(-z2 + tmp_mu_2) + dexp(tmp_mu_m_2)*(-z3 - z6*tmp_mu_2 + z2*tmp_mu_4 + c13*tmp_mu*derf(tmp_mu_m)))* (-z3*(z24*tmp_mu_4*(z4 +                 &
        z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 +                    &
        z45*tmp_mu_2)*derf(tmp_mu_m))* (-z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) + dexp(tmp_mu_m_2)*(-z163 + z120*tmp_mu_4*(-z105 -         &
        z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) + c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m)) -                        &
        z50*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 - z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(z30*tmp_mu_4*(z56 +           &
        z1466*tmp_mu_2 + z13533*tmp_mu_4 + z28704*tmp_mu_6 - z19680*tmp_mu_8) + dexp(tmp_mu_m_2)*(-z149 + z180*tmp_mu_4*(-z105 - z1680*tmp_mu_2 - z8694*tmp_mu_4 -      &
        z8064*tmp_mu_6 + z3280*tmp_mu_8)) + c20*dexp(tmp_mu_m_2)*tmp_mu_3*(z112 + z45*tmp_mu_2*(z72 + z672*tmp_mu_2 + z1715*tmp_mu_4))*                                 &
        derf(tmp_mu_m))))/(dexp(z1/tmp_mu_2)*(z7*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 +            &
        z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))**2 + z8*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 - z2*tmp_mu_2 + (-z1 +        &
        z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(-z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) +                          &
        dexp(tmp_mu_m_2)*(-z163 + z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) + c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 +         &
        z2625*tmp_mu_4)*derf(tmp_mu_m))))))/tmp_c_2 + (z735*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 -                 &
        z960*tmp_mu_6 + z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))**3 + z14000*dexp(z2/tmp_mu_2)*(z1 + z6*tmp_mu_4*(z3 -        &       
        z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))**2*(z30*tmp_mu_4*(z56 + z1466*tmp_mu_2 + z13533*tmp_mu_4 +                  &
        z28704*tmp_mu_6 - z19680*tmp_mu_8) + dexp(tmp_mu_m_2)*(-z149 + z180*tmp_mu_4*(-z105 - z1680*tmp_mu_2 - z8694*tmp_mu_4 - z8064*tmp_mu_6 + z3280*tmp_mu_8)) +     &
        c20*dexp(tmp_mu_m_2)*tmp_mu_3*(z112 + z45*tmp_mu_2*(z72 + z672*tmp_mu_2 + z1715*tmp_mu_4))*derf(tmp_mu_m)) + z144*(-z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 -        &
        z906*tmp_mu_4 + z984*tmp_mu_6) + dexp(tmp_mu_m_2)*(-z163 + z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) +                             &
        c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m))**2*(-z2*tmp_mu_2*(-z2 + tmp_mu_2) + dexp(tmp_mu_m_2)*(-z3 -                &
        z6*tmp_mu_2 + z2*tmp_mu_4 + c13*tmp_mu*derf(tmp_mu_m))) - z420*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 -      &        
        z960*tmp_mu_6 + z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))*(z4*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 -                 &
        z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 - z906*tmp_mu_4 +                       &
        z984*tmp_mu_6) + dexp(tmp_mu_m_2)*(z163 - z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) - c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 +         &
        z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m)) - z5*(z30*tmp_mu_4*(z56 + z1466*tmp_mu_2 + z13533*tmp_mu_4 + z28704*tmp_mu_6 - z19680*tmp_mu_8) +               &
        dexp(tmp_mu_m_2)*(-z149 + z180*tmp_mu_4*(-z105 - z1680*tmp_mu_2 - z8694*tmp_mu_4 - z8064*tmp_mu_6 + z3280*tmp_mu_8)) + c20*dexp(tmp_mu_m_2)*tmp_mu_3*(z112 +    &
        z45*tmp_mu_2*(z72 + z672*tmp_mu_2 + z1715*tmp_mu_4))*derf(tmp_mu_m))*(z2*tmp_mu_2*(-z2 + tmp_mu_2) + dexp(tmp_mu_m_2)*(z3 + z6*tmp_mu_2 - z2*tmp_mu_4 -         &
        c13*tmp_mu*derf(tmp_mu_m)))))/(dexp(z1/tmp_mu_2)*tmp_c_4*(z7*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 -        &        
        z960*tmp_mu_6 + z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))**2 + z8*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 -             &
        z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(-z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) +     &             
        dexp(tmp_mu_m_2)*(-z163 + z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) + c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 +         &
        z2625*tmp_mu_4)*derf(tmp_mu_m))))))/(z12*(z60*tmp_mu_4*(-z25 - z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) + dexp(tmp_mu_m_2)*(z163 - z120*tmp_mu_4*(-z105 - &
        z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) - c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m))**2 -                     &
        z175*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) + dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8) +                               &
        c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))*(z30*tmp_mu_4*(z56 + z1466*tmp_mu_2 + z13533*tmp_mu_4 + z28704*tmp_mu_6 - z19680*tmp_mu_8) +  &
        dexp(tmp_mu_m_2)*(-z149 + z180*tmp_mu_4*(-z105 - z1680*tmp_mu_2 - z8694*tmp_mu_4 - z8064*tmp_mu_6 + z3280*tmp_mu_8)) + c20*dexp(tmp_mu_m_2)*tmp_mu_3*(z112 +    &
        z45*tmp_mu_2*(z72 + z672*tmp_mu_2 + z1715*tmp_mu_4))*derf(tmp_mu_m)) + z525*tmp_c_4*(z7*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) +                      &
        dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))**2 -                &       
        z8*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 - z2*tmp_mu_2 + (-z1 + z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(z60*tmp_mu_4*(-z25 -           &
        z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) + dexp(tmp_mu_m_2)*(z163 - z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) -              &
        c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m))) - z70*tmp_c_2*(z3*(z24*tmp_mu_4*(z4 + z13*tmp_mu_2 - z27*tmp_mu_4) +      &
        dexp(tmp_mu_m_2)*(-z13 - z540*tmp_mu_4 - z960*tmp_mu_6 + z648*tmp_mu_8) + c15*dexp(tmp_mu_m_2)*tmp_mu_3*(z8 + z45*tmp_mu_2)*derf(tmp_mu_m))*(z60*tmp_mu_4*(-z25 &             
        - z313*tmp_mu_2 - z906*tmp_mu_4 + z984*tmp_mu_6) + dexp(tmp_mu_m_2)*(z163 - z120*tmp_mu_4*(-z105 - z770*tmp_mu_2 - z945*tmp_mu_4 + z492*tmp_mu_6)) -            &
        c19*dexp(tmp_mu_m_2)*tmp_mu_3*(z50 + z756*tmp_mu_2 + z2625*tmp_mu_4)*derf(tmp_mu_m)) - z50*dexp(tmp_mu_m_2)*(z1 + z6*tmp_mu_4*(z3 - z2*tmp_mu_2 + (-z1 +        &
        z2*tmp_mu_2)/dexp(z1/tmp_mu_2)) - c14*tmp_mu_3*derf(tmp_mu_m))*(z30*tmp_mu_4*(z56 + z1466*tmp_mu_2 + z13533*tmp_mu_4 + z28704*tmp_mu_6 - z19680*tmp_mu_8) +     &
        dexp(tmp_mu_m_2)*(-z149 + z180*tmp_mu_4*(-z105 - z1680*tmp_mu_2 - z8694*tmp_mu_4 - z8064*tmp_mu_6 + z3280*tmp_mu_8)) + c20*dexp(tmp_mu_m_2)*tmp_mu_3*(z112      &
        + z45*tmp_mu_2*(z72 + z672*tmp_mu_2 + z1715*tmp_mu_4))*derf(tmp_mu_m))))  

     v_x =   (5.016210808718423d-8*((-176267.3384363221d0*kF*                                                                          &  
         (12.d0*(60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                      &
               dexp(z1/tmp_mu_2)*                                                                                                   &
                (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                          &
               53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*                         &
                derf(z1/tmp_mu))**2 - 175.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                             &
              dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                    &
              21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))*                            &
            (30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                  &
              dexp(z1/tmp_mu_2)*                                                                                                    &
               (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                         &
                    3280.d0*tmp_mu_8)) + 26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                            &
               (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)) +                            &
           525.d0*tmp_c_4*(7.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                            &
                  dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                &
                  21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))**2                      &
                - 8.d0*dexp(z1/tmp_mu_2)*                                                                                           &
               (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                           &
                 10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                      &
               (60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                       &
                 dexp(z1/tmp_mu_2)*                                                                                                 &
                  (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                        &
                 53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                    &
                  (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))) -                                                  &
           70.d0*tmp_c_2*(3.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                             &
                 dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                 &
                 21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))*                         &
               (60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                       &
                 dexp(z1/tmp_mu_2)*                                                                                                 &
                  (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                        &
                 53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                    &
                  (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)) -                                                   &
              50.d0*dexp(z1/tmp_mu_2)*                                                                                              &
               (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                           &
                 10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                      &
               (30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +               &
                 dexp(z1/tmp_mu_2)*                                                                                                 &
                  (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      &
                       3280.d0*tmp_mu_8)) +                                                                                           &
                 26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                    &
                  (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))*                        &
         (4320.d0*tmp_mu_10*(13600.d0 + 1.706072d+6*tmp_mu_2 + 2.8534952d+7*tmp_mu_4 + 5.7391306d+7*tmp_mu_6 -                        &
              2.07750436d+8*tmp_mu_8 + 4.05588137d+8*tmp_mu_10 + 1.304278563d+9*tmp_mu_12 -                                           &
              7.58674464d+8*tmp_mu_14 - 1.853448d+7*tmp_mu_16 +                                                                       &
              420.d0*tmp_c_4*(104.d0 + 4582.d0*tmp_mu_2 + 3726.d0*tmp_mu_4 - 84355.d0*tmp_mu_6 + 318724.d0*tmp_mu_8 -                 &
                 324246.d0*tmp_mu_10 + 94740.d0*tmp_mu_12) +                                                                          &
              35.d0*tmp_c_2*(1600.d0 + 117904.d0*tmp_mu_2 + 1.21908d+6*tmp_mu_4 - 2.001602d+6*tmp_mu_6 -                              &
                 6.472181d+6*tmp_mu_8 + 4.6210218d+7*tmp_mu_10 - 4.4546664d+7*tmp_mu_12 + 1.102992d+7*tmp_mu_14)) +                   &
           180.d0*dexp(z1/tmp_mu_2)*tmp_mu_6*                                                                                       &
            (-16.d0*(7620.d0 + 589499.d0*tmp_mu_2) +                                                                                  &
              tmp_mu_4*(-2.02949448d+8 + tmp_mu_2*                                                                                    &
                  (-1.14715329d+9 + tmp_mu_2*                                                                                         &
                     (-7.121960687d+9 + 12.d0*tmp_mu_2*                                                                               &
                        (-5.19410809d+8 + 4.322121126d+9*tmp_mu_2 - 8.751055192d+9*tmp_mu_4 -                                         &
                          8.187831852d+9*tmp_mu_6 + 4.440839904d+9*tmp_mu_8 + 1.1120688d+8*tmp_mu_10)))) -                            &
              840.d0*tmp_c_4*(392.d0 + tmp_mu_2*                                                                                      &
                  (13664.d0 + tmp_mu_2*(129211.d0 +                                                                                   &
                       2.d0*tmp_mu_2*(91871.d0 +                                                                                      &
                          6.d0*tmp_mu_2*(-32157.d0 - 280292.d0*tmp_mu_2 + 1.424544d+6*tmp_mu_4 - 1.256958d+6*tmp_mu_6 +               &
                             284220.d0*tmp_mu_8))))) -                                                                                &
              84.d0*tmp_c_2*(5168.d0 + tmp_mu_2*                                                                                      &
                  (280988.d0 + tmp_mu_2*(4.82065d+6 +                                                                                 &
                       tmp_mu_2*(2.3184549d+7 +                                                                                       &
                          20.d0*tmp_mu_2*(2.684689d+6 +                                                                               &
                             3.d0*tmp_mu_2*(-3.853162d+6 + 241153.d0*tmp_mu_2 + 3.241087d+7*tmp_mu_4 -                                &
                                2.7788292d+7*tmp_mu_6 + 5.51496d+6*tmp_mu_8))))))) -                                                  &
           6.d0*dexp(z2/tmp_mu_2)*tmp_mu_2*                                                                                         &
            (-161176.d0 - 1050.d0*tmp_c_4*(484.d0 +                                                                                   &
                 tmp_mu_2*(6693.d0 + 2.d0*tmp_mu_2*                                                                                   &
                     (176359.d0 + 12.d0*tmp_mu_2*                                                                                     &
                        (174151.d0 + tmp_mu_2*                                                                                        &
                           (136575.d0 + 2.d0*tmp_mu_2*                                                                                &
                              (382873.d0 + 6.d0*tmp_mu_2*                                                                             &
                                 (-315101.d0 - 271525.d0*tmp_mu_2 + 2.177136d+6*tmp_mu_4 - 1.541178d+6*tmp_mu_6 +                     &
                                   284220.d0*tmp_mu_8))))))) -                                                                        &
              70.d0*tmp_c_2*(8744.d0 + tmp_mu_2*                                                                                      &
                  (121960.d0 + tmp_mu_2*(9.927611d+6 +                                                                                &
                       6.d0*tmp_mu_2*(3.4792175d+7 +                                                                                  &
                          tmp_mu_2*(1.67872201d+8 +                                                                                   &
                             4.d0*tmp_mu_2*(8.4718337d+7 +                                                                            &
                                15.d0*tmp_mu_2*                                                                                       &
                                 (8.631239d+6 +                                                                                       &
                                   3.d0*tmp_mu_2*                                                                                     &
                                    (-2.971644d+7 + 1.4760413d+7*tmp_mu_2 + 9.4463182d+7*tmp_mu_4 -                                   &
                                      6.6606504d+7*tmp_mu_6 + 1.102992d+7*tmp_mu_8)))))))) +                                          &
              tmp_mu_2*(-2.130534d+6 + 5.d0*tmp_mu_2*                                                                                 &
                  (-4.8151313d+7 + tmp_mu_2*                                                                                          &
                     (-1.331854738d+9 + 3.d0*tmp_mu_2*                                                                                &
                        (-2.846479687d+9 +                                                                                            &
                          2.d0*tmp_mu_2*(-4.629688747d+9 +                                                                            &
                             tmp_mu_2*(-2.7950463725d+10 +                                                                            &
                                12.d0*tmp_mu_2*                                                                                       &
                                 (2.502393951d+9 +                                                                                    &
                                   2.d0*tmp_mu_2*                                                                                     &
                                    (5.939208622d+9 - 9.448153699d+9*tmp_mu_2 - 4.219392723d+9*tmp_mu_4 +                             &
                                      2.164816512d+9*tmp_mu_6 + 5.560344d+7*tmp_mu_8))))))))) -                                       &
           1.d0*dexp(z3/tmp_mu_2)*                                                                                                  &
            (584417.d0 + 6300.d0*tmp_c_4*(363.d0 +                                                                                    &
                 tmp_mu_2*(726.d0 + tmp_mu_2*                                                                                         &
                     (77371.d0 + 2.d0*tmp_mu_2*                                                                                       &
                        (862357.d0 + 12.d0*tmp_mu_2*                                                                                  &
                           (122181.d0 + 4.d0*tmp_mu_2*                                                                                &
                              (57485.d0 + tmp_mu_2*                                                                                   &
                                 (3583.d0 +                                                                                           &
                                   6.d0*tmp_mu_2*                                                                                     &
                                    (-229669.d0 - 85164.d0*tmp_mu_2 + 535658.d0*tmp_mu_4 - 304233.d0*tmp_mu_6 +                       &
                                      47370.d0*tmp_mu_8)))))))) +                                                                     &
              210.d0*tmp_c_2*(11906.d0 + tmp_mu_2*                                                                                    &
                  (32185.d0 + 2.d0*tmp_mu_2*(1.796618d+6 +                                                                            &
                       tmp_mu_2*(6.5993831d+7 +                                                                                       &
                          12.d0*tmp_mu_2*(4.0116891d+7 +                                                                              &
                             2.d0*tmp_mu_2*(3.3802349d+7 +                                                                            &
                                4.d0*tmp_mu_2*                                                                                        &
                                 (2.9278334d+7 +                                                                                      &
                                   15.d0*tmp_mu_2*                                                                                    &
                                    (-3.419139d+6 - 2.5322678d+7*tmp_mu_2 + 6.333055d+6*tmp_mu_4 +                                    &
                                      3.3783636d+7*tmp_mu_6 - 1.9409106d+7*tmp_mu_8 + 2.75748d+6*tmp_mu_10))))))))                   &
               + tmp_mu_2*(2.035001d+6 - 24.d0*tmp_mu_2*                                                                              &
                  (-9.449101d+6 + 5.d0*tmp_mu_2*                                                                                      &
                     (-9.0936608d+7 + tmp_mu_2*                                                                                       &
                        (-9.00714773d+8 + 3.d0*tmp_mu_2*                                                                              &
                           (-8.35070569d+8 +                                                                                          &
                             24.d0*tmp_mu_2*                                                                                          &
                              (-1.74924872d+8 +                                                                                       &
                                tmp_mu_2*(-4.85635668d+8 +                                                                            &
                                   tmp_mu_2*                                                                                          &
                                    (1.838107009d+9 +                                                                                 &
                                      tmp_mu_2*                                                                                       &
                                       (2.661083593d+9 +                                                                              &
                                        8.d0*tmp_mu_2*                                                                                &
                                        (-3.43546795d+8 - 8.935971d+7*tmp_mu_2 + 4.3941939d+7*tmp_mu_4 +                              &
                                        1.158405d+6*tmp_mu_6))))))))))) +                                                             &
           5.317361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu*derf(z1/tmp_mu)*                                                          &
            (2160.d0*tmp_mu_8*(27200.d0 + 3.436944d+6*tmp_mu_2 +                                                                      &
                 tmp_mu_4*(5.9485936d+7 + 1.64924132d+8*tmp_mu_2 - 3.87808028d+8*tmp_mu_4 - 3.4035404d+7*tmp_mu_6 +                   &
                    2.530904115d+9*tmp_mu_8 - 6.89047145d+8*tmp_mu_10) -                                                              &
                 420.d0*tmp_c_4*(-208.d0 - 9348.d0*tmp_mu_2 - 13376.d0*tmp_mu_4 + 160330.d0*tmp_mu_6 - 419415.d0*tmp_mu_8 +           &
                    215492.d0*tmp_mu_10) - 35.d0*tmp_c_2*                                                                             &
                  (-3200.d0 - 238688.d0*tmp_mu_2 - 2.593952d+6*tmp_mu_4 + 2.116336d+6*tmp_mu_6 +                                      &
                    1.8574112d+7*tmp_mu_8 - 6.7310355d+7*tmp_mu_10 + 3.1450102d+7*tmp_mu_12)) +                                       &
              30.d0*dexp(z1/tmp_mu_2)*tmp_mu_4*                                                                                     &
               (-487680.d0 + 840.d0*tmp_c_4*(-1568.d0 +                                                                               &
                    tmp_mu_2*(-56136.d0 + tmp_mu_2*                                                                                   &
                        (-549634.d0 + 9.d0*tmp_mu_2*                                                                                  &
                           (-114337.d0 + 73108.d0*tmp_mu_2 + 2.199848d+6*tmp_mu_4 - 5.079256d+6*tmp_mu_6 +                            &
                             1.723936d+6*tmp_mu_8)))) +                                                                               &
                 tmp_mu_2*(-3.8230496d+7 +                                                                                            &
                    tmp_mu_2*(-8.33569312d+8 +                                                                                        &
                       tmp_mu_2*(-5.029510172d+9 +                                                                                    &
                          3.d0*tmp_mu_2*(-1.0783285216d+10 +                                                                          &
                             3.d0*tmp_mu_2*(-5.295192793d+9 + 2.9560295084d+10*tmp_mu_2 - 7.160940136d+9*tmp_mu_4 -                   &
                                5.151922016d+10*tmp_mu_6 + 1.102475432d+10*tmp_mu_8))))) +                                            &
                 84.d0*tmp_c_2*(-20672.d0 + tmp_mu_2*                                                                                 &
                     (-1.144328d+6 + tmp_mu_2*                                                                                        &
                        (-1.9941696d+7 + tmp_mu_2*                                                                                    &
                           (-1.03283384d+8 +                                                                                          &
                             5.d0*tmp_mu_2*(-5.7403003d+7 +                                                                           &
                                4.d0*tmp_mu_2*                                                                                        &
                                 (4.1051518d+7 + 8.7309294d+7*tmp_mu_2 - 2.96281371d+8*tmp_mu_4 +                                     &
                                   9.4350306d+7*tmp_mu_6)))))) +                                                                      &
                 127.6166772651972d0*tmp_mu_3*                                                                                        &
                  (27200.d0 + 3.461744d+6*tmp_mu_2 + 6.1919968d+7*tmp_mu_4 + 2.16617448d+8*tmp_mu_6 -                                 &
                    3.249299d+8*tmp_mu_8 - 9.09549095d+8*tmp_mu_10 + 1.678510925d+9*tmp_mu_12 +                                       &
                    420.d0*tmp_c_4*(208.d0 + 9532.d0*tmp_mu_2 + 19432.d0*tmp_mu_4 - 148397.d0*tmp_mu_6 +                              &
                       206255.d0*tmp_mu_8) +                                                                                          &
                    70.d0*tmp_c_2*(1600.d0 + 120784.d0*tmp_mu_2 + 1.375912d+6*tmp_mu_4 - 66954.d0*tmp_mu_6 -                          &
                       1.1481791d+7*tmp_mu_8 + 1.8126605d+7*tmp_mu_10))*derf(z1/tmp_mu)) +                                          &
              dexp(z2/tmp_mu_2)*                                                                                                    &
               (322352.d0 - 2100.d0*tmp_c_4*(-484.d0 +                                                                                &
                    tmp_mu_2*(-6935.d0 + 6.d0*tmp_mu_2*                                                                               &
                        (-60378.d0 + tmp_mu_2*                                                                                        &
                           (-734209.d0 + 2.d0*tmp_mu_2*                                                                               &
                              (-451456.d0 +                                                                                           &
                                9.d0*tmp_mu_2*                                                                                        &
                                 (-207729.d0 + 887064.d0*tmp_mu_2 + 2.420496d+6*tmp_mu_4 - 3.401596d+6*tmp_mu_6 +                     &
                                   861968.d0*tmp_mu_8)))))) -                                                                         &
                 70.d0*tmp_c_2*(-17488.d0 + tmp_mu_2*                                                                                 &
                     (-252664.d0 + 3.d0*tmp_mu_2*                                                                                     &
                        (-6.739917d+6 + 2.d0*tmp_mu_2*                                                                                &
                           (-7.1704624d+7 +                                                                                           &
                             tmp_mu_2*(-3.71974837d+8 +                                                                               &
                                3.d0*tmp_mu_2*                                                                                        &
                                 (-2.93982813d+8 +                                                                                    &
                                   20.d0*tmp_mu_2*                                                                                    &
                                    (-2.7297085d+7 + 2.14511177d+8*tmp_mu_2 + 2.13246558d+8*tmp_mu_4 -                                &
                                      3.90631677d+8*tmp_mu_6 + 9.4350306d+7*tmp_mu_8))))))) +                                         &
                 tmp_mu_2*(4.422244d+6 + 5.d0*tmp_mu_2*                                                                               &
                     (9.763766d+7 - 3.d0*tmp_mu_2*                                                                                    &
                        (-9.08401907d+8 + 3.d0*tmp_mu_2*                                                                              &
                           (-2.052668356d+9 +                                                                                         &
                             tmp_mu_2*(-7.326673971d+9 +                                                                              &
                                2.d0*tmp_mu_2*                                                                                        &
                                 (-2.1723419441d+10 +                                                                                 &
                                   3.d0*tmp_mu_2*                                                                                     &
                                    (4.296097885d+9 + 5.441057038d+10*tmp_mu_2 - 1.920846208d+9*tmp_mu_4 -                            &
                                      3.127198724d+10*tmp_mu_6 + 5.51237716d+9*tmp_mu_8))))))) -                                      &
                 212.6944621086619d0*tmp_mu_3*                                                                                        &
                  (60960.d0 + 4.841632d+6*tmp_mu_2 +                                                                                  &
                    420.d0*tmp_c_4*(392.d0 + 14404.d0*tmp_mu_2 + 145878.d0*tmp_mu_4 + 335382.d0*tmp_mu_6 +                            &
                       95517.d0*tmp_mu_8 - 6.383736d+6*tmp_mu_10 + 3.71259d+6*tmp_mu_12) +                                            &
                    42.d0*tmp_c_2*(5168.d0 + tmp_mu_2*                                                                                &
                        (291176.d0 + 5.d0*tmp_mu_2*                                                                                   &
                           (1.0308d+6 + 5.709832d+6*tmp_mu_2 + 1.8243958d+7*tmp_mu_4 - 3.3471963d+7*tmp_mu_6 -                        &
                             1.77650376d+8*tmp_mu_8 + 1.0875963d+8*tmp_mu_10))) +                                                     &
                    tmp_mu_4*(1.06965184d+8 +                                                                                         &
                       3.d0*tmp_mu_2*(2.28437d+8 +                                                                                    &
                          tmp_mu_2*(1.51864306d+9 +                                                                                   &
                             tmp_mu_2*(3.044150278d+9 +                                                                               &
                                15.d0*tmp_mu_2*(-8.64890837d+8 + 182.d0*tmp_mu_2*(-5.688044d+6 + 3.689035d+6*tmp_mu_2)))              &
                             ))))*derf(z1/tmp_mu) -                                                                                 &
                 2261.946710584651d0*tmp_mu_6*                                                                                        &
                  (-320.d0*(85.d0 + 350.d0*tmp_c_2 + 273.d0*tmp_c_4) -                                                                &
                    16.d0*(217909.d0 + 534730.d0*tmp_c_2 + 255045.d0*tmp_c_4)*tmp_mu_2 -                                              &
                    560.d0*(114950.d0 + 21.d0*tmp_c_2*(8666.d0 + 915.d0*tmp_c_2))*tmp_mu_4 +                                          &
                    8820.d0*(-30598.d0 - 7720.d0*tmp_c_2 + 6325.d0*tmp_c_4)*tmp_mu_6 +                                                &
                    2.0727d+6*(109.d0 + 440.d0*tmp_c_2)*tmp_mu_8 + 1.793050875d+9*tmp_mu_10)*derf(z1/tmp_mu)**2))))/                &
       (dexp(z1/tmp_mu_2)*tmp_mu_2) -                                                                                               &
      (44066.83460908053d0*kF*tmp_c_4*(7.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                &
               dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                   &
               21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))**2 -                       &
           8.d0*dexp(z1/tmp_mu_2)*                                                                                                  &
            (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                              &
              10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                         &
            (60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                          &
              dexp(z1/tmp_mu_2)*                                                                                                    &
               (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                           &
              53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*                          &
               derf(z1/tmp_mu)))*(3600.d0*tmp_mu_8*                                                                                 &
            (-40.d0*(34.d0 + 4283.d0*tmp_mu_2) +                                                                                      &
              84.d0*tmp_c_4*(-52.d0 - 2278.d0*tmp_mu_2 - 1241.d0*tmp_mu_4 + 46744.d0*tmp_mu_6 - 163188.d0*tmp_mu_8 +                  &
                 94740.d0*tmp_mu_10) + 7.d0*tmp_c_2*                                                                                  &
               (-800.d0 - 59064.d0*tmp_mu_2 - 607806.d0*tmp_mu_4 + 1.238083d+6*tmp_mu_6 + 4.233018d+6*tmp_mu_8 -                      &
                 2.6642736d+7*tmp_mu_10 + 1.330368d+7*tmp_mu_12) +                                                                    &
              tmp_mu_4*(-2.929678d+6 + 3.d0*tmp_mu_2*                                                                                 &
                  (-2.134269d+6 + 9.680447d+6*tmp_mu_2 - 1.2378083d+7*tmp_mu_4 - 8.8101504d+7*tmp_mu_6 +                              &
                    4.652352d+7*tmp_mu_8))) -                                                                                         &
           90.d0*dexp(z1/tmp_mu_2)*tmp_mu_4*                                                                                        &
            (-8.d0*(2540.d0 + 193693.d0*tmp_mu_2) +                                                                                   &
              280.d0*tmp_c_4*(-196.d0 + tmp_mu_2*                                                                                     &
                  (-6315.d0 + 2.d0*tmp_mu_2*(-20497.d0 +                                                                              &
                       6.d0*tmp_mu_2*(132.d0 - 2573.d0*tmp_mu_2 + 190244.d0*tmp_mu_4 - 515856.d0*tmp_mu_6 +                           &
                          189480.d0*tmp_mu_8)))) +                                                                                    &
              tmp_mu_4*(-3.0203038d+7 + tmp_mu_2*                                                                                     &
                  (-1.12404623d+8 + 20.d0*tmp_mu_2*                                                                                   &
                     (-3.5057434d+7 + 3.d0*tmp_mu_2*                                                                                  &
                        (-1.8630495d+7 + 1.65465892d+8*tmp_mu_2 - 7.9315526d+7*tmp_mu_4 - 5.38500096d+8*tmp_mu_6 +                    &
                          1.8609408d+8*tmp_mu_8)))) +                                                                                 &
              28.d0*tmp_c_2*(-2584.d0 + tmp_mu_2*                                                                                     &
                  (-135348.d0 + tmp_mu_2*(-1.969613d+6 +                                                                              &
                       5.d0*tmp_mu_2*(-1.059055d+6 +                                                                                  &
                          2.d0*tmp_mu_2*(-1.660448d+6 + 8.864969d+6*tmp_mu_2 + 1.6220058d+7*tmp_mu_4 -                                &
                             7.9892832d+7*tmp_mu_6 + 2.660736d+7*tmp_mu_8)))))) +                                                     &
           4.d0*dexp(z2/tmp_mu_2)*                                                                                                  &
            (-20147.d0 + 240.d0*tmp_mu_4*(-32445.d0 +                                                                                 &
                 tmp_mu_2*(-1.432235d+6 + 6.d0*tmp_mu_2*                                                                              &
                     (-1.703695d+6 + 2.d0*tmp_mu_2*                                                                                   &
                        (-479931.d0 + 5.d0*tmp_mu_2*                                                                                  &
                           (-2.784577d+6 + 3.000585d+6*tmp_mu_2 + 3.3010425d+7*tmp_mu_4 + 3.60822d+6*tmp_mu_6 -                       &
                             3.3965352d+7*tmp_mu_8 + 8.72316d+6*tmp_mu_10))))) +                                                      &
              525.d0*tmp_c_4*(-121.d0 + tmp_mu_2*                                                                                     &
                  (121.d0 + 24.d0*tmp_mu_2*(-1083.d0 +                                                                                &
                       2.d0*tmp_mu_2*(-10351.d0 +                                                                                     &
                          tmp_mu_2*(11953.d0 +                                                                                        &
                             12.d0*tmp_mu_2*                                                                                          &
                              (-3422.d0 + 16275.d0*tmp_mu_2 + 59560.d0*tmp_mu_4 - 88167.d0*tmp_mu_6 + 23685.d0*tmp_mu_8))))           &
                    )) + 35.d0*tmp_c_2*(-2186.d0 +                                                                                    &
                 tmp_mu_2*(1093.d0 + 24.d0*tmp_mu_2*                                                                                  &
                     (-26970.d0 + tmp_mu_2*(-872675.d0 +                                                                              &
                          4.d0*tmp_mu_2*(-1.017149d+6 +                                                                               &
                             15.d0*tmp_mu_2*                                                                                          &
                              (37671.d0 - 448576.d0*tmp_mu_2 + 3.31445d+6*tmp_mu_4 + 3.16134d+6*tmp_mu_6 -                            &
                                6.656262d+6*tmp_mu_8 + 1.66296d+6*tmp_mu_10))))))) +                                                  &
           79.76042329074822d0*dexp(z1/tmp_mu_2)*tmp_mu_3*derf(z1/tmp_mu)*                                                        &
            (40.d0*tmp_mu_4*(-160.d0*(34.d0 + 4321.d0*tmp_mu_2) -                                                                     &
                 84.d0*tmp_c_4*(208.d0 + 9336.d0*tmp_mu_2 + 11426.d0*tmp_mu_4 - 180045.d0*tmp_mu_6 + 324360.d0*tmp_mu_8) -            &
                 3.d0*tmp_mu_4*(4.091644d+6 + 1.2674668d+7*tmp_mu_2 - 3.8216237d+7*tmp_mu_4 - 6.3015855d+7*tmp_mu_6 +                 &
                    2.37628125d+8*tmp_mu_8) -                                                                                         &
                 7.d0*tmp_c_2*(3200.d0 + 239776.d0*tmp_mu_2 + 2.605652d+6*tmp_mu_4 - 2.760948d+6*tmp_mu_6 -                           &
                    2.6320425d+7*tmp_mu_8 + 5.883513d+7*tmp_mu_10)) +                                                                 &
              dexp(z1/tmp_mu_2)*                                                                                                    &
               (40640.d0 + 280.d0*tmp_c_4*(392.d0 + 13174.d0*tmp_mu_2 + 89655.d0*tmp_mu_4 + 31770.d0*tmp_mu_6 +                       &
                    186612.d0*tmp_mu_8 - 6.05286d+6*tmp_mu_10 + 3.89232d+6*tmp_mu_12) +                                               &
                 28.d0*tmp_c_2*(5168.d0 + 5.d0*tmp_mu_2*                                                                              &
                     (55660.d0 + 821754.d0*tmp_mu_2 + 2.515625d+6*tmp_mu_4 + 8.546585d+6*tmp_mu_6 -                                   &
                       3.0755916d+7*tmp_mu_8 - 1.7031111d+8*tmp_mu_10 + 1.1767026d+8*tmp_mu_12)) +                                    &
                 tmp_mu_2*(3.162528d+6 + 5.d0*tmp_mu_2*                                                                               &
                     (1.2473748d+7 + tmp_mu_2*                                                                                        &
                        (5.126422d+7 + 3.d0*tmp_mu_2*                                                                                 &
                           (1.07186415d+8 +                                                                                           &
                             4.d0*tmp_mu_2*(6.289758d+7 - 4.38217639d+8*tmp_mu_2 - 6.0128796d+8*tmp_mu_4 +                            &
                                4.7525625d+8*tmp_mu_6))))) +                                                                          &
                 70.89815403622064d0*tmp_mu_3*                                                                                        &
                  (-32.d0*(85.d0 + 350.d0*tmp_c_2 + 273.d0*tmp_c_4) -                                                                 &
                    16.d0*(21795.d0 + 53221.d0*tmp_c_2 + 25095.d0*tmp_c_4)*tmp_mu_2 -                                                 &
                    504.d0*(12734.d0 + 19325.d0*tmp_c_2 + 1505.d0*tmp_c_4)*tmp_mu_4 +                                                 &
                    1260.d0*(-20166.d0 + 1267.d0*tmp_c_2 + 5635.d0*tmp_c_4)*tmp_mu_6 +                                                &
                    110250.d0*(471.d0 + 1085.d0*tmp_c_2)*tmp_mu_8 + 2.72041875d+8*tmp_mu_10)*derf(z1/tmp_mu))))*                    &
         (6300.d0*(-3.d0 + 2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                         &
              7.089815403622064d0*tmp_mu*derf(z1/tmp_mu)) +                                                                         &
           (2100.d0*(1.d0 + (18.d0 - 6.d0/dexp(z1/tmp_mu_2))*tmp_mu_4 +                                                             &
                12.d0*(-1.d0 + dexp(-1.d0/tmp_mu_2))*tmp_mu_6 -                                                                       &
                10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu) -                                                                       &
                (0.4d0*(-2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) +                                                                           &
                     dexp(z1/tmp_mu_2)*                                                                                             &
                      (-3.d0 - 6.d0*tmp_mu_2 + 2.d0*tmp_mu_4 + 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu)))*                        &
                   (-3.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                  &
                        dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                          &
                        21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*                                     &
                         derf(z1/tmp_mu))*(-60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +         &
                        dexp(z1/tmp_mu_2)*                                                                                          &
                         (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) +                &
                        53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                             &
                         (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)) -                                            &
                     50.d0*dexp(z1/tmp_mu_2)*                                                                                       &
                      (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 +                                                                   &
                           (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                             &
                        10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                               &
                      (30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 -                             &
                           19680.d0*tmp_mu_8) +                                                                                       &
                        dexp(z1/tmp_mu_2)*                                                                                          &
                         (-149.d0 + 180.d0*tmp_mu_4*                                                                                  &
                            (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8)) +                  &
                        26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                             &
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                 &
                 (dexp(z1/tmp_mu_2)*                                                                                                &
                   (7.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                   &
                         dexp(z1/tmp_mu_2)*                                                                                         &
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                            &
                         21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*                                    &
                          derf(z1/tmp_mu))**2 +                                                                                     &
                     8.d0*dexp(z1/tmp_mu_2)*                                                                                        &
                      (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 +                                                                   &
                           (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                             &
                        10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                               &
                      (-60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                               &
                        dexp(z1/tmp_mu_2)*                                                                                          &
                         (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) +                &
                        53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                             &
                         (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))))/tmp_c_2 +                                &
           (735.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                         &
                  dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                &
                  21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))**3                      &
                + 14000.d0*dexp(z2/tmp_mu_2)*                                                                                       &
               (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                           &
                  10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))**2*                                                                  &
               (30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +               &
                 dexp(z1/tmp_mu_2)*                                                                                                 &
                  (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      &
                       3280.d0*tmp_mu_8)) +                                                                                           &
                 26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                    &
                  (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)) +                         &
              144.d0*(-60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                &
                  dexp(z1/tmp_mu_2)*                                                                                                &
                   (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) +                      &
                  53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                   &
                   (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))**2*                                                &
               (-2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) +                                                                                   &
                 dexp(z1/tmp_mu_2)*                                                                                                 &
                  (-3.d0 - 6.d0*tmp_mu_2 + 2.d0*tmp_mu_4 + 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))) -                           &
              420.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                       &
                 dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                 &
                 21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))*                         &
               (4.d0*dexp(z1/tmp_mu_2)*                                                                                             &
                  (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                        &
                    10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                   &
                  (60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                    &
                    dexp(z1/tmp_mu_2)*                                                                                              &
                     (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                     &
                    53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                 &
                     (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)) -                                                &
                 5.d0*(30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        &
                    dexp(z1/tmp_mu_2)*                                                                                              &
                     (-149.d0 + 180.d0*tmp_mu_4*                                                                                      &
                        (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8)) +                      &
                    26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                 &
                     (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))*                       &
                  (2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) +                                                                                 &
                    dexp(z1/tmp_mu_2)*                                                                                              &
                     (3.d0 + 6.d0*tmp_mu_2 - 2.d0*tmp_mu_4 - 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu)))))/                        &
            (dexp(z1/tmp_mu_2)*tmp_c_4*                                                                                             &
              (7.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                        &
                    dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                              &
                    21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))                      &
                   **2 + 8.d0*dexp(z1/tmp_mu_2)*                                                                                    &
                 (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                         &
                   10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    &
                 (-60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                    &
                   dexp(z1/tmp_mu_2)*                                                                                               &
                    (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) +                     &
                   53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                  &
                    (50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))))/tmp_mu_2))/                                   &
  (12.d0*(60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                             &
         dexp(z1/tmp_mu_2)*(163.d0 -                                                                                                &
            120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                                        &
         53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*                               &
          derf(z1/tmp_mu))**2 - 175.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                   &
        dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                          &
        21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))*                                  &
      (30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                        &
        dexp(z1/tmp_mu_2)*(-149.d0 +                                                                                                &
           180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8)) +                   &
        26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                             &
         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)) +                                  &
     525.d0*tmp_c_4*(7.d0*(24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                  &
            dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                      &
            21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))**2 -                          &
        8.d0*dexp(z1/tmp_mu_2)*                                                                                                     &
         (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                 &
           10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                            &
         (60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                             &
           dexp(z1/tmp_mu_2)*                                                                                                       &
            (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                              &
           53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*                             &
            derf(z1/tmp_mu))) - 70.d0*tmp_c_2*(3.d0*                                                                                &
         (24.d0*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                                   &
           dexp(z1/tmp_mu_2)*(-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8) +                                       &
           21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))*                               &
         (60.d0*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) +                                             &
           dexp(z1/tmp_mu_2)*                                                                                                       &
            (163.d0 - 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6)) -                              &
           53.17361552716548d0*dexp(z1/tmp_mu_2)*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*                             &
            derf(z1/tmp_mu)) - 50.d0*dexp(z1/tmp_mu_2)*                                                                           &
         (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                 &
           10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                            &
         (30.d0*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                     &
           dexp(z1/tmp_mu_2)*                                                                                                       &
            (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8))        &
            + 26.58680776358274d0*dexp(z1/tmp_mu_2)*tmp_mu_3*                                                                       &
            (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))**2                             

    elseif (dirac_approximant == "dirac_pade_order_6") then

    !e_x =    (0.03377372788077926d0*kF**3*(0.07957747154594767d0*kF*                                                                     & 
     e_x =    (0.03377372788077926d0*kF_HF_3*(0.07957747154594767d0*kF*                                                                     & 
             (-3.d0 + 2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                    & 
               7.089815403622064d0*tmp_mu*derf(z1/tmp_mu)) +                                                                              & 
            (0.02652582384864922d0*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                              & 
                   (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                   & 
                  10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu)) +                                                                          & 
               (0.07957747154594767d0*kF*(-3.d0 + 2.d0*tmp_mu_2*                                                                          & 
                     (-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                   & 
                    7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                          & 
                  ((-3.627615631996127d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -            & 
                          1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                      & 
                           (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                   & 
                             53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**2*              & 
                       (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu))))/dexp(z3/tmp_mu_2) +                                                                       & 
                    (5.290272796661019d-12*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                  & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                 & 
                            21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                       & 
                       (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-149.d0 + 180.d0*tmp_mu_4*                                                                                    & 
                              (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                     & 
                             26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*         & 
                              derf(z1/tmp_mu)))**2)/dexp(z3/tmp_mu_2) +                                                                   & 
                    (8.24458098180938d-14*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                   & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                 & 
                            21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                       & 
                       (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                  & 
                       (630.d0*kF*tmp_mu_4*(-196.d0 +                                                                                     & 
                            tmp_mu_2*(-8680.d0 + 3.d0*tmp_mu_2*                                                                           & 
                                (-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                            & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                              & 
                                (-693.d0 + 2.d0*tmp_mu_2*                                                                                 & 
                                   (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))      & 
                                  )) + 558.3229630352375d0*tmp_mu_3*                                                                      & 
                             (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*         & 
                             derf(z1/tmp_mu))))/dexp(z3/tmp_mu_2) -                                                                       & 
                    (1.374096830301563d-12*kF*(1.d0 +                                                                                     & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu)))*(630.d0*kF*tmp_mu_4*                                                                       & 
                          (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                 & 
                               3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -               & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                              & 
                                (-693.d0 + 2.d0*tmp_mu_2*                                                                                 & 
                                   (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))      & 
                                  )) + 558.3229630352375d0*tmp_mu_3*                                                                      & 
                             (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*         & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) +                                                                       & 
                    (1.104138521838546d-6*(-126.d0*kF*tmp_mu_4*                                                                           & 
                          (-704.d0 + tmp_mu_2*(-47072.d0 +                                                                                & 
                               5.d0*tmp_mu_2*(-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 -                                 & 
                                  2.5593024d+7*tmp_mu_6 + 9.989952d+6*tmp_mu_8))) +                                                       & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-5549.d0 + 504.d0*tmp_mu_4*                                                                                    & 
                             (-3003.d0 + 4.d0*tmp_mu_2*                                                                                   & 
                                (-32032.d0 + 3.d0*tmp_mu_2*                                                                               & 
                                   (-207207.d0 + 20.d0*tmp_mu_2*                                                                          & 
                                      (-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +                      & 
                            111.6645926070475d0*tmp_mu_3*                                                                                 & 
                             (1408.d0 + 455.d0*tmp_mu_2*                                                                                  & 
                                (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*           & 
                             derf(z1/tmp_mu)))*((-1.75904832712392d-6*                                                                    & 
                            (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                 & 
                               dexp(z1/tmp_mu_2)*kF*                                                                                      & 
                                (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                           & 
                                  21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                             & 
                          dexp(z2/tmp_mu_2) +                                                                                             & 
                         (2.01034094528448d-6*kF*                                                                                         & 
                            (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 +                                                                 & 
                                 (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                             & 
                              10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                               & 
                            (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                           & 
                              1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                  & 
                               (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +               & 
                                 53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/            & 
                          dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2)))/                                                                        & 
                ((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -               & 
                        1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                        & 
                         (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                     & 
                           53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/               & 
                   dexp(z3/tmp_mu_2) -                                                                                                    & 
                  (1.269665471198645d-11*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                    & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                   & 
                          21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                         & 
                     (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                  & 
                       1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                        (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                      & 
                          53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                    & 
                     (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +          & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                    & 
                             3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                            & 
                           (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                     & 
                   dexp(z3/tmp_mu_2) +                                                                                                    & 
                  (1.058054559332204d-10*kF*(1.d0 +                                                                                       & 
                       6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                 & 
                       10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                      & 
                     (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +          & 
                        dexp(z1/tmp_mu_2)*kF*                                                                                             & 
                         (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                   & 
                              3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                           & 
                            (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                 & 
                   dexp(z2/tmp_mu_2) +                                                                                                    & 
                  (8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                              & 
                        (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                   & 
                             3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                 & 
                       1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                        (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                & 
                              (-693.d0 + 2.d0*tmp_mu_2*                                                                                   & 
                                 (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))      & 
                            + 558.3229630352375d0*tmp_mu_3*                                                                               & 
                           (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*           & 
                           derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                       & 
                          (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                   & 
                             dexp(z1/tmp_mu_2)*kF*                                                                                        & 
                              (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                             & 
                                21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                               & 
                        dexp(z2/tmp_mu_2) -                                                                                               & 
                       (2.01034094528448d-6*kF*(1.d0 +                                                                                    & 
                            6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                            & 
                            10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                 & 
                          (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                             & 
                            1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                    & 
                             (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                 & 
                               53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/              & 
                        dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2)))/tmp_c_2 +                                                                 & 
            ((3.299159768397792d-17*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                   & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**4)/                  & 
                dexp(4.d0/tmp_mu_2) +                                                                                                     & 
               (7.016442215776554d-15*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                     & 
                        21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2*                                        & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                    & 
                dexp(4.d0/tmp_mu_2) -                                                                                                     & 
               (2.004697775936158d-14*kF*(-3.d0 + 2.d0*tmp_mu_2*                                                                          & 
                     (-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                   & 
                    7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                          & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**3)/                    & 
                dexp(z3/tmp_mu_2) -                                                                                                       & 
               (3.644905047156651d-15*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                           & 
                     (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                 & 
                    10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                         & 
                  (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                           & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                         & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                        (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu))))/dexp(z3/tmp_mu_2) +                                                                            & 
               (4.733642918385261d-16*kF**2*(1.d0 +                                                                                       & 
                     6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                   & 
                     10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))**2*                                                                     & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                         (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -         & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                  & 
                            (-693.d0 + 2.d0*tmp_mu_2*                                                                                     & 
                               (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))       & 
                         + 558.3229630352375d0*tmp_mu_3*                                                                                  & 
                         (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*             & 
                         derf(z1/tmp_mu)))**2)/dexp(z2/tmp_mu_2) -                                                                        & 
               (7.100464377577892d-17*kF*(-3.d0 + 2.d0*tmp_mu_2*                                                                          & 
                     (-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                   & 
                    7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                          & 
                  (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                           & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                         (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -         & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                  & 
                            (-693.d0 + 2.d0*tmp_mu_2*                                                                                     & 
                               (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))       & 
                         + 558.3229630352375d0*tmp_mu_3*                                                                                  & 
                         (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*             & 
                         derf(z1/tmp_mu)))**2)/dexp(z3/tmp_mu_2) -                                                                        & 
               (5.74383127224137d-9*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                   & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**2*                   & 
                  ((2.512926181605599d-7*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                    & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                 & 
                            21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                       & 
                       (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) +                                                                       & 
                    (8.786455180439159d-8*kF*(-3.d0 +                                                                                     & 
                         2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                 & 
                         7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                     & 
                       (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                    & 
                            tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                          & 
                                (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                      & 
                                  9.989952d+6*tmp_mu_8))) +                                                                               & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-5549.d0 + 504.d0*tmp_mu_4*                                                                                    & 
                             (-3003.d0 + 4.d0*tmp_mu_2*                                                                                   & 
                                (-32032.d0 + 3.d0*tmp_mu_2*                                                                               & 
                                   (-207207.d0 + 20.d0*tmp_mu_2*                                                                          & 
                                      (-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +                      & 
                            111.6645926070475d0*tmp_mu_3*                                                                                 & 
                             (1408.d0 + 455.d0*tmp_mu_2*                                                                                  & 
                                (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*           & 
                             derf(z1/tmp_mu))))/dexp(z1/tmp_mu_2) +                                                                       & 
                    (4.351387327455584d-8*kF*(1.d0 +                                                                                      & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (630.d0*kF*tmp_mu_4*(-196.d0 +                                                                                     & 
                            tmp_mu_2*(-8680.d0 + 3.d0*tmp_mu_2*                                                                           & 
                                (-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                            & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                              & 
                                (-693.d0 + 2.d0*tmp_mu_2*                                                                                 & 
                                   (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))      & 
                                  )) + 558.3229630352375d0*tmp_mu_3*                                                                      & 
                             (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*         & 
                             derf(z1/tmp_mu))))/dexp(z1/tmp_mu_2)))/dexp(z2/tmp_mu_2) -                                                   & 
               (1.104138521838546d-6*(-126.d0*kF*tmp_mu_4*                                                                                & 
                     (-704.d0 + tmp_mu_2*(-47072.d0 +                                                                                     & 
                          5.d0*tmp_mu_2*(-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +              & 
                             9.989952d+6*tmp_mu_8))) +                                                                                    & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-5549.d0 + 504.d0*tmp_mu_4*(-3003.d0 +                                                                              & 
                          4.d0*tmp_mu_2*(-32032.d0 +                                                                                      & 
                             3.d0*tmp_mu_2*(-207207.d0 +                                                                                  & 
                                20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +             & 
                       111.6645926070475d0*tmp_mu_3*                                                                                      & 
                        (1408.d0 + 455.d0*tmp_mu_2*(216.d0 +                                                                              & 
                             11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*derf(z1/tmp_mu)))*      & 
                  ((2.333010303327509d-9*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                    & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                & 
                             21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**3)/                                  & 
                     dexp(z3/tmp_mu_2) +                                                                                                  & 
                    (0.00006315672344916482d0*(30.d0*kF*tmp_mu_4*                                                                         & 
                          (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu)))*(0.0007036193308495679d0*kF**2*                                                            & 
                          (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                      & 
                             10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))**2 -                                                            & 
                         (0.0001055428996274352d0*kF*                                                                                     & 
                            (-3.d0 + 2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 -                                                                    & 
                                 (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                           & 
                              7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                & 
                            (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                 & 
                              dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                               (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                            & 
                                 21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu))))/                                 & 
                          dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2)))/                                                                        & 
                dexp(z1/tmp_mu_2) +                                                                                                       & 
               (0.0001515761362779956d0*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -               & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  ((8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                             & 
                          (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                 & 
                               3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -               & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                              & 
                                (-693.d0 + 2.d0*tmp_mu_2*                                                                                 & 
                                   (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))      & 
                                  )) + 558.3229630352375d0*tmp_mu_3*                                                                      & 
                             (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*         & 
                             derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                     & 
                            (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                 & 
                               dexp(z1/tmp_mu_2)*kF*                                                                                      & 
                                (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                           & 
                                  21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                             & 
                          dexp(z2/tmp_mu_2) +                                                                                             & 
                         (5.025852363211199d-6*kF*                                                                                        & 
                            (-3.d0 + 2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 -                                                                    & 
                                 (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                           & 
                              7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                & 
                            (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 -                        & 
                                 19680.d0*tmp_mu_8) +                                                                                     & 
                              dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                               (-149.d0 + 180.d0*tmp_mu_4*                                                                                & 
                                  (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                 & 
                                 26.58680776358274d0*tmp_mu_3*                                                                            & 
                                  (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/              & 
                          dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2) +                                                                         & 
                    0.02652582384864922d0*kF*(1.d0 +                                                                                      & 
                       6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                 & 
                       10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                      & 
                     ((3.988771716834285d-9*(30.d0*kF*tmp_mu_4*                                                                           & 
                              (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                    & 
                             dexp(z1/tmp_mu_2)*kF*                                                                                        & 
                              (-149.d0 + 180.d0*tmp_mu_4*                                                                                 & 
                                 (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                  & 
                                26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*      & 
                                 derf(z1/tmp_mu)))**2)/dexp(z2/tmp_mu_2) +                                                                & 
                       (1.46440919673986d-9*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                 & 
                            dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                             (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                              & 
                               21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                    & 
                          (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                 & 
                               tmp_mu_2*(-47072.d0 +                                                                                      & 
                                  5.d0*tmp_mu_2*(-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 -                              & 
                                     2.5593024d+7*tmp_mu_6 + 9.989952d+6*tmp_mu_8))) +                                                    & 
                            dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                             (-5549.d0 + 504.d0*tmp_mu_4*                                                                                 & 
                                (-3003.d0 + 4.d0*tmp_mu_2*                                                                                & 
                                   (-32032.d0 + 3.d0*tmp_mu_2*                                                                            & 
                                      (-207207.d0 +                                                                                       & 
                                        20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))        & 
                                  ) + 111.6645926070475d0*tmp_mu_3*                                                                       & 
                                (1408.d0 + 455.d0*tmp_mu_2*                                                                               & 
                                   (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*        & 
                                derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2))))/dexp(z1/tmp_mu_2))/                                               & 
             (tmp_c_6*((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -         & 
                       1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                        (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                      & 
                          53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/                & 
                  dexp(z3/tmp_mu_2) -                                                                                                     & 
                 (1.269665471198645d-11*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                     & 
                      dexp(z1/tmp_mu_2)*kF*                                                                                               & 
                       (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                    & 
                         21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                          & 
                    (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                   & 
                      1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                          & 
                       (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                       & 
                         53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                     & 
                    (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +           & 
                      dexp(z1/tmp_mu_2)*kF*                                                                                               & 
                       (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                     & 
                            3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                             & 
                          (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                      & 
                  dexp(z3/tmp_mu_2) +                                                                                                     & 
                 (1.058054559332204d-10*kF*(1.d0 +                                                                                        & 
                      6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                  & 
                      10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                       & 
                    (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +           & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                    & 
                             3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                            & 
                           (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                  & 
                  dexp(z2/tmp_mu_2) +                                                                                                     & 
                 (8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                               & 
                       (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                    & 
                            3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                  & 
                      1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                          & 
                       (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                 & 
                             (-693.d0 + 2.d0*tmp_mu_2*                                                                                    & 
                                (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))      & 
                          + 558.3229630352375d0*tmp_mu_3*                                                                                 & 
                          (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*            & 
                          derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                        & 
                         (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                    & 
                            dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                             (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                              & 
                               21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                                & 
                       dexp(z2/tmp_mu_2) -                                                                                                & 
                      (2.01034094528448d-6*kF*(1.d0 +                                                                                     & 
                           6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                             & 
                           10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                  & 
                         (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                              & 
                           1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                     & 
                            (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                  & 
                              53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/               & 
                       dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2))) +                                                                          & 
            ((1.913575149757242d-15*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                         & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                     & 
                        21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**3*                                        & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                        (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu))))/dexp(4.d0/tmp_mu_2) -                                                                          & 
               (1.75904832712392d-6*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                         & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                     & 
                        21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2*                                        & 
                  ((9.573052120402284d-9*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -              & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                  & 
                       (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) +                                                                       & 
                    (2.92881839347972d-8*kF*(1.d0 +                                                                                       & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                    & 
                            tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                          & 
                                (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                      & 
                                  9.989952d+6*tmp_mu_8))) +                                                                               & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-5549.d0 + 504.d0*tmp_mu_4*                                                                                    & 
                             (-3003.d0 + 4.d0*tmp_mu_2*                                                                                   & 
                                (-32032.d0 + 3.d0*tmp_mu_2*                                                                               & 
                                   (-207207.d0 + 20.d0*tmp_mu_2*                                                                          & 
                                      (-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +                      & 
                            111.6645926070475d0*tmp_mu_3*                                                                                 & 
                             (1408.d0 + 455.d0*tmp_mu_2*                                                                                  & 
                                (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*           & 
                             derf(z1/tmp_mu))))/dexp(z1/tmp_mu_2)))/dexp(z2/tmp_mu_2) +                                                   & 
               (6.031022835853439d-6*kF*(-3.d0 + 2.d0*tmp_mu_2*                                                                           & 
                     (-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                   & 
                    7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                          & 
                  (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                     & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  ((3.988771716834285d-9*(30.d0*kF*tmp_mu_4*                                                                              & 
                           (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                       & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-149.d0 + 180.d0*tmp_mu_4*                                                                                    & 
                              (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                     & 
                             26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*         & 
                              derf(z1/tmp_mu)))**2)/dexp(z2/tmp_mu_2) -                                                                   & 
                    (6.216267610650834d-11*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -            & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                  & 
                       (630.d0*kF*tmp_mu_4*(-196.d0 +                                                                                     & 
                            tmp_mu_2*(-8680.d0 + 3.d0*tmp_mu_2*                                                                           & 
                                (-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                            & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                              & 
                                (-693.d0 + 2.d0*tmp_mu_2*                                                                                 & 
                                   (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))      & 
                                  )) + 558.3229630352375d0*tmp_mu_3*                                                                      & 
                             (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*         & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2)))/dexp(z1/tmp_mu_2) +                                                   & 
               0.0007036193308495679d0*kF**2*(1.d0 +                                                                                      & 
                   6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                     & 
                   10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))**2*                                                                       & 
                ((8.368052552799199d-11*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -               & 
                       1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                        (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                      & 
                          53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                    & 
                     (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                      & 
                          tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                            & 
                              (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                        & 
                                9.989952d+6*tmp_mu_8))) +                                                                                 & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-5549.d0 + 504.d0*tmp_mu_4*(-3003.d0 +                                                                           & 
                             4.d0*tmp_mu_2*(-32032.d0 +                                                                                   & 
                                3.d0*tmp_mu_2*(-207207.d0 +                                                                               & 
                                   20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +          & 
                          111.6645926070475d0*tmp_mu_3*                                                                                   & 
                           (1408.d0 + 455.d0*tmp_mu_2*                                                                                    & 
                              (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*             & 
                           derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) -                                                                         & 
                  (5.180223008875695d-11*(30.d0*kF*tmp_mu_4*                                                                              & 
                        (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                          & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                    & 
                             3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                            & 
                           (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                      & 
                     (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                             & 
                           (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -       & 
                       1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                        (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                & 
                              (-693.d0 + 2.d0*tmp_mu_2*                                                                                   & 
                                 (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))      & 
                            + 558.3229630352375d0*tmp_mu_3*                                                                               & 
                           (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*           & 
                           derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2)) -                                                                        & 
               0.02652582384864922d0*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                            & 
                   (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                   & 
                  10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                           & 
                ((-5.353624014161935d-14*kF*(-3.d0 +                                                                                      & 
                       2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                   & 
                       7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                       & 
                     (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                             & 
                            (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6)))       & 
                         - 1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                     & 
                         (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                               & 
                               (-693.d0 + 2.d0*tmp_mu_2*                                                                                  & 
                                  (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))      & 
                              ) + 558.3229630352375d0*tmp_mu_3*                                                                           & 
                            (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*          & 
                            derf(z1/tmp_mu)))**2)/dexp(z2/tmp_mu_2) +                                                                     & 
                  (0.00006315672344916482d0*(30.d0*kF*tmp_mu_4*                                                                           & 
                        (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                          & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                    & 
                             3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                            & 
                           (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                      & 
                     ((5.74383127224137d-9*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -            & 
                             1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                   & 
                              (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                & 
                                53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**2)/          & 
                        dexp(z2/tmp_mu_2) +                                                                                               & 
                       (8.786455180439159d-8*kF*(-3.d0 +                                                                                  & 
                            2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                              & 
                            7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                  & 
                          (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                 & 
                               tmp_mu_2*(-47072.d0 +                                                                                      & 
                                  5.d0*tmp_mu_2*(-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 -                              & 
                                     2.5593024d+7*tmp_mu_6 + 9.989952d+6*tmp_mu_8))) +                                                    & 
                            dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                             (-5549.d0 + 504.d0*tmp_mu_4*                                                                                 & 
                                (-3003.d0 + 4.d0*tmp_mu_2*                                                                                & 
                                   (-32032.d0 + 3.d0*tmp_mu_2*                                                                            & 
                                      (-207207.d0 +                                                                                       & 
                                        20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))        & 
                                  ) + 111.6645926070475d0*tmp_mu_3*                                                                       & 
                                (1408.d0 + 455.d0*tmp_mu_2*                                                                               & 
                                   (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*        & 
                                derf(z1/tmp_mu))))/dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2)) +                                               & 
               (0.001326291192432461d0*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                      & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  ((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -             & 
                          1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                      & 
                           (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                   & 
                             53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/             & 
                     dexp(z3/tmp_mu_2) +                                                                                                  & 
                    (6.65908463915373d-12*kF*(-3.d0 +                                                                                     & 
                         2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 - (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                 & 
                         7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                     & 
                       (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                  & 
                       (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                    & 
                            tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                          & 
                                (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                      & 
                                  9.989952d+6*tmp_mu_8))) +                                                                               & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-5549.d0 + 504.d0*tmp_mu_4*                                                                                    & 
                             (-3003.d0 + 4.d0*tmp_mu_2*                                                                                   & 
                                (-32032.d0 + 3.d0*tmp_mu_2*                                                                               & 
                                   (-207207.d0 + 20.d0*tmp_mu_2*                                                                          & 
                                      (-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +                      & 
                            111.6645926070475d0*tmp_mu_3*                                                                                 & 
                             (1408.d0 + 455.d0*tmp_mu_2*                                                                                  & 
                                (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*           & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) +                                                                       & 
                    (0.00006315672344916482d0*(30.d0*kF*tmp_mu_4*                                                                         & 
                          (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu)))*((3.350568242140799d-6*kF*                                                                 & 
                            (1.d0 + 6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 +                                                                 & 
                                 (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                             & 
                              10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                               & 
                            (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 -                        & 
                                 19680.d0*tmp_mu_8) +                                                                                     & 
                              dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                               (-149.d0 + 180.d0*tmp_mu_4*                                                                                & 
                                  (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                 & 
                                 26.58680776358274d0*tmp_mu_3*                                                                            & 
                                  (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/              & 
                          dexp(z1/tmp_mu_2) -                                                                                             & 
                         (6.527080991183375d-8*kF*                                                                                        & 
                            (-3.d0 + 2.d0*tmp_mu_2*(-3.d0 + tmp_mu_2 -                                                                    & 
                                 (1.d0*(-2.d0 + tmp_mu_2))/dexp(z1/tmp_mu_2)) +                                                           & 
                              7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))*                                                                & 
                            (630.d0*kF*tmp_mu_4*(-196.d0 +                                                                                & 
                                 tmp_mu_2*(-8680.d0 +                                                                                     & 
                                    3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                              1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                  & 
                               (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                         & 
                                     (-693.d0 + 2.d0*tmp_mu_2*                                                                            & 
                                        (-9625.d0 +                                                                                       & 
                                          12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))        & 
                                  + 558.3229630352375d0*tmp_mu_3*                                                                         & 
                                  (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*    & 
                                  derf(z1/tmp_mu))))/dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2)))/                                             & 
                dexp(z1/tmp_mu_2))/                                                                                                       & 
             (tmp_c_4*((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -         & 
                       1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                        (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                      & 
                          53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/                & 
                  dexp(z3/tmp_mu_2) -                                                                                                     & 
                 (1.269665471198645d-11*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                     & 
                      dexp(z1/tmp_mu_2)*kF*                                                                                               & 
                       (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                    & 
                         21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                          & 
                    (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                   & 
                      1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                          & 
                       (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                       & 
                         53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                     & 
                    (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +           & 
                      dexp(z1/tmp_mu_2)*kF*                                                                                               & 
                       (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                     & 
                            3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                             & 
                          (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                      & 
                  dexp(z3/tmp_mu_2) +                                                                                                     & 
                 (1.058054559332204d-10*kF*(1.d0 +                                                                                        & 
                      6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                  & 
                      10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                       & 
                    (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +           & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                    & 
                             3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                            & 
                           (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                  & 
                  dexp(z2/tmp_mu_2) +                                                                                                     & 
                 (8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                               & 
                       (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                    & 
                            3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                  & 
                      1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                          & 
                       (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                 & 
                             (-693.d0 + 2.d0*tmp_mu_2*                                                                                    & 
                                (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))      & 
                          + 558.3229630352375d0*tmp_mu_3*                                                                                 & 
                          (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*            & 
                          derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                        & 
                         (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                    & 
                            dexp(z1/tmp_mu_2)*kF*                                                                                         & 
                             (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                              & 
                               21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                                & 
                       dexp(z2/tmp_mu_2) -                                                                                                & 
                      (2.01034094528448d-6*kF*(1.d0 +                                                                                     & 
                           6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                             & 
                           10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                  & 
                         (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                              & 
                           1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                     & 
                            (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                  & 
                              53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/               & 
                       dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2)))))/                                                                         & 
        (1.d0 - (1.d0*((2.519177522219533d-13*(30.d0*kF*tmp_mu_4*                                                                         & 
                      (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                            & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**3)/                    & 
                dexp(z3/tmp_mu_2) -                                                                                                       & 
               (7.851981887437505d-15*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                 & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                         & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                        (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu))))/dexp(z3/tmp_mu_2) +                                                                            & 
               (8.922706690269892d-16*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                         (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -         & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                  & 
                            (-693.d0 + 2.d0*tmp_mu_2*                                                                                     & 
                               (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))))       & 
                         + 558.3229630352375d0*tmp_mu_3*                                                                                  & 
                         (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*             & 
                         derf(z1/tmp_mu)))**2)/dexp(z3/tmp_mu_2) +                                                                        & 
               (1.104138521838546d-6*(-126.d0*kF*tmp_mu_4*                                                                                & 
                     (-704.d0 + tmp_mu_2*(-47072.d0 +                                                                                     & 
                          5.d0*tmp_mu_2*(-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +              & 
                             9.989952d+6*tmp_mu_8))) +                                                                                    & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-5549.d0 + 504.d0*tmp_mu_4*(-3003.d0 +                                                                              & 
                          4.d0*tmp_mu_2*(-32032.d0 +                                                                                      & 
                             3.d0*tmp_mu_2*(-207207.d0 +                                                                                  & 
                                20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +             & 
                       111.6645926070475d0*tmp_mu_3*                                                                                      & 
                        (1408.d0 + 455.d0*tmp_mu_2*(216.d0 +                                                                              & 
                             11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*derf(z1/tmp_mu)))*      & 
                  ((5.74383127224137d-9*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -               & 
                          1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                      & 
                           (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                   & 
                             53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**2)/             & 
                     dexp(z2/tmp_mu_2) -                                                                                                  & 
                    (8.376420605351998d-8*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                   & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                 & 
                            21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                       & 
                       (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +        & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-149.d0 + 180.d0*tmp_mu_4*                                                                                     & 
                             (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                      & 
                            26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*          & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2)))/dexp(z1/tmp_mu_2)))/                                                  & 
           (tmp_c_6*((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -           & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/                  & 
                dexp(z3/tmp_mu_2) -                                                                                                       & 
               (1.269665471198645d-11*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                     & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                        & 
                dexp(z3/tmp_mu_2) +                                                                                                       & 
               (1.058054559332204d-10*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                           & 
                     (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                 & 
                    10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                         & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                    & 
                dexp(z2/tmp_mu_2) +                                                                                                       & 
               (8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                                 & 
                     (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                      & 
                          3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                    & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                          & 
                       (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                      & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                & 
                             21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                                  & 
                     dexp(z2/tmp_mu_2) -                                                                                                  & 
                    (2.01034094528448d-6*kF*(1.d0 +                                                                                       & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/                 & 
                     dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2))) -                                                                            & 
          (1.d0*((3.627615631996127d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -               & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**2*                   & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                        & 
                dexp(z3/tmp_mu_2) -                                                                                                       & 
               (5.290272796661019d-12*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                    & 
                dexp(z3/tmp_mu_2) +                                                                                                       & 
               (1.942233019753171d-12*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                     & 
                        21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2*                                        & 
                  (-126.d0*kF*tmp_mu_4*(-704.d0 + tmp_mu_2*                                                                               & 
                        (-47072.d0 + 5.d0*tmp_mu_2*(-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 -                           & 
                             2.5593024d+7*tmp_mu_6 + 9.989952d+6*tmp_mu_8))) +                                                            & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-5549.d0 + 504.d0*tmp_mu_4*(-3003.d0 +                                                                              & 
                          4.d0*tmp_mu_2*(-32032.d0 +                                                                                      & 
                             3.d0*tmp_mu_2*(-207207.d0 +                                                                                  & 
                                20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +             & 
                       111.6645926070475d0*tmp_mu_3*                                                                                      & 
                        (1408.d0 + 455.d0*tmp_mu_2*(216.d0 +                                                                              & 
                             11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*derf(z1/tmp_mu))))/     & 
                dexp(z3/tmp_mu_2) +                                                                                                       & 
               (1.374096830301563d-12*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                           & 
                     (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                 & 
                    10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                         & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                         & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                        (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) -                                                                            & 
               (0.00007578806813899778d0*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -              & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  ((2.92881839347972d-8*kF*(1.d0 +                                                                                        & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                    & 
                            tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                          & 
                                (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                      & 
                                  9.989952d+6*tmp_mu_8))) +                                                                               & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-5549.d0 + 504.d0*tmp_mu_4*                                                                                    & 
                             (-3003.d0 + 4.d0*tmp_mu_2*                                                                                   & 
                                (-32032.d0 + 3.d0*tmp_mu_2*                                                                               & 
                                   (-207207.d0 + 20.d0*tmp_mu_2*                                                                          & 
                                      (-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +                      & 
                            111.6645926070475d0*tmp_mu_3*                                                                                 & 
                             (1408.d0 + 455.d0*tmp_mu_2*                                                                                  & 
                                (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*           & 
                             derf(z1/tmp_mu))))/dexp(z1/tmp_mu_2) +                                                                       & 
                    (1.087846831863896d-9*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                   & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                 & 
                            21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                       & 
                       (630.d0*kF*tmp_mu_4*(-196.d0 +                                                                                     & 
                            tmp_mu_2*(-8680.d0 + 3.d0*tmp_mu_2*                                                                           & 
                                (-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                            & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                              & 
                                (-693.d0 + 2.d0*tmp_mu_2*                                                                                 & 
                                   (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6))      & 
                                  )) + 558.3229630352375d0*tmp_mu_3*                                                                      & 
                             (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*         & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2)))/dexp(z1/tmp_mu_2)))/                                                  & 
           (tmp_c_2*((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -           & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/                  & 
                dexp(z3/tmp_mu_2) -                                                                                                       & 
               (1.269665471198645d-11*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                     & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                        & 
                dexp(z3/tmp_mu_2) +                                                                                                       & 
               (1.058054559332204d-10*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                           & 
                     (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                 & 
                    10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                         & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                    & 
                dexp(z2/tmp_mu_2) +                                                                                                       & 
               (8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                                 & 
                     (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                      & 
                          3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                    & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                          & 
                       (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                      & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                & 
                             21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                                  & 
                     dexp(z2/tmp_mu_2) -                                                                                                  & 
                    (2.01034094528448d-6*kF*(1.d0 +                                                                                       & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/                 & 
                     dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2))) -                                                                            & 
          (1.d0*((4.711189132462503d-15*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -               & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**2*                   & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                        (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu))))/dexp(z3/tmp_mu_2) +                                                                            & 
               (6.870484151507817d-14*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                         & 
                  (630.d0*kF*tmp_mu_4*(-196.d0 + tmp_mu_2*                                                                                & 
                        (-8680.d0 + 3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -          & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu))))/dexp(z3/tmp_mu_2) -                                                                            & 
               (0.00007578806813899778d0*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -              & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  ((3.988771716834285d-9*(30.d0*kF*tmp_mu_4*                                                                              & 
                           (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                       & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-149.d0 + 180.d0*tmp_mu_4*                                                                                    & 
                              (-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 + 3280.d0*tmp_mu_8) +                     & 
                             26.58680776358274d0*tmp_mu_3*(112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*         & 
                              derf(z1/tmp_mu)))**2)/dexp(z2/tmp_mu_2) +                                                                   & 
                    (1.46440919673986d-9*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                    & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                 & 
                            21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                       & 
                       (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                    & 
                            tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                          & 
                                (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                      & 
                                  9.989952d+6*tmp_mu_8))) +                                                                               & 
                         dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                          (-5549.d0 + 504.d0*tmp_mu_4*                                                                                    & 
                             (-3003.d0 + 4.d0*tmp_mu_2*                                                                                   & 
                                (-32032.d0 + 3.d0*tmp_mu_2*                                                                               & 
                                   (-207207.d0 + 20.d0*tmp_mu_2*                                                                          & 
                                      (-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +                      & 
                            111.6645926070475d0*tmp_mu_3*                                                                                 & 
                             (1408.d0 + 455.d0*tmp_mu_2*                                                                                  & 
                                (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*           & 
                             derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2)))/dexp(z1/tmp_mu_2) +                                                   & 
               0.02652582384864922d0*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                            & 
                   (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                   & 
                  10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                           & 
                ((6.973377127332666d-11*(30.d0*kF*tmp_mu_4*                                                                               & 
                        (56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +                          & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                    & 
                             3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                            & 
                           (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))*                      & 
                     (-126.d0*kF*tmp_mu_4*(-704.d0 +                                                                                      & 
                          tmp_mu_2*(-47072.d0 + 5.d0*tmp_mu_2*                                                                            & 
                              (-271868.d0 - 3.526766d+6*tmp_mu_2 - 1.8609033d+7*tmp_mu_4 - 2.5593024d+7*tmp_mu_6 +                        & 
                                9.989952d+6*tmp_mu_8))) +                                                                                 & 
                       dexp(z1/tmp_mu_2)*kF*                                                                                              & 
                        (-5549.d0 + 504.d0*tmp_mu_4*(-3003.d0 +                                                                           & 
                             4.d0*tmp_mu_2*(-32032.d0 +                                                                                   & 
                                3.d0*tmp_mu_2*(-207207.d0 +                                                                               & 
                                   20.d0*tmp_mu_2*(-90376.d0 - 295295.d0*tmp_mu_2 - 185328.d0*tmp_mu_4 + 52031.d0*tmp_mu_6)))) +          & 
                          111.6645926070475d0*tmp_mu_3*                                                                                   & 
                           (1408.d0 + 455.d0*tmp_mu_2*                                                                                    & 
                              (216.d0 + 11.d0*tmp_mu_2*(560.d0 + 7368.d0*tmp_mu_2 + 40824.d0*tmp_mu_4 + 68607.d0*tmp_mu_6)))*             & 
                           derf(z1/tmp_mu))))/dexp(z2/tmp_mu_2) -                                                                         & 
                  (6.727562349189214d-13*(630.d0*kF*tmp_mu_4*                                                                             & 
                         (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                  & 
                              3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                & 
                        1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                        & 
                         (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                               & 
                               (-693.d0 + 2.d0*tmp_mu_2*                                                                                  & 
                                  (-9625.d0 + 12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))      & 
                              ) + 558.3229630352375d0*tmp_mu_3*                                                                           & 
                            (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*          & 
                            derf(z1/tmp_mu)))**2)/dexp(z2/tmp_mu_2))))/                                                                   & 
           (tmp_c_4*((4.353138758395353d-13*(60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -           & 
                     1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                      (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                        & 
                        53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))**3)/                  & 
                dexp(z3/tmp_mu_2) -                                                                                                       & 
               (1.269665471198645d-11*(24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                       & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                      & 
                       21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))*                                            & 
                  (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                     & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                         & 
                       53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu)))*                       & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                    dexp(z1/tmp_mu_2)*kF*                                                                                                 & 
                     (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                       & 
                          3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                               & 
                        (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu))))/                        & 
                dexp(z3/tmp_mu_2) +                                                                                                       & 
               (1.058054559332204d-10*kF*(1.d0 + 6.d0*tmp_mu_4*                                                                           & 
                     (3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                                                 & 
                    10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                         & 
                  (30.d0*kF*tmp_mu_4*(56.d0 + 1466.d0*tmp_mu_2 + 13533.d0*tmp_mu_4 + 28704.d0*tmp_mu_6 - 19680.d0*tmp_mu_8) +             & 
                     dexp(z1/tmp_mu_2)*kF*                                                                                                & 
                      (-149.d0 + 180.d0*tmp_mu_4*(-105.d0 - 1680.d0*tmp_mu_2 - 8694.d0*tmp_mu_4 - 8064.d0*tmp_mu_6 +                      & 
                           3280.d0*tmp_mu_8) + 26.58680776358274d0*tmp_mu_3*                                                              & 
                         (112.d0 + 45.d0*tmp_mu_2*(72.d0 + 672.d0*tmp_mu_2 + 1715.d0*tmp_mu_4))*derf(z1/tmp_mu)))**2)/                    & 
                dexp(z2/tmp_mu_2) +                                                                                                       & 
               (8.202171876514911d-7*(630.d0*kF*tmp_mu_4*                                                                                 & 
                     (-196.d0 + tmp_mu_2*(-8680.d0 +                                                                                      & 
                          3.d0*tmp_mu_2*(-51650.d0 - 344447.d0*tmp_mu_2 - 576320.d0*tmp_mu_4 + 286080.d0*tmp_mu_6))) -                    & 
                    1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                            & 
                     (4.d0*(-2279.d0 + 630.d0*tmp_mu_4*                                                                                   & 
                           (-693.d0 + 2.d0*tmp_mu_2*(-9625.d0 +                                                                           & 
                                12.d0*tmp_mu_2*(-9009.d0 - 36036.d0*tmp_mu_2 - 26950.d0*tmp_mu_4 + 8940.d0*tmp_mu_6)))) +                 & 
                       558.3229630352375d0*tmp_mu_3*                                                                                      & 
                        (392.d0 + 165.d0*tmp_mu_2*(112.d0 + tmp_mu_2*(1980.d0 + 343.d0*tmp_mu_2*(40.d0 + 81.d0*tmp_mu_2))))*              & 
                        derf(z1/tmp_mu)))*((1.75904832712392d-6*                                                                          & 
                       (24.d0*kF*tmp_mu_4*(4.d0 + 13.d0*tmp_mu_2 - 27.d0*tmp_mu_4) +                                                      & 
                          dexp(z1/tmp_mu_2)*kF*                                                                                           & 
                           (-13.d0 - 540.d0*tmp_mu_4 - 960.d0*tmp_mu_6 + 648.d0*tmp_mu_8 +                                                & 
                             21.26944621086619d0*tmp_mu_3*(8.d0 + 45.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)/                                  & 
                     dexp(z2/tmp_mu_2) -                                                                                                  & 
                    (2.01034094528448d-6*kF*(1.d0 +                                                                                       & 
                         6.d0*tmp_mu_4*(3.d0 - 2.d0*tmp_mu_2 + (-1.d0 + 2.d0*tmp_mu_2)/dexp(z1/tmp_mu_2)) -                               & 
                         10.6347231054331d0*tmp_mu_3*derf(z1/tmp_mu))*                                                                    & 
                       (60.d0*kF*tmp_mu_4*(-25.d0 - 313.d0*tmp_mu_2 - 906.d0*tmp_mu_4 + 984.d0*tmp_mu_6) -                                & 
                         1.d0*dexp(z1/tmp_mu_2)*kF*                                                                                       & 
                          (-163.d0 + 120.d0*tmp_mu_4*(-105.d0 - 770.d0*tmp_mu_2 - 945.d0*tmp_mu_4 + 492.d0*tmp_mu_6) +                    & 
                            53.17361552716548d0*tmp_mu_3*(50.d0 + 756.d0*tmp_mu_2 + 2625.d0*tmp_mu_4)*derf(z1/tmp_mu))))/                 & 
                     dexp(z1/tmp_mu_2)))/dexp(z1/tmp_mu_2))))                                                                               

     v_x = 0.d0

    elseif (dirac_approximant == "mu_interpolation") then

    !e_x = (0.001343813934716645d0*kF_4*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                            &                 
     e_x = (0.001343813934716645d0*kF_HF_3*kF*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                            &                 
             9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                                 &      
              (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))**2*                 &      
          (3.544907701811032d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                   &      
               2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +          &      
               3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2) -                                          &      
            (1.d0*tmp_mu*(-3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                  &      
                     2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +    &      
                     3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2)**3 -                                 &      
                 50.26548245743669d0*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                               &      
                    9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                          &      
                     (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c))) -            &      
                 3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                            &      
                    2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +     &      
                    3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2)*                                      &      
                  (4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                                                  &      
                    9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                          &      
                     (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))))/           &      
             (4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                                                       &      
               9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                               &      
                (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))))/                &      
        (3.544907701811032d0*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                                       &      
              9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                                &      
               (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))**2 +               &      
          3.d0*tmp_mu*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                                              &      
             9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                                 &      
              (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))*                    &      
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 +                                                                      &      
             9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                                 &      
              (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)) +                    &      
             (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                     &      
                2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +         &      
                3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2)**2) -                                     &      
          12.d0*tmp_mu_3*(3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                   &      
                 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +        &      
                 3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2)**3 +                                     &      
             50.26548245743669d0*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                                   &      
                9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                              &      
                 (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c))) +                &      
             3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                &      
                2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +         &      
                3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2)*                                          &      
              (4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                                                      &      
                9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                              &      
                 (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))) +               &      
          42.53889242173238d0*tmp_mu_2*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4) +                                             &      
             9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                                 &      
              (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)))*                    &      
           (2.d0 - 1.d0*tmp_c_2 + 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*                                   &      
              dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) -                                                            &      
             3.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2 +                                             &      
             2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2))))

    v_x = (0.001473656880480512d0*kF*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                               & 
       9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*(4.d0*                                                                                                                 &
        (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                            &
          9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                    &
        (42.53889242173238d0*tmp_mu_2*(2.d0 - 1.d0*tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) +                                                                    &
             2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                   &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                                &
          3.544907701811032d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                      &
              9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2 +                                                                                                            &
          3.d0*tmp_mu*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                              &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                 &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2 +                                                                                                                 &
             (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +           &
                3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2) +                                                                                                         &
          12.d0*tmp_mu_3*(3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                   &
                 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -           &
             50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                   &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                             &
             3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                                &
                2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                &
              (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                      &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))*                                                                                                            &
        (31.90416931629929d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                         &
             2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                  &
          (9.d0*tmp_mu*(-3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                        &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -         &
               50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                 &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                                           &
               3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                                  &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                    &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/                                                                                                          &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)) -                                                                                                               &
       (36.d0*tmp_c*(1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                           &
            3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                      &
          (42.53889242173238d0*tmp_mu_2*(2.d0 - 1.d0*tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) +                                                                  &
               2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                 &
             (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                       &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                              &
            3.544907701811032d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                    &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2 +                                                                                                          &
            3.d0*tmp_mu*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                            &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                               &
             (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                       &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2 +                                                                                                               &
               (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2) +                                                                                                       &
            12.d0*tmp_mu_3*(3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                 &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -         &
               50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                 &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                           &
               3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                              &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                    &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))*                                                                                                          &
          (31.90416931629929d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                       &
               2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                &
            (9.d0*tmp_mu*(-3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                      &
                     2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3         &
                  - 50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                            &
                    9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                                         &
                 3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                                &
                    2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*            &
                  (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                  &
                    9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/                                                                                                        &
             (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                       &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/(1/tmp_c + tmp_c) +                                                                                          &
       9.d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                        &
          9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                    &
        (42.53889242173238d0*tmp_mu_2*(2.d0 - 1.d0*tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) +                                                                    &
             2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                   &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                                &
          3.544907701811032d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                      &
              9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2 +                                                                                                            &
          3.d0*tmp_mu*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                              &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                 &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2 +                                                                                                                 &
             (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +           &
                3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2) +                                                                                                         &
          12.d0*tmp_mu_3*(3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                   &
                 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -           &
             50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                   &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                             &
             3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                                &
                2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                &
              (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                      &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))*                                                                                                            &
        ((-14.17963080724413d0*tmp_c*(1.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                  &
               2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2       &
               ))/(1/tmp_c + tmp_c) + (tmp_mu*(-3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                 &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -         &
               50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                 &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                                           &
               3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                                  &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                    &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/                                                                                                          &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                                                &
          (18.d0*tmp_c_2*tmp_mu*(1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +               &
               3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                   &
             (-3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                                  &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -         &
               50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                 &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                                           &
               3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                                  &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                    &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/                                                                                                          &
           ((1.d0 + tmp_c_2)*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                       &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2) -                                                                                                         &
          (6.d0*tmp_c_2*tmp_mu*(6.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                 &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2*          &
                (1.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                               &
               2.d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                            &
                (1.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                               &
               150.7964473723101d0*(1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +            &
                  3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                               &
               9.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                              &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                               &
                  3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/                                                                                              &
           ((1.d0 + tmp_c_2)*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                       &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2))) -                                                                                                            &
       3.d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                        &
          9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                    &
        (31.90416931629929d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                         &
             2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                  &
          (9.d0*tmp_mu*(-3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                        &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -         &
               50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                 &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                                           &
               3.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                                  &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                    &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/                                                                                                          &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2))*                                                                                                                &
        (-28.35926161448826d0*tmp_mu_2*(2.d0 - 1.d0*tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) +                                                                   &
             2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                   &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                         &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                                &
          (56.71852322897651d0*tmp_c_2*tmp_mu_2*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                    &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                               &
             (1.d0 + tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) -                                                                                                  &
               2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2       &
               ))/(1.d0 + tmp_c_2) - (255.2333545303943d0*tmp_c*tmp_mu_2*                                                                                                                 &
             (2.d0 - 1.d0*tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) +                                                                                             &
               2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                 &
             (1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                  &
               3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2))/(1/tmp_c + tmp_c) -                                                                               &
          (42.53889242173238d0*tmp_c*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                               &
               9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                               &
             (1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                  &
               3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2))/(1/tmp_c + tmp_c) -                                                                               &
          1.d0*tmp_mu*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                              &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                 &
           (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 + 9.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)*                                                                                 &
              (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)) +                                                                                        &
             (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +           &
                3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2) -                                                                                                         &
          (18.d0*tmp_c_2*tmp_mu*(1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +               &
               3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                   &
             (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 + 9.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)*                                                                               &
                (-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)) +                                                                                      &
               (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2))/(1.d0 + tmp_c_2) +                                                                                     &
          36.d0*tmp_mu_3*((-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                                            &
                2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 +            &
             16.75516081914556d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                   &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                             &
             (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +           &
                3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                              &
              (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                      &
                9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)) +                                                                                                            &
          tmp_c*tmp_mu*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                             &
             9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                 &
           (-18.d0*tmp_c - 36.d0*tmp_c_3 + (9.d0*tmp_c_2*(-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)))/dsqrt(1.d0 + tmp_c_m_2) -                    &
             36.d0*tmp_c_3*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)*(-2.d0*dsqrt(1.d0 + tmp_c_2) + tmp_c_2*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)) -                                &
             (9.d0*tmp_c_5*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)*(-3.d0*dsqrt(1.d0 + tmp_c_2) + 2.d0*(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)))/                  &
              (1.d0 + tmp_c_2) - (8.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                               &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (1.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2))/(1/tmp_c + tmp_c)) -                                                                           &
          (24.d0*tmp_c_2*tmp_mu_3*(-6.d0*(-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) -                                                                             &
                   2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) + 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2*          &
                (1.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                               &
               2.d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                &
                  9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                            &
                (1.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +         &
                  3.d0*(tmp_c_2 + tmp_c_4)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) -                                                                                               &
               150.7964473723101d0*(1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +            &
                  3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                               &
               9.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                              &
                  2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*              &
                (1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4 - 2.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                               &
                  3.d0*(tmp_c_4 + tmp_c_6)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))/(1.d0 + tmp_c_2))))/                                                                          &
   (42.53889242173238d0*tmp_mu_2*(2.d0 - 1.d0*tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(tmp_c_2/(1.d0 + tmp_c_2)) +                                                                         &
         2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                       &
       (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                             &
         9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                                    &
      3.544907701811032d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                          &
          9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2 +                                                                                                                &
      3.d0*tmp_mu*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                  &
         9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                     &
       (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                             &
         9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2 +                                                                                                                     &
         (-2.d0 + tmp_c_2 + 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) - 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +               &
            3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**2) +                                                                                                             &
      12.d0*tmp_mu_3*(3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                                                                       &
             2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) - 3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)**3 -               &
         50.26548245743669d0*(4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                       &
            9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2) +                                                                                                                 &
         3.d0*(2.d0 - 1.d0*tmp_c_2 - 2.d0*(1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) + 2.d0*dsqrt(1.d0 + tmp_c_2)*(2.d0 + 3.d0*tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) -      &
            3.d0*tmp_c_4*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)*                                                                                                                  &
          (4.d0 + 9.d0*tmp_c_2 + 9.d0*tmp_c_4 - 18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c) +                                                          &
            9.d0*tmp_c_6*dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)**2)))**2                                                                                                              

 
   else
    print*, 'Exchange functional required does not exist ...'
    print*,'dirac_exchange_functional',dirac_exchange_functional
    stop
   endif

   ! For very large values of tmp_mu
   elseif (tmp_mu .lt. 1.d+9) then

   ! Inverse quadratic/quartic/hexuple/octuple for large values of tmp_mu
   !e_x = (-c30*kF_4*(z4 + z9*(tmp_c_2 + tmp_c_4) + z9*tmp_c_4*dlog(dsqrt(z1 + tmp_c_m_2) + tmp_c_m)*(-z2*dsqrt(z1 + tmp_c_2) +  & 
   !    tmp_c_2*dlog(dsqrt(z1 + tmp_c_m_2) + tmp_c_m))))/tmp_mu_2 +
   !e_x = (c29*kF_4*(z132 - z15*tmp_c_2*(-z32 + z9*tmp_c_2 + z45*tmp_c_4) - z320*(z4 + z9*(tmp_c_2 + tmp_c_4))*tmp_mu_2 + z45*tmp_c_4*(dlog(dsqrt(z1 + tmp_c_m_2) +     &
   !    z1/tmp_c)*(dsqrt(z1 + tmp_c_2)*(-z6 + z15*tmp_c_2 + z64*tmp_mu_2) + tmp_c_2*(z15*tmp_c_2 + z64*tmp_mu_2)*(dlog(tmp_c) - z1*dlog(z1+ dsqrt(z1 + tmp_c_2)))) -    &
   !    z1*dsqrt(z1 + tmp_c_2)*(-z6 + z15*tmp_c_2 + z64*tmp_mu_2)*(dlog(tmp_c) - z1*dlog(z1 + dsqrt(z1 + tmp_c_2))))))/tmp_mu_4
   !e_x =   (c33*kF_4*(z16*(-z74 + z693*tmp_mu_2) +                             &
   !   z35*(-z164*tmp_c_2 + z164*tmp_c_4 + z45*tmp_c_6 - z315*tmp_c_8 -        &       
   !   z36*tmp_c_2*(-z32 + z9*tmp_c_2 + z45*tmp_c_4)*tmp_mu_2 -                 &       
   !   z768*(z4 + z9*(tmp_c_2 + tmp_c_4))*tmp_mu_4) +                           &       
   !   z105*tmp_c_4*(dlog(dsqrt(z1 + tmp_c_m_2) +z1/tmp_c)*                     &       
   !   (dsqrt(z1 + tmp_c_2)*(z105*tmp_c_4 + z60*tmp_c_2*(-z1 + z9*tmp_mu_2) +   &       
   !   z8*(z2 - z27*tmp_mu_2 + z288*tmp_mu_4)) +                                &       
   !   z3*tmp_c_2*(z35*tmp_c_4 + z180*tmp_c_2*tmp_mu_2 + z768*tmp_mu_4)*        &       
   !   (dlog(tmp_c) - z1*dlog(z1 + dsqrt(z1 + tmp_c_2)))) -                     &       
   !   z1*dsqrt(z1 + tmp_c_2)*(z105*tmp_c_4 + z60*tmp_c_2*(-z1 + z9*tmp_mu_2) + &       
   !   z8*(z2 - z27*tmp_mu_2 + z288*tmp_mu_4))*                                 &         
   !   (dlog(tmp_c) - z1*dlog(z1 + dsqrt(z1 + tmp_c_2))))))/tmp_mu_6 
   !e_x = (1.084919225877748d-11*kF_4*                                                                 &   
    e_x = (1.084919225877748d-11*kF_HF_3*kF*                                                                 &   
          (46816.d0 - 454656.d0*tmp_mu_2 +                                                             &   
            21.d0*(tmp_c_2*(13248.d0 + 5.d0*tmp_c_2*                                                   &   
                   (-3564.d0 + 35.d0*tmp_c_2*(44.d0 + 51.d0*tmp_c_2 - 189.d0*tmp_c_4))) -              &   
               640.d0*tmp_c_2*(164.d0 - 164.d0*tmp_c_2 - 45.d0*tmp_c_4 + 315.d0*tmp_c_6)*tmp_mu_2 -    &   
               4608.d0*(-44.d0 + 5.d0*tmp_c_2*(-32.d0 + 9.d0*tmp_c_2 + 45.d0*tmp_c_4))*tmp_mu_4 -      &   
               491520.d0*(4.d0 + 9.d0*(tmp_c_2 + tmp_c_4))*tmp_mu_6) +                                 &   
            315.d0*tmp_c_4*(dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                &   
                (dsqrt(1.d0 + tmp_c_2)*(2205.d0*tmp_c_6 + 280.d0*tmp_c_4*(-5.d0 + 48.d0*tmp_mu_2) +    &   
                     20.d0*tmp_c_2*(35.d0 - 384.d0*tmp_mu_2 + 3456.d0*tmp_mu_4) +                      &   
                     8.d0*(-15.d0 + 128.d0*tmp_mu_2*(2.d0 - 27.d0*tmp_mu_2 + 288.d0*tmp_mu_4))) +      &   
                  3.d0*tmp_c_2*(735.d0*tmp_c_6 + 4480.d0*tmp_c_4*tmp_mu_2 +                            &   
                     23040.d0*tmp_c_2*tmp_mu_4 + 98304.d0*tmp_mu_6)*                                   &   
                   (dlog(tmp_c) - 1.d0*dlog(1.d0 + dsqrt(1.d0 + tmp_c_2)))) -                          &   
               1.d0*dsqrt(1.d0 + tmp_c_2)*                                                             &   
                (2205.d0*tmp_c_6 + 280.d0*tmp_c_4*(-5.d0 + 48.d0*tmp_mu_2) +                           &   
                  20.d0*tmp_c_2*(35.d0 - 384.d0*tmp_mu_2 + 3456.d0*tmp_mu_4) +                         &   
                  8.d0*(-15.d0 + 128.d0*tmp_mu_2*(2.d0 - 27.d0*tmp_mu_2 + 288.d0*tmp_mu_4)))*          &   
                (dlog(tmp_c) - 1.d0*dlog(1.d0 + dsqrt(1.d0 + tmp_c_2))))))/tmp_mu_8                        

   ! Inverse quadratic/quartic/hexuple for large values of tmp_mu
   !v_x = (-c31*kF*(z2 + z5*tmp_c_2 + z3*tmp_c_4 + z3*tmp_c_4*dsqrt(z1 + tmp_c_2)*dlog(tmp_c/(z1 + dsqrt(z1 + tmp_c_2)))))/((z1 + tmp_c_2)*tmp_mu_2) 
   !v_x = (c32*kF*(dsqrt(z1 + tmp_c_2)*(z44 + z120*tmp_c_2 - z45*tmp_c_4 - z160*(z2 + z3*tmp_c_2)*tmp_mu_2) + z15*tmp_c_4*(-z6 + z3*tmp_c_2 + z32*tmp_mu_2)*dlog((z1 +  & 
   !    dsqrt(z1 + tmp_c_2))/tmp_c)))/(dsqrt(z1 + tmp_c_2)*tmp_mu_4)  
    v_x = (c34*kF*((z1 + tmp_c_2)*                                                             &                         
       (-z5*(z592 + z7*tmp_c_2*(z328 + z45*tmp_c_2*(-z6 + tmp_c_2))) -                         &                                  
       z504*(-z44 - z120*tmp_c_2 + z45*tmp_c_4)*tmp_mu_2 - z80640*(z2 + z3*tmp_c_2)*tmp_mu_4)  &                
       + z105*tmp_c_4*dsqrt(z1 + tmp_c_2)*                                                     &                
       (z48 - z80*tmp_c_2 + z15*tmp_c_4 + z216*(-z2 + tmp_c_2)*tmp_mu_2 + z2304*tmp_mu_4)*     &                        
       dlog((z1 + dsqrt(z1 + tmp_c_2))/tmp_c)))/((z1 + tmp_c_2)*tmp_mu_6)                                       
        

   ! Limit for large tmp_mu 
   else
  
    e_x = 0.d0
    v_x = 0.d0 
   
   endif 
   ! For the Coulomb_Gaunt ee interaction
  elseif (dirac_interaction == "Coulomb_Gaunt") then

   ! quadratic range-separation for very low values of tmp_mu
   if (tmp_mu .lt. 1.d-1) then

    e_x = (0.002687627869433291d0*kF_4*                                                    & 
          (-6.d0*tmp_c_2*tmp_mu_2 -                                                        & 
          21.26944621086619d0*tmp_c_2*(1.d0 + tmp_c_2)*tmp_mu*                             & 
          (-1.d0 + tmp_c*datan(z1/tmp_c)) -                                                & 
          4.d0*dsqrt(1.d0 + tmp_c_2)*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) -          & 
          22.d0*tmp_c_2*dsqrt(1.d0 + tmp_c_2)*                                             & 
          dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) -                                     & 
          18.d0*tmp_c_4*dsqrt(1.d0 + tmp_c_2)*                                             & 
          dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c) +                                     & 
          (1.d0 + tmp_c_2)*(4.d0 + tmp_c_2 +                                               & 
          (1.d0 + tmp_c_2)**2*dlog(1.d0 + tmp_c_m_2) +                                     & 
          9.d0*tmp_c_4*dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)**2 -                     & 
          2.d0*dlog(tmp_c) + dlog(1.d0 + tmp_c_2) +                                        & 
          tmp_c_2*(8.d0 + 7.d0*tmp_c_2)*(-2.d0*dlog(tmp_c) + dlog(1.d0 + tmp_c_2)))))/     & 
          (1.d0 + tmp_c_2)


    v_x = (0.1061032953945969d0*kF*(4.d0*(1.d0 + tmp_c_2)**2 +                             & 
           5.317361552716548d0*tmp_c_2*(1.d0 + tmp_c_2)*tmp_mu -                           &
           3.d0*tmp_c_4*tmp_mu_2 -                                                         &
           2.d0*(1.d0 + tmp_c_2)**2*(2.d0 + 5.d0*tmp_c_2)*dlog(tmp_c) +                    &
           (1.d0 + tmp_c_2)**2*(2.d0 + 5.d0*tmp_c_2)*dlog(1.d0 + tmp_c_2) +                &
           4.d0*dsqrt(1.d0 + tmp_c_2)*(1.d0 + 4.d0*tmp_c_2 + 3.d0*tmp_c_4)*                &
           dlog(tmp_c/(1.d0 + dsqrt(1.d0 + tmp_c_2)))))/(1.d0 + tmp_c_2)**2
 
   ! Medium values of tmp_mu
   !elseif (tmp_mu .le. 1d+1) then
   elseif (tmp_mu .le. 1.25d0) then
    if (dirac_approximant == "dirac_pade_order_2") then
     e_x = (0.00002559645589936467d0*kF_4*(105.d0*(-2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) +                                                  &  
               dexp(z1/tmp_mu_2)*(-3.d0 - 6.d0*tmp_mu_2 + 2.d0*tmp_mu_4 + 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))) +                & 
            (-7.d0*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                            & 
                   dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6) +                                    & 
                   21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu))**2 +                           & 
               15.d0*(6.d0*tmp_mu_2*(30.d0 + 167.d0*tmp_mu_2 + 404.d0*tmp_mu_4 - 576.d0*tmp_mu_6) +                                     & 
                  dexp(z1/tmp_mu_2)*(-161.d0 + 6.d0*tmp_mu_2*(-105.d0 - 735.d0*tmp_mu_2 - 980.d0*tmp_mu_4 + 576.d0*tmp_mu_6)) +         & 
                  21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu*(15.d0 + 91.d0*tmp_mu_2 + 315.d0*tmp_mu_4)*derf(z1/tmp_mu))*             & 
                (-2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) + dexp(z1/tmp_mu_2)*                                                                 & 
                   (-3.d0 - 6.d0*tmp_mu_2 + 2.d0*tmp_mu_4 + 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))))/                              & 
             (tmp_c_2*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                         & 
                 dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 +                                       & 
                    21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu))))))/                                            & 
             (dexp(z1/tmp_mu_2)*(1.d0 + (0.1428571428571429d0*                                                                          & 
               (6.d0*tmp_mu_2*(30.d0 + 167.d0*tmp_mu_2 + 404.d0*tmp_mu_4 - 576.d0*tmp_mu_6) +                                           & 
                 dexp(z1/tmp_mu_2)*(-161.d0 + 6.d0*tmp_mu_2*(-105.d0 - 735.d0*tmp_mu_2 - 980.d0*tmp_mu_4 + 576.d0*tmp_mu_6) +           & 
                    21.26944621086619d0*tmp_mu*(15.d0 + 91.d0*tmp_mu_2 + 315.d0*tmp_mu_4)*derf(z1/tmp_mu))))/                           & 
             (tmp_c_2*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                         & 
                 dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 +                                       & 
                    21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu))))))

     v_x = (6.551785546081206d-10*((-110167.0865227013d0*kF*                                                                                                                    &
               (-180.d0*tmp_mu_4*(-36.d0 - 258.d0*tmp_mu_2 + 223.d0*tmp_mu_4 + 1712.d0*tmp_mu_6 + 304.d0*tmp_mu_8) -                                                            &
                 12.d0*dexp(z1/tmp_mu_2)*tmp_mu_2*                                                                                                                              &
                  (987.d0 + 5854.d0*tmp_mu_2 + 2494.d0*tmp_mu_4 + 5040.d0*tmp_mu_6 - 97320.d0*tmp_mu_8 - 9120.d0*tmp_mu_10 +                                                    &
                    26.58680776358274d0*tmp_mu*(-72.d0 - 498.d0*tmp_mu_2 + 233.d0*tmp_mu_4 + 1781.d0*tmp_mu_6 + 1836.d0*tmp_mu_8)*derf(z1/tmp_mu)) -                            &
                 1.d0*dexp(z2/tmp_mu_2)*(-5635.d0 +                                                                                                                             &
                    12.d0*tmp_mu_2*(-2415.d0 - 2730.d0*tmp_mu_2 + 14966.d0*tmp_mu_4 - 58905.d0*tmp_mu_6 + 71640.d0*tmp_mu_8 + 4560.d0*tmp_mu_10) +                              &
                    10.6347231054331d0*tmp_mu*(1974.d0 + 5.d0*tmp_mu_2*                                                                                                         &
                        (2161.d0 + 207.d0*tmp_mu_2 + 3780.d0*tmp_mu_4 + 330.d0*tmp_mu_6 - 11016.d0*tmp_mu_8))*derf(z1/tmp_mu) +                                                 &
                    2261.946710584651d0*tmp_mu_2*(-9.d0 - 60.d0*tmp_mu_2 + 7.d0*tmp_mu_4)*derf(z1/tmp_mu)**2))*                                                                 &
               (105.d0*(-2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) + dexp(z1/tmp_mu_2)*                                                                                                  &
                     (-3.d0 - 6.d0*tmp_mu_2 + 2.d0*tmp_mu_4 + 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))) +                                                                    &
                 (-7.d0*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                                                               &
                        dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6) +                                                                       &
                        21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu))**2 +                                                              &
                    15.d0*(6.d0*tmp_mu_2*(30.d0 + 167.d0*tmp_mu_2 + 404.d0*tmp_mu_4 - 576.d0*tmp_mu_6) +                                                                        &
                       dexp(z1/tmp_mu_2)*(-161.d0 + 6.d0*tmp_mu_2*(-105.d0 - 735.d0*tmp_mu_2 - 980.d0*tmp_mu_4 + 576.d0*tmp_mu_6)) +                                            &
                       21.26944621086619d0*dexp(z1/tmp_mu_2)*tmp_mu*(15.d0 + 91.d0*tmp_mu_2 + 315.d0*tmp_mu_4)*derf(z1/tmp_mu))*                                                &
                     (-2.d0*tmp_mu_2*(-2.d0 + tmp_mu_2) + dexp(z1/tmp_mu_2)*                                                                                                    &
                        (-3.d0 - 6.d0*tmp_mu_2 + 2.d0*tmp_mu_4 + 7.089815403622064d0*tmp_mu*derf(z1/tmp_mu))))/                                                                 &
                  (tmp_c_2*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                                                            &
                      dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 +                                                                          &
                         21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu))))))/                                                                               &
             (dexp(z1/tmp_mu_2)*(6.d0*tmp_c*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                                                 &
                  dexp(z1/tmp_mu_2)*tmp_c*                                                                                                                                      &
                   (-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 + 21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)                      &
              + (2.313508816976728d+7*kF*(1.d0 + (0.1428571428571429d0*                                                                                                         &
                    (6.d0*tmp_mu_2*(30.d0 + 167.d0*tmp_mu_2 + 404.d0*tmp_mu_4 - 576.d0*tmp_mu_6) +                                                                              &
                      dexp(z1/tmp_mu_2)*(-161.d0 + 6.d0*tmp_mu_2*(-105.d0 - 735.d0*tmp_mu_2 - 980.d0*tmp_mu_4 + 576.d0*tmp_mu_6) +                                              &
                         21.26944621086619d0*tmp_mu*(15.d0 + 91.d0*tmp_mu_2 + 315.d0*tmp_mu_4)*derf(z1/tmp_mu))))/                                                              &
                  (tmp_c_2*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                                                            &
                      dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 +                                                                          &
                         21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu)))))*                                                                                &
               (36.d0*tmp_mu_6*(21.d0*tmp_c_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4)**2 +                                                                                     &
                    4.d0*(36.d0 + 96.d0*tmp_mu_2 - 529.d0*tmp_mu_4 + 789.d0*tmp_mu_6 - 3570.d0*tmp_mu_8 + 128.d0*tmp_mu_10)) -                                                  &
                 12.d0*dexp(z1/tmp_mu_2)*tmp_mu_4*                                                                                                                              &
                  (372.d0 - 628.d0*tmp_mu_2 + tmp_mu_4*(17371.d0 + 85208.d0*tmp_mu_2 - 281094.d0*tmp_mu_4 + 14688.d0*tmp_mu_6 + 4608.d0*tmp_mu_8) +                             &
                    21.d0*tmp_c_2*(1.d0 + 4.d0*tmp_mu_2)*(-6.d0 + 7.d0*tmp_mu_2)*(-53.d0 + 3.d0*tmp_mu_2*(-53.d0 - 79.d0*tmp_mu_2 + 84.d0*tmp_mu_4))) +                         &
                 dexp(z2/tmp_mu_2)*tmp_mu_2*                                                                                                                                    &
                  (21.d0*tmp_c_2*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6)*                                                                                 &
                     (-107.d0 + 6.d0*(-1.d0 + tmp_mu)*tmp_mu_2*(1.d0 + tmp_mu)*(61.d0 + 84.d0*tmp_mu_2)) +                                                                      &
                    4.d0*(-1309.d0 + 4.d0*tmp_mu_2*(-2422.d0 +                                                                                                                  &
                          3.d0*tmp_mu_2*(5023.d0 + 24067.d0*tmp_mu_2 + 94798.d0*tmp_mu_4 - 220404.d0*tmp_mu_6 + 39474.d0*tmp_mu_8 + 1152.d0*tmp_mu_10)                          &
                          ))) + dexp(z3/tmp_mu_2)*                                                                                                                              &
                  (4655.d0 - 21.d0*tmp_c_2*(1.d0 + tmp_mu_2)*(35.d0 + 90.d0*tmp_mu_2 + 270.d0*tmp_mu_4 - 168.d0*tmp_mu_6)**2 -                                                  &
                    2.d0*tmp_mu_2*(-9450.d0 + tmp_mu_2*(175.d0 +                                                                                                                &
                          6.d0*tmp_mu_2*(30604.d0 + 242655.d0*tmp_mu_2 + 433916.d0*tmp_mu_4 - 591054.d0*tmp_mu_6 + 100368.d0*tmp_mu_8 +                                         &
                             1536.d0*tmp_mu_10)))) + 1.772453850905516d0*dexp(z1/tmp_mu_2)*tmp_mu*derf(z1/tmp_mu)*                                                              &
                  (108.d0*tmp_mu_4*(144.d0 + 564.d0*tmp_mu_2 - 388.d0*tmp_mu_4 + 5657.d0*tmp_mu_6 - 23069.d0*tmp_mu_8 + 8212.d0*tmp_mu_10 +                                     &
                       7.d0*tmp_c_2*(1.d0 + 4.d0*tmp_mu_2)*(-6.d0 + 7.d0*tmp_mu_2)*(-18.d0 - 57.d0*tmp_mu_2 + 28.d0*tmp_mu_4)) -                                                &
                    12.d0*dexp(z1/tmp_mu_2)*tmp_mu_2*                                                                                                                           &
                     (744.d0 + 1114.d0*tmp_mu_2 + 21.d0*tmp_c_2*                                                                                                                &
                        (636.d0 + 3923.d0*tmp_mu_2 + 7954.d0*tmp_mu_4 + 3126.d0*tmp_mu_6 - 17136.d0*tmp_mu_8 + 4704.d0*tmp_mu_10) +                                             &
                       tmp_mu_4*(46993.d0 + 9.d0*tmp_mu_2*(29155.d0 - 12736.d0*tmp_mu_2 - 62562.d0*tmp_mu_4 + 16424.d0*tmp_mu_6)) +                                             &
                       63.80833863259858d0*tmp_mu*(-36.d0 - 186.d0*tmp_mu_2 - 371.d0*tmp_mu_4 - 2893.d0*tmp_mu_6 + 4900.d0*tmp_mu_8 +                                           &
                          7.d0*tmp_c_2*(3.d0 + 10.d0*tmp_mu_2)*(-9.d0 - 27.d0*tmp_mu_2 + 28.d0*tmp_mu_4))*derf(z1/tmp_mu)) +                                                    &
                    dexp(z2/tmp_mu_2)*(-5236.d0 +                                                                                                                               &
                       21.d0*tmp_c_2*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6)*                                                                             &
                        (-107.d0 + 6.d0*tmp_mu_2*(-67.d0 - 85.d0*tmp_mu_2 + 28.d0*tmp_mu_4)) +                                                                                  &
                       3.d0*tmp_mu_2*(-9415.d0 + tmp_mu_2*(76707.d0 +                                                                                                           &
                             4.d0*tmp_mu_2*(143302.d0 + 9.d0*tmp_mu_2*(76662.d0 - 10181.d0*tmp_mu_2 - 39493.d0*tmp_mu_4 + 8212.d0*tmp_mu_6)))) +                                &
                       42.53889242173238d0*tmp_mu*derf(z1/tmp_mu)*                                                                                                              &
                        (-186.d0 - 871.d0*tmp_mu_2 + 21.d0*tmp_c_2*(3.d0 + 10.d0*tmp_mu_2)*                                                                                     &
                           (-53.d0 + 6.d0*tmp_mu_2*(-28.d0 - 55.d0*tmp_mu_2 + 28.d0*tmp_mu_4)) +                                                                                &
                          3.d0*tmp_mu_4*(-5119.d0 + 6.d0*tmp_mu_2*(-5528.d0 - 7793.d0*tmp_mu_2 + 4900.d0*tmp_mu_4)) +                                                           &
                          10.6347231054331d0*tmp_mu*(36.d0 + 21.d0*tmp_c_2*(3.d0 + 10.d0*tmp_mu_2)**2 +                                                                         &
                             7.d0*tmp_mu_2*(33.d0 + 125.d0*(tmp_mu_2 + 6.d0*tmp_mu_4)))*derf(z1/tmp_mu))))))/                                                                   &
             (dexp(z1/tmp_mu_2)*tmp_c_2*                                                                                                                                        &
               (6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                                                                        &
                  dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 +                                                                              &
                     21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu)))**2)))/                                                                                &
        (1.d0 + (0.1428571428571429d0*(6.d0*tmp_mu_2*(30.d0 + 167.d0*tmp_mu_2 + 404.d0*tmp_mu_4 - 576.d0*tmp_mu_6) +                                                            &
                dexp(z1/tmp_mu_2)*(-161.d0 + 6.d0*tmp_mu_2*(-105.d0 - 735.d0*tmp_mu_2 - 980.d0*tmp_mu_4 + 576.d0*tmp_mu_6) +                                                    &
                   21.26944621086619d0*tmp_mu*(15.d0 + 91.d0*tmp_mu_2 + 315.d0*tmp_mu_4)*derf(z1/tmp_mu))))/                                                                    &
            (tmp_c_2*(6.d0*tmp_mu_2*(6.d0 + 17.d0*tmp_mu_2 - 28.d0*tmp_mu_4) +                                                                                                  &
                dexp(z1/tmp_mu_2)*(-35.d0 - 90.d0*tmp_mu_2 - 270.d0*tmp_mu_4 + 168.d0*tmp_mu_6 +                                                                                &
                   21.26944621086619d0*tmp_mu*(3.d0 + 10.d0*tmp_mu_2)*derf(z1/tmp_mu)))))**2                                                                                    

  
    else
     print*, 'Exchange functional required does not exist ...'
     print*,'dirac_exchange_functional',dirac_exchange_functional
     stop
    endif

   ! For very large values of tmp_mu
   elseif (tmp_mu .lt. 1.d+9) then

    !Inverse hexuple for large values of tmp_mu
    e_x = (3.332871861896442d-8*kF_4*(856.d0 - 5292.d0*tmp_mu_2 +                                                            &            
          35.d0*(-68.d0*tmp_c_2 + 74.d0*tmp_c_4 + 15.d0*tmp_c_6 - 135.d0*tmp_c_8 -                                           & 
             9.d0*tmp_c_2*(-56.d0 + 21.d0*tmp_c_2 + 81.d0*tmp_c_4)*tmp_mu_2 -                                                & 
             384.d0*(-2.d0 + 9.d0*(tmp_c_2 + tmp_c_4))*tmp_mu_4) +                                                           & 
          105.d0*tmp_c_4*(dlog(dsqrt(1.d0 + tmp_c_m_2) + 1.d0/tmp_c)*                                                        & 
              (dsqrt(1.d0 + tmp_c_2)*(6.d0 + 45.d0*tmp_c_4 - 90.d0*tmp_mu_2 + 1152.d0*tmp_mu_4 +                             & 
                   tmp_c_2*(-25.d0 + 243.d0*tmp_mu_2)) +                                                                     & 
                9.d0*tmp_c_2*(5.d0*tmp_c_4 + 27.d0*tmp_c_2*tmp_mu_2 + 128.d0*tmp_mu_4)*                                      & 
                 (dlog(tmp_c) - 1.d0*dlog(1.d0 + dsqrt(1.d0 + tmp_c_2)))) -                                                  & 
             1.d0*dsqrt(1.d0 + tmp_c_2)*(6.d0 + 45.d0*tmp_c_4 - 90.d0*tmp_mu_2 + 1152.d0*tmp_mu_4 +                          & 
                tmp_c_2*(-25.d0 + 243.d0*tmp_mu_2))*(dlog(tmp_c) - 1.d0*dlog(1.d0 + dsqrt(1.d0 + tmp_c_2))))))/tmp_mu_6

   ! Inverse hexuple for large values of tmp_mu
    v_x = (-6.578825359288002d-7*kF*((1.d0 + tmp_c_2)*(1.d0 + dsqrt(1.d0 + tmp_c_2))*                             & 
             (5.d0*(-856.d0 + 7.d0*tmp_c_2*(272.d0 - 240.d0*tmp_c_2 + 45.d0*tmp_c_4)) +                           & 
               1512.d0*(14.d0 - 35.d0*tmp_c_2 + 15.d0*tmp_c_4)*tmp_mu_2 +                                         & 
               80640.d0*(-1.d0 + 3.d0*tmp_c_2)*tmp_mu_4) -                                                        & 
            105.d0*tmp_c_4*(1.d0 + tmp_c_2 + dsqrt(1.d0 + tmp_c_2))*                                              & 
             (36.d0 - 70.d0*tmp_c_2 + 15.d0*tmp_c_4 + 72.d0*(-5.d0 + 3.d0*tmp_c_2)*tmp_mu_2 + 2304.d0*tmp_mu_4)*  & 
             dlog((1.d0 + dsqrt(1.d0 + tmp_c_2))/tmp_c)))/                                                        & 
          ((1.d0 + tmp_c_2)*(1.d0 + dsqrt(1.d0 + tmp_c_2))*tmp_mu_6)

   ! Limit for large tmp_mu 
   else
  
    e_x = 0.d0
    v_x = 0.d0 
 
   endif
  endif 
 endif 
 end


