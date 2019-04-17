!************************************************************************
!     subroutine dftfun_exerfpbe(fderiv,rhoc,
!    >                   sigmacc,,
!    >                   zk,vrhoc,
!    >                   vsigmacc)
!************************************************************************
!     Short-range PBE exchange energy functional for erf interaction
!
!
!************************************************************************
!     implicit none

! input
!     logical fderiv
!     double precision rhoc
!     double precision sigmacc

! output
!     double precision zk
!     double precision vrhoc
!     double precision vsigmacc

! function
!     double precision berf
!     double precision dberfda

! local
!     double precision tol
!     parameter(tol=1d-12)

!     double precision zkxerflda
!     double precision vrhocxerflda
!     double precision vrhooxerflda

!     integer i
!     integer izk, icorr
!     double precision mu
!     double precision rho,drho2
!     double precision exerflda,dexerfldadrho
!     double precision exerfpbe,dexerfpbedrho,dexerfpbeddrho2
!     double precision t1,t2,t3,t4
!     double precision kappa,sq,sqs,sqss,fx,fxs,ksig
!     double precision q
!     double precision, parameter :: f13=0.333333333333333d0

!     izk=icorr(npt)

! Parameter of the modified interaction
!     mu = mu_erf

! Compute LDA part
!     do i=1,npt
!        zkxerflda(i)=0d0
!        vrhocxerflda(i)=0d0
!        vrhooxerflda(i)=0d0
!     end do

! call spin-independent LDA exchange functional
!     call dftfun_exerf(fderiv,rhoc,rhoo,
!    >                   zkxerflda,vrhocxerflda,vrhooxerflda)


!        density
!        rho = rhoc
!        square of density gradient
!        drho2 = sigmacc

!        test on density
!        if (rho.lt.tol) goto 1

!        LDA energy density
!        exerflda = zkxerflda

!        kappa=0.804d0
!        sq=drho2*2.6121172985233599567768d-2*rho**(-8d0/3d0)
!        fx=1d0+kappa
!    &    -kappa/(1d0+berf(1.616204596739954813d
!    &    -1*mu*rho**(-f13))*sq/kappa)
!        exerfpbe=exerflda*fx

!        zk = exerfpbe

! Derivative

!        if (fderiv) then

!        LDA energy density derivative
!        dexerfldadrho = vrhocxerflda

!        sqs=-8d0*sq/(3d0*rho)
!        fxs=kappa**2*(-1.616204596739954813d-1*mu*rho**(-4d0*f13)/3d0
!    &    *dberfda(1.616204596739954813d-1*mu*rho**(-f13))*sq
!    &    +berf(1.616204596739954813d-1*mu*rho**(-f13))*sqs)
!    &    /(kappa+berf(1.616204596739954813d-1*mu*rho**(-f13))*sq)**2
!        dexerfpbedrho=dexerfldadrho*fx+exerflda*fxs
!        sqss=2.6121172985233599567768d-2*rho**(-8d0/3d0)
!        dexerfpbeddrho2=exerflda*berf(1.616204596739954813d-1*mu
!    &    *rho**(-1.d0/3.d0))*sqss*kappa**2/(kappa
!    &    +berf(1.616204596739954813d-1*mu*rho**(-1.d0/3.d0))*sq)**2

!      derivatives
!      vrhoc = dexerfpbedrho
!      vsigmacc = dexerfpbeddrho2


!     End of derivative
!     end if




!     return
!     end

!-------------------------------------------
!     function berf(a)
!-------------------------------------------
!  Second-order exchange gradient expansion coefficient for erf
!  interaction
!  a = mu/(2*kF)
!
!  Author : J. Toulouse
!  Date   : 10-03-04
!-------------------------------------------
!     implicit none

!     double precision a
!     double precision eta,fak,berf,berf_dexp
!     include "common/cpi"

! function
!     double precision dderf

!     eta=19.0d0
!     fak=2.540118935556d0*dexp(-eta*a*a)

!     if(a .lt. 0.075d0) then
!      expansion for small mu to avoid numerical problems
!      denominator becomes zero for a approximately 0.4845801308
!      (and for one negative and two complex values of a)
!      berf = (-7d0+72.d0*a*a)
!    >        /(27.d0*(-3d0-24.d0*a*a+32.d0*a**4+8d0*dsqrt(pi)*a))

!     else if(a .gt. 50.d0) then
!      berf = 1.d0/(72.d0*a*a)-1.d0/(17280.d0*a**4)
!    >        - 23.d0/(358400.d0*a**6)

!     else


!      Code generated by Mathematica
!      berf_dexp=dexp(2.5d-1/a**2)
!      berf = (1.851851851851851851851852d-2*(-1.d0 + 1.44d2*a**4*(-1.d0
!    &   + berf_dexp) - 2.d0*a**2*(1.1d1 + 7.d0*berf_dexp
!    &  )))/(a**2*(3.2d1*a**4*(-1.d0 + berf_dexp) - 3.d0*berf_dexp
!    &  + 1.417963080724412821838534d1*a*dderf(5.d-1/a)*berf_dexp
!    &  - 8.d0*a**2*(-2.d0 + 3.d0*berf_dexp)))

!     end if

!     berf=berf*fak

!     return
!     end

!-------------------------------------------
!     function dberfda(a)
!-------------------------------------------
!  Derivative of second-order exchange gradient
!  expansion coefficient for erf interaction
!  a = mu/(2*kF)
!
!  Author : J. Toulouse
!  Date   : 10-03-04
!-------------------------------------------
!     implicit none

!     double precision a
!     double precision eta,fak,dfakda,berf,dberfda,berf_dexp
!     double precision t1,t2,tdexp,t3,t4,t5
!     include "common/cpi"

! function
!     double precision dderf

!     eta=19.0d0
!     fak=2.540118935556d0*dexp(-eta*a*a)
!     dfakda=-2.0d0*eta*a*fak

!     if(a .lt. 0.075d0) then
!      expansion for small mu to avoid numerical problems
!      denominator becomes zero for a approximately 0.4845801308
!      (and for one negative and two complex values of a)
!      berf = (-7d0+72.d0*a*a)
!    >        /(27.d0*(-3d0-24.d0*a*a+32.d0*a**4+8d0*dsqrt(pi)*a))
!      dberfda = (8d0*(-96.d0*a + 112.d0*a**3 - 576.d0*a**5
!    >  + 7d0*dsqrt(pi) + 72.d0*a**2*dsqrt(pi)))/
!    >  (27.d0*(3d0 + 24.d0*a**2 - 32.d0*a**4 - 8d0*a*dsqrt(pi))**2)

!     else if(a .gt. 50.d0) then
!      berf = 1.d0/(72.d0*a*a)-1.d0/(17280.d0*a**4)
!    >        - 23.d0/(358400.d0*a**6)
!      dberfda = - 1.d0/(36.d0*a**3) +  1.d0/(4320.d0*a**5)
!    >        + 69.d0/(179200.d0*a**7)


!     else

!      Code generated by Mathematica
!      berf_dexp=dexp(2.5d-1/a**2)
!      berf = (1.851851851851851851851852d-2*(-1.d0 + 1.44d2*a**4*(-1.d0
!    &   + berf_dexp) - 2.d0*a**2*(1.1d1 + 7.d0*berf_dexp
!    &  )))/(a**2*(3.2d1*a**4*(-1.d0 + berf_dexp) - 3.d0*berf_dexp
!    &  + 1.417963080724412821838534d1*a*dderf(5.d-1/a)*berf_dexp
!    &  - 8.d0*a**2*(-2.d0 + 3.d0*berf_dexp)))

!      tdexp=dexp(2.5d-1/a**2)
!      t1 = (1.851851851851851851851852d-2*(5.76d2*a**3*(-1.d0 + tdexp
!    &  ) + (7.d0*tdexp)/a - 7.2d1*a*tdexp
!    &  - 4.d0*a*(1.1d1 + 7.d0*tdexp)))/(a**2*(3.2d1*a*
!    &  *4*(-1.d0 + tdexp) - 3.d0*tdexp + 1.4179
!    &  63080724412821838534d1*a*dderf(5.d-1/a)*tdexp - 8.d0
!    &  *a**2*(-2.d0 + 3.d0*tdexp)))
!      t2 = -1.851851851851851851851852d-2/a**2
!      t3 = -8.d0/a + 1.28d2*a**3*(-1.d0 + tdexp) + (1.5d0*
!    &  tdexp)/a**3 + (1.2d1*tdexp)/a - 1.6d1*a*
!    &  tdexp + 1.417963080724412821838534d1*dderf(5.d-1/a)*
!    &  tdexp - (7.08981540362206410919267d0*dderf(5.d-1/a)*
!    &  tdexp)/a**2 - 1.6d1*a*(-2.d0 + 3.d0*tdexp
!    &  )
!      t4 = (-1.d0 + 1.44d2*a**4*(-1.d0 + tdexp) - 2.d0*a**2
!    &  *(1.1d1 + 7.d0*tdexp))/(3.2d1*a**4*(-1.d0 + tdexp
!    &  ) - 3.d0*tdexp + 1.417963080724412821838534
!    &  d1*a*dderf(5.d-1/a)*tdexp - 8.d0*a**2*(-2.d0 + 3.d0*
!    &  tdexp))**2
!      t5 = (-3.703703703703703703703704d-2*(-1.d0 + 1.44d2*a**4*(-1.d0
!    &  + tdexp) - 2.d0*a**2*(1.1d1 + 7.d0*tdexp
!    &  )))/(a**3*(3.2d1*a**4*(-1.d0 + tdexp) - 3.d0*tdexp
!    &  + 1.417963080724412821838534d1*a*dderf(5.d-1/a)*tdexp
!    &  - 8.d0*a**2*(-2.d0 + 3.d0*tdexp)))
!      dberfda = t1 + t2*t3*t4 + t5

!     end if

!     dberfda=dberfda*fak+berf*dfakda

!     return
!     end
