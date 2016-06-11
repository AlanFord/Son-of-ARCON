      subroutine init

c***********************************************************************
c
c     init
c
c     description:  compute diffusion coefficients and wind speed 
c                   adjustment factor that are functions of stability
c                   class and the scenario.
c
c     other modules required:  invmol2, nsigma1
c
c***********************************************************************

      implicit none

      include 'param.inc'
      include 'met.inc'
      include 'scenario.inc'
      include 'units.inc'

      integer   i1

      real      invmol2

      real      invl(7), pi, trht, sigy, sigz, psi,
     +          psi1, psi2, psi10, psirh, x1, x2, y10, yrh

      pi = 3.141593

c     compute variables that are functions of the scenario and 
c     stability but not functions of wind speed or direction

      do i1 = 1,7

c       normal diffusion coefficients

        call nsigma1(dist, i1, sigz0, sigy0, sigz, sigy)
        nsigy(i1) = sigy
        nsigz(i1) = sigz

c       z/l for the wind profile model
     
        invl(i1) = invmol2(i1, z0)
        zol(i1) = 10.0 * invl(i1)

c       wind profile factors from the diabatic profile relationships

        if(i1 .le. 3) then
          x1 = (1. - 16. * mht1 * invl(i1) )**0.25
          y10 = (1. - 16. * 10.0 * invl(i1) )**0.25
          yrh = (1. - 16. * rht * invl(i1) )**0.25

          psi1 = alog( (1.+x1**2)/2. * ( (1.+x1)/2. )**2 )
     +                - 2. * atan(x1) + pi/2
          psi10 = alog( (1. + y10**2)/2. * ( (1.+y10)/2. )**2 )
     +                - 2. * atan(y10) + pi/2
          psirh = alog( (1. + yrh**2)/2. * ( (1.+yrh)/2. )**2 )
     +                - 2. * atan(yrh) + pi/2

        else if(i1 .ge. 5) then 
          psi1 = -5 * mht1 * invl(i1)
          psi10 = -5 * 10.0 * invl(i1)
          psirh = -5 * rht * invl(i1)
        else        
          psi1 = 0.0 
          psi10 = 0.0
          psirh = 0.0 
        endif

        if( mht2 .gt. 0 ) then
          if( i1 .le. 3 ) then
            x2 = (1. - 16. * mht2 * invl(i1) )**0.25
            psi2 = alog( (1.+x2**2)/2. * ( (1.+x2)/2.)**2 )
     +                  - 2. * atan(x2) + pi/2
          else if(i1 .ge. 5) then
            psi2 = -5 * mht2 * invl(i1)
          else 
            psi2 = 0.0
          endif
        endif 

c  **  establish wind speed adjustment factors for each stability 
c      class using diabatic profile (panofsky and dutton, 1984)

c  **     c1 converts spdl to 10 m speed
c         c2 converts spdl to rht speed
c         c3 converts spdu to 10 m speed
c         c4 converts spdu to rht speed

        c1(i1) = ( alog( 10.0 / z0 ) - psi10 )
     +         / ( alog( mht1 / z0 ) - psi1 ) 
        trht = amax1(rht,10.0)
        psi = psi10
        if( rht .gt. 10.0 ) psi = psirh
        c2(i1) = ( alog( trht / z0 ) - psi ) 
     +         / ( alog( mht1 / z0 ) - psi1 )
        if( mht2 .gt. 0 ) then
          c3(i1) = ( alog( 10.0 / z0 ) - psi10 )
     +           / ( alog( mht2 / z0 ) - psi2 )
          c4(i1) = ( alog( trht / z0 ) - psi )
     +           / ( alog( mht2 / z0 ) - psi2 )
        else
          c3(i1) = 0.0
          c4(i1) = 0.0
        endif

      enddo

      if( test_flg) then
        write(qa_unit,'(2x,a)')  'subroutine init'
        write(qa_unit,'(/2x,a,7f8.2)') 'sy= ', (nsigy(i1), i1=1,7)
        write(qa_unit,'(2x,a,7f8.2)')  'sz= ', (nsigz(i1), i1=1,7)
        write(qa_unit,'(/2x,a,7f8.3)')  'c1= ', (c1(i1), i1=1,7)
        write(qa_unit,'(2x,a,7f8.3)')  'c2= ', (c2(i1), i1=1,7)
        write(qa_unit,'(2x,a,7f8.3)')  'c3= ', (c3(i1), i1=1,7)
        write(qa_unit,'(2x,a,7f8.3)')  'c4= ', (c4(i1), i1=1,7) 
      endif

      return

      end
