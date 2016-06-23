      subroutine windyxoq( rtype, area, dist, eff_ht, stab, ubar, sigy,  
     +           sigz, sigz0, sigy0, flow, w0, sw_const, clxoq, saxoq )

c***********************************************************************
c
c     windyxoq
c
c     description:  windyxoq computes x/q in windy conditions.  for elevated 
c                   releases it uses the standard gaussian model.  for 
c                   releases in the vicinity of buildings is uses the model 
c                   described in pnl-10286 (ramsdell and fosmire 1995).  
c                   for elevated vent releases (rtype = 2), the model uses 
c                   the split-h approach of reg. guide 1.111. 
c
c     input:        rtype      ==>   release type 1 = ground, 2 = vent,
c                                      3 = stack
c                   area       ==>   building cross-sectional area (m^2) 
c                   dist       ==>   distance from release point to 
c                                      intake (m)
c                   eff_ht     ==>   effective release height (m)
c                   stab       ==>   stability class
c                   ubar       ==>   wind speed at release height (m)
c                   sigy       ==>   nrc lateral diffusion coefficient
c                                    for the stability class and 
c                                    distance (m)
c                   sigz       ==>   nrc vertical diffusion coefficient
c                                    for the stability class and 
c                                    distance (m)
c                   flow       ==>   release flow rate (m^3/s)
c                   w0         ==>   stack effluent velocity (m/s)
c                   sw_const   ==>   assumed width of plume in units of
c                                    the adjusted sigma y (dimensionless)
c     
c     output:       clxoq      ==>   normalized concentration at center
c                                    line of plume (s/m^3)
c                   saxoq      ==>   normalized concentration in a 
c                                    sector-average plume 
c
c     other modules required:   curvefix, wakecorr, nsigma
c                       
c***********************************************************************

      implicit    none

      integer*2   rtype

      integer     stab

      real        area, dist, dsy1, dsz1, dsy2, dsz2, flow, pi, sigy, 
     +            sigz, tsigy, tsigz, ubar, max_wide, eff_ht, w0, wr, wf  

      real        clxoq, saxoq, pwidth, sw_const, sqrt2pi, clxoqw, 
     &            clxoqs, saxoqw, saxoqs, maxsigy, srange, nsy, nsz,
     &            sigz0, sigy0

      pi = 3.14159
      sqrt2pi = sqrt( 2 * pi )

      if( rtype .ge. 2 ) then

c     calculate the normalized concentration at plume centerline
c     for a stack release

        clxoqs = exp(-0.5 * (eff_ht / sigz)**2) /
     &            (pi * ubar * sigy * sigz)
    
c     adjust for finite flow

        if( (clxoqs .gt. 1.0e-10) .and. (flow .gt. 0.0) )
     &         clxoqs = 1.0 / ( 1.0 / clxoqs + flow )


c     determine plume width for sector average... 0.3927 = (2 pi) / 16

        max_wide = amin1( (2*pi * dist), (sw_const * sigy) )
        pwidth = amax1( max_wide, (0.3927 * dist) )

c     calculate the normalized sector average concentration

        saxoqs = 2.0 * exp( -0.5 *(eff_ht/sigz)**2) / 
     &            (sqrt2pi * pwidth * sigz * ubar)

c     adjust for finite flow 

        if( (saxoqs .gt. 1.0e-10) .and. (flow .gt. 0.0) ) 
     &          saxoqs = 1.0 / ( 1.0 / saxoqs + flow ) 
     
      endif

      if( rtype .le. 2 ) then

c     calculate slant range between release point and receptor

        srange = sqrt( dist**2 + eff_ht**2 )

c     check to see if slant range is < 1% greater than dist.  if it is
c     use existing nrc diffusion coefficients.

        if( srange .lt. 1.01 * dist ) then
          nsy = sigy
          nsz = sigz
        else  

c     calculate nrc diffusion coefficients for slant range

          call nsigma1( srange, stab, sigz0, sigy0, nsz, nsy )

        endif

c     get low wind speed correction for the nrc diffusion coefficients

        call curvefix( srange, ubar, dsy1, dsz1 )

c     get the building wake correction

        call wakecorr( area, srange, ubar, dsy2, dsz2 )

c     calculate the adjusted diffusion coefficients

        tsigy = sqrt( nsy**2 + dsy1**2 + dsy2**2 )
        tsigz = sqrt( nsz**2 + dsz1**2 + dsz2**2 )

c     limit tsigy so that tsigy <= 2pi srange / sqrt(12)  (sigma y for 
c     a uniform distribution across a line perpendicular to the plume
c     axis with length equal to the circumference of a circle with
c     radius srange

        maxsigy = 2.0 * pi * srange / sqrt( 12. )
        tsigy = amin1( tsigy, maxsigy )

c     calculate the normalized concentration at plume centerline
c     for the entrained portion of the plume

        clxoqw = 1.0 / ( pi * ubar * tsigy * tsigz )

c     adjust for finite flow

        if( (clxoqw .gt. 1e-10) .and. (flow .gt. 0.0) ) 
     &    clxoqw = 1.0 / ( 1.0 / clxoqw + flow )

    
c     determine plume width for sector average... 0.3927 = (2 pi) / 16

        max_wide = amin1( (2*pi * srange), (sw_const * tsigy) )
        pwidth = amax1( max_wide, (0.3927 * srange) )

c     calculate the normalized sector average concentration

        saxoqw = 2.0 / (sqrt2pi * pwidth * tsigz * ubar )

c     adjust for finite flow

        if( (saxoqw .gt. 1e-10) .and. (flow .gt. 0.0) )
     &    saxoqw = 1.0 / ( 1.0 / saxoqw + flow ) 

      endif

      if( rtype .eq. 1 ) then           ! ground-level release 
     
        clxoq = clxoqw
        saxoq = saxoqw

      else if( rtype .eq. 3 ) then      ! stack release

        clxoq = clxoqs
        saxoq = saxoqs

      else if( rtype .eq. 2 ) then      ! elevated vent release

c     vent release... calculate fraction entrained in the building
c     wake using regulatory guide 1.111 method.

        wr = w0 / ubar

        if( wr .le. 1.0 ) then  
          wf = 1.0
        else if( wr .le. 1.5 ) then
          wf = 2.58 - 1.58 * wr
        else if( wr .lt. 5.0 ) then
          wf = 0.3 - 0.06 * wr 
        else
          wf = 0.0
        endif

c     calculate x/q using entrained fraction
       
        clxoq = (1.0 - wf) * clxoqs + wf * clxoqw
        saxoq = (1.0 - wf) * saxoqs + wf * saxoqw

      else                              ! unknown release

        stop 'unknown release type'

      endif

      return
      end
