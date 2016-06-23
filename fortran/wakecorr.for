      subroutine wakecorr( area, dist, ubar, dsy, dsz )

c***********************************************************************
c
c     wakecorr
c
c     description:  wakecorr computes adjustments to the nrc diffusion
c                   coefficients to account for building wakes.  these
c                   adjustments, described in pnl-10286 (ramsdell 1995), 
c                   are most significant in high wind speed conditions.  
c                   the numerical coefficients in the model are based on 
c                   turbulence and wake diffusion data.  
c
c     input:        area    ==>  building cross-sectional area (m^2)
c                   dist    ==>  "slant range" distance from 
c                                 release point to the receptor (m)
c                   ubar    ==>  release height wind speed (m/s)
c
c     output:       dsy     ==>  adjustment to the nrc lateral diffusion
c                                coefficient to account for the wake (m)
c                   dsz     ==>  adjustment to the nrc vertical diffusion
c                                coefficient to account for the wake (m)
c
c     other modules required: none
c       
c***********************************************************************

      implicit   none

      real       area, dist, ubar, dsy, dsz, hts, vts, tt
 
c     time scale for diffusion, in seconds  ==>  hts, vts
    
      hts = 10 * sqrt( area ) / ubar
      vts = hts 

c     travel time = dist / ubar  ==> tt

      tt = dist / ubar

c     compute adjustments to nrc diffusion coefficients

      dsy = 
     +  sqrt(5.24e-2 * ubar**2 * area * (1.0-(1.0+tt/hts)*exp(-tt/hts))) 

      dsz = 
     +  sqrt(1.17e-2 * ubar**2 * area * (1.0-(1.0+tt/vts)*exp(-tt/vts)))

      return
      end
