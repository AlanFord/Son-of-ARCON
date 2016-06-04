      subroutine curvefix( dist, ubar, dsy, dsz )

c***********************************************************************
c
c     curvefix
c
c     description:  curvefix computes wake dispersion coefficient
c                   increments for low wind speed conditions.  these 
c                   increments correct the nrc diffusion curves for 
c                   for systematic underestimation of concentrations 
c                   measured in wake diffusion studies under low wind
c                   speeds.  the correction and its derivation is 
c                   described in pnl-10286 (ramsdell 1995).  
c
c     input:        dist    ==>  "stretched string" distance from release
c                                point to receptor (m)
c                   ubar    ==>  release height wind speed (m/s)
c
c     output:       dsy     ==>  correction to the nrc lateral diffusion
c                                coefficient (m)
c                   dsz     ==>  correction to the nrc vertical diffusion
c                                coefficient (m)
c
c     other modules required:  erfcc
c
c***********************************************************************

      implicit   none

      real       dist, ubar, dsy, dsz, hts, tt, vts
 
c     horizontal time scale for meander in seconds  ==>  hts 

      hts = 1000.0

c     vertical time scale for meander in seconds  ==>  vts

      vts = 100.0

c     travel time = dist / ubar  ==> tt

      tt = dist / ubar

c     compute low wind speed diffusion coefficient corrections

      dsy = sqrt( 9.13e5 * (1.0 - (1.0+tt/hts) * exp(-tt/hts)) ) 

      dsz = sqrt( 6.67e2 * (1.0 - (1.0+tt/vts) * exp(-tt/vts)) )

      return
      end
