c***********************************************************************
c
c     scenario.inc                                      
c
c     scenario description and common block definitions
c
c     included in:  arcon96, blockdata, init, proc, record, scenread,
c                   summary, xoqcalc5
c
c***********************************************************************

      integer*2 dir1, dir2, rtype, wind_dir, window

      real    barea, dist, f0, mht1, mht2, recht, rht, w0,
     +        z0, sw_cnst, t_diff, srad

      common  /scenario/ dir1, dir2, rtype, wind_dir, window,
     +                   barea, dist, f0, mht1, mht2, recht, rht, w0,
     +                   z0, sw_cnst, t_diff, srad
     