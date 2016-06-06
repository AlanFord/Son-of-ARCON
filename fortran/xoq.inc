c***********************************************************************
c
c     xoq.inc
c
c     x/q related variables and common block definitions
c
c     included in:  arcon96, blockdata, metread, proc, record, scenread
c                   shift, summary, xoqcalc5
c
c***********************************************************************

      logical   limits

      integer   first_xoq, last_xoq, tot_xoq, miss_xoq, na(10), 
     +          navmin(10), calm_xoq, in_sect, out_sect, elevated

      real      clmax, clmin, clxoq(maxhours), samax, samin,
     +          saxoq(maxhours), lowlim(10), uplim(10),
     +          distr(102,10), sum(10), zero(10)

      common /xoq1/ na, navmin, first_xoq, last_xoq, tot_xoq,
     +              miss_xoq, clmax, clmin, clxoq, limits, calm_xoq,
     +              in_sect, out_sect, elevated

      common /xoq2/ samax, samin, saxoq, lowlim, uplim, distr, zero, sum
