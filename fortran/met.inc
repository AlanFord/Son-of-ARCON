c***********************************************************************
c
c     met.inc
c
c     meteorological data type and common block definitions
c
c     included in:  arcon96, blockdata, init, metread, record, scenread,
c                   shift, xoqcalc5
c
c***********************************************************************

      logical*1 end_met

      integer nmetfile, spd_type, stab(maxhours)

      integer*2 dirl(maxhours), diru(maxhours), spdl(maxhours),
     +          spdu(maxhours), julday(maxhours), chour(maxhours)

      integer   first_met, last_met, next_met, tot_met

      real      c1(7), c2(7), c3(7), c4(7) ,cf(3), nsigy(7), nsigz(7),
     +          nsigy2(7), nsigz2(7), tsigy, tsigz, ustar, zol(7), umin,
     +          sigy0, sigz0

      common    /metdata/ end_met, nmetfile, spd_type, stab, dirl, diru, 
     +                    spdl, spdu, julday, first_met, last_met,
     +                    next_met, tot_met, umin, c1, c2, c3, c4, cf, 
     +                    nsigy, nsigz, nsigy2, nsigz2, tsigy, tsigz, 
     +                    ustar, zol, chour, sigy0, sigz0
