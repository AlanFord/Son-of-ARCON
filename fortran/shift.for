      subroutine shift

c***********************************************************************
c
c     shift
c
c     description:  subroutine to shift meteorological data and hourly
c                   x/q values from the end of the storage arrays to the 
c                   beginning before reading the next set of meteorological
c                   data. 
c
c     other modules required:  none
c
c***********************************************************************

      implicit none

      include 'param.inc'
      include 'met.inc'
      include 'units.inc'
      include 'xoq.inc'

      integer   i1, j0, j1, met_shift

c     move residual met. data and x/q's to front of data arrays

      met_shift = na(10) - 1
      j0 = maxhours - met_shift

      do i1 = 1,met_shift
        j1 = j0 + i1
        stab(i1) = stab(j1)
        dirl(i1) = dirl(j1)
        spdl(i1) = spdl(j1)
        diru(i1) = diru(j1)
        spdu(i1) = spdu(j1)
        clxoq(i1) = clxoq(j1)
        saxoq(i1) = saxoq(j1)
      enddo
    
      first_met = met_shift + 1

      if( test_flg) 
     +   write(log_unit,'(5x,a,i6)') ' first_met: ', first_met

      return

      end
