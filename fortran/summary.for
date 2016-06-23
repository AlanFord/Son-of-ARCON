      subroutine summary

c***********************************************************************
c
c     summary
c
c     description:  subroutine summarizes the x/q data for control room
c                   habitability assessments.  
c
c     other modules required:  xoq95
c 
c***********************************************************************

      implicit none

      include 'param.inc'       
      include 'scenario.inc'
      include 'units.inc'
      include 'xoq.inc'

      integer   i1, i2, i3

      real t1(10), t2(10), sector_f(10), clim, slim, val95(10),
     &     std_xoq, xoq95_2

c     total the distributions for check against total hours of data 

      do i1 = 1,10
        do i2 = 2,101
          t1(i1) = t1(i1) + distr(i2,i1)
        enddo
        t2(i1) = t1(i1) + distr(1,i1) + distr(102,i1) + zero(i1)
        if( t2(i1) .gt. 0.0)  sector_f(i1) = 1.0 - zero(i1) / t2(i1)
      enddo  

      write(log_unit,3) tot_xoq, miss_xoq, in_sect, elevated, calm_xoq,
     +     out_sect
    3 format(/5x,'total number of hours of data processed = ', i6,/
     +        5x,'hours of missing data                   = ', i6,/
     +        5x,'hours direction in window               = ', i6,/
     +        5x,'hours elevated plume w/ dir. in window  = ', i6,/
     +        5x,'hours of calm winds                     = ', i6,/
     +        5x,'hours direction not in window or calm   = ', i6 /)

      write(log_unit,4) 
    4 format(5x, 'distribution summary data by averaging interval')     
      
      write(log_unit,5) na, (10**uplim(i1),i1=1,10), 
     +     (10**lowlim(i1),i1=1,10), (distr(102,i1),i1=1,10), t1,
     +     (distr(1,i1),i1=1,10), zero, t2, sector_f*100. 
    5 format(
     +   3x, ' aver. per.', 2x, 10(6x,i4,1x), 
     +  /3x, ' upper lim.', 2x, 10(1pe10.2,1x),
     +  /3x, '   low lim.', 2x, 10(1pe10.2,1x),
     +  /3x, 'above range', 2x, 10(4x,0pf6.0,1x),
     +  /3x, '   in range', 2x, 10(4x,f6.0,1x),  
     +  /3x, 'below range', 2x, 10(4x,f6.0,1x),
     +  /3x, '       zero', 2x, 10(4x,f6.0,1x),
     +  /3x, ' total x/qs', 2x, 10(4x,f6.0,1x),
     +  /3x, ' % non zero', 2x, 10(4x,f6.2,1x) )

      if( test_flg ) then
        write(qa_unit,'(/2x,a/)') 'subroutine summary'
        write(qa_unit,7)  'x/q frequency distributions',
     +      'xoq', (na(i3),i3=1,4), 'xoq', (na(i3),i3=5,10)
    7   format( 2x,a27,/6x,a3,5x,4(i3,4x),7x,a3,2x,6(3x,i3,3x) )

        write(qa_unit,'(1x, a10,4(0pf7.0),3x,a10,6(0pf9.0))')
     +     ' abv. lim.', (distr(102,i3), i3 = 1,4), 
     +     ' abv. lim.', (distr(102,i3), i3 = 5,10)

        do i1 = 1,100
          i2 = 102 - i1
          clim = 10**(lowlim(1) + (i2-2)/25.)
          slim = 10**(lowlim(3) + (i2-2)/25.) 
          write(qa_unit,8) clim, (distr(i2,i3),i3=1,4), slim,
     +                   (distr(i2,i3),i3 = 5,10)
    8     format(1x,1pe10.3,4(0pf7.0),3x,1pe10.3,6(0pf9.0))
        enddo 

        write(qa_unit,'(1x, a10,4(0pf7.0),3x,a10,6(0pf9.0))')
     +     'belw. lim.', (distr(1,i3), i3 = 1,4), 
     +     'belw. lim.', (distr(1,i3), i3 = 5,10)
      endif

      open( cfd_unit, file=cfd_file, status='new', err=999)

      write(cfd_unit,9)  'x/q cumulative frequency distributions',
     +      'xoq', (na(i3),i3=1,4), 'xoq', (na(i3),i3=5,10)
    9 format( /2x,a39,/6x,a3,5x,4(i3,4x),7x,a3,2x,6(3x,i3,3x) )

      write(cfd_unit,'(1x, a10,4(0pf7.0),3x,a10,6(0pf9.0))')
     +     ' abv. lim.', (distr(102,i3), i3 = 1,4), 
     +     ' abv. lim.', (distr(102,i3), i3 = 5,10)
     
      do i1 = 1,100
        i2 = 102 - i1
        clim = 10**(lowlim(1) + (i2-2)/25.)
        slim = 10**(lowlim(3) + (i2-2)/25.)
        if( i1 .ge. 2 ) then
          do i3 = 1,10
            distr(i2,i3) = distr(i2,i3) + distr(i2+1,i3) 
          enddo
        endif
        write(cfd_unit,8) clim, (distr(i2,i3),i3=1,4), slim,
     +                   (distr(i2,i3),i3 = 5,10)
      enddo 

      write(cfd_unit,'(1x, a10, 4(0pf7.0),3x,a10,6(0pf9.0))')
     +     'belw. lim.', (distr(1,i3), i3 = 1,4), 
     +     'belw. lim.', (distr(1,i3), i3 = 5,10)

      close(cfd_unit)

      call xoq95( lowlim, t2, distr, val95 )

      write(log_unit, '(/5x, a26, /16x, 10(1pe10.2,1x))' )
     +     '95th percentile x/q values', (val95(i3), i3 = 1,10)


c     calculate the 95% x/q values for standard averaging intervals... 
c     0 - 2 hours (1 hour x/q), 2 - 8 hours, etc.... the calculations
c     are based on the 95% concentrations above and the exposures 
c     required to go from one value to the next.  if statement check
c     to see if the averaging periods were changed.  

      write(log_unit, '(/5x,a)')
     +   '95% x/q for standard averaging intervals'

c     0 to 2 hour x/q  ... select larger of the 1 and 2 hr values... 
c     normally the 1 hr value, but with elevated release it can be the 
c     2 hr value if there aren't sufficient 1 hr values.

      xoq95_2 = amax1( val95(1), val95(2) )
      
      if( na(1) .eq. 1 ) 
     +   write(log_unit, '(/5x,a16,2x,1pe10.2)') 
     +   '0 to 2 hours    ', xoq95_2

c     2 to 8 hour x/q
      
      if( (na(1) .eq. 1) .and. (na(4) .eq. 8) ) then
        std_xoq =  (8.0 * val95(4) - 2.0 * xoq95_2) / 6.0
        if( std_xoq .lt. 0.0 ) std_xoq = 0.0
        write(log_unit, '(5x,a16,2x,1pe10.2)') 
     +     '2 to 8 hours    ', std_xoq
      endif                  
      
c     8 to 24 hour x/q

      if( (na(4) .eq. 8) .and. (na(6) .eq. 24) ) then
        std_xoq = (24.0 * val95(6) - 8.0 * val95(4)) / 16.0
        if( std_xoq .lt. 0.0 ) std_xoq = 0.0
        write(log_unit, '(5x,a16,2x,1pe10.2)') 
     +   '8 to 24 hours   ', std_xoq
      endif

c     1 to 4 day x/q

      if( (na(6) .eq. 24) .and. (na(7) .eq. 96) ) then
        std_xoq = (4.0 * val95(7) - 1.0 * val95(6)) / 3.0
        if( std_xoq .lt. 0.0 ) std_xoq = 0.0
        write(log_unit, '(5x,a16,2x,1pe10.2)') 
     +     '1 to 4 days     ', std_xoq
      endif

c     4 to 30 day x/q
      
      if( (na(7) .eq. 96) .and. (na(10) .eq. 720) ) then
        std_xoq = (30.0 * val95(10) - 4.0 * val95(7)) / 26.0
        if( std_xoq .lt. 0.0 ) std_xoq = 0.0
        write(log_unit, '(5x,a16,2x,1pe10.2)') 
     +     '4 to 30 days    ', std_xoq
      endif

      write(log_unit,6) clmax, clmin, samax, samin
    6 format(/32x 'hourly value range ',/
     +       27x,'   max x/q  ',10x,'   min x/q  ',/ 10x, 'centerline',
     +       5x,1pe12.2,10x,1pe12.2/,10x, 'sector-average',1x,1pe12.2,
     +       10x,1pe12.2)

      return

  999 write( log_unit, '(/2x,a,a)' )
     +   'unable to open cum. distribution file = ', cfd_file
      stop 'unable to open cdf file... check for existing name'

      end
