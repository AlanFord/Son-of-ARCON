      subroutine xoq95( lowlim, t2, distr, val95 )

c***********************************************************************
c
c     xoq95
c
c     description:   calculate the 95% x/q from the cumulative frequency
c                    distributions for each averaging interval.
c
c     other modules required:  none
c 
c***********************************************************************

      implicit none

      include  'param.inc'
      include  'units.inc'

      integer  ib, j

      real     lowlim(10), t2(10), distr(102,10),
     +         llim, uplim, hrs95, val95(10)

      do j = 1,10

        if( t2(j) .lt. 20. ) then

c  **   insufficient averages for averaging period j to calculate 95% 
c       value

          val95(j) = 0.0
          cycle
        endif

c  **   determine 95% of total possible averaging periods

        hrs95 = 0.05 * t2(j)

c  **   first check to see if 95th percentile is above range
c       (pathological case)

        if( hrs95 .le. distr(102,j) ) then 

          write( log_unit, '(/2x,a,i2,a)' )
     +      '95th percentile value for averaging interval ', j, 
     +      ' is out of range'       

c  **     return negative value   

          val95(j)  =  -99999

        else if( hrs95 .gt. distr(2,j) ) then

c  **   95th percentile value is below range... assume lower end of range

          val95(j) = 10**lowlim(j)
 
        else

c  **   95% value is in expected range
c  **   find the bin containing the 95% value

          ib = 99
          do while( (distr(ib+2,j) .le. hrs95) .and. (ib .ge. 1) )
            ib = ib - 1
          enddo

c  **     convert ib to the upper and lower limits of band containing 
c         the 95% value

          llim = 10**(lowlim(j) + ib/25.)
          uplim = 10**(lowlim(j) + (ib+1)/25.) 

          if( distr(ib+2,j) .gt. hrs95 ) then
            val95(j) = llim + (uplim - llim) * (distr(ib+2,j) - hrs95)
     +                                / (distr(ib+2,j) - distr(ib+3,j)) 
          else
            val95(j) = llim
          endif

        endif

      enddo 

      return
      end
