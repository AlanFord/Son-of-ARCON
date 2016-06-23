      subroutine proc

c***********************************************************************
c
c     proc                                             
c
c     description:  processes x/q values for control room habitability
c                   assessments.
c
c     other modules required:  none
c 
c***********************************************************************

      implicit none

      include 'param.inc'
      include 'scenario.inc'
      include 'units.inc'
      include 'xoq.inc'

      integer  i1, i2, i3, indx(10), j, nave(10), nave_tot, cl_nave(10), 
     &         sa_nave(10)         

      real     cluplim, sauplim, v1, v2, ave(10), ratio, minv(10), 
     &         tot_sum, cl_sum(10), sa_sum(10), v1a, v1b, v2a, v2b 

      save nave, ave                        
      
c     determine upper and lower limits for x/q distributions from 
c     maximum values in the first meteorological data set

      if ( limits ) then

         cluplim = aint( alog10( clmax ) )
         do i1 = 1,4
            uplim(i1) = cluplim
            lowlim(i1) = cluplim - 4.0
            minv(i1) = 10**(uplim(i1) - 5.0)
         enddo     
         sauplim = aint( alog10( (8*clmax + 4*samax)/12.0 ) )
         do i1 = 5,10
            uplim(i1) = sauplim
            lowlim(i1) = sauplim - 4.0
            minv(i1) = 10**(uplim(i1) - 5.0)
         enddo       
         limits = .false.
      endif
      
c     generate frequency distributions 

      if ( test_flg ) then
         write(qa_unit,'(/2x,a)')  'subroutine proc'
         write(qa_unit,'(3(/2x,a,1pe12.3,2x,a,1pe12.3))')
     +     ' clmax: ',clmax,' cluplim: ', 10.0**cluplim,
     +     ' samax: ',samax,' sauplim: ', 10.0**sauplim,
     +     ' lowlim(1): ', 10.0**lowlim(1),
     +     ' lowlim(3): ', 10.0**lowlim(3)
         write(qa_unit, '(/5x, 10i5 )' ) (na(i2), i2=1,10)
         write(qa_unit, '(5x, 10i5 )' ) (navmin(i2), i2=1,10)
      endif

c  **  go through the entire set of x/q values
     
      do i1 = first_xoq,last_xoq

c  **    do all averaging intervals

         do i2 = 1,10  

c  **       find the values to be added and deleted from the sums used to
c           compute the averages... add v1 prior to computing averages and
c           delete v2 after computing the averages 
 
            v2 = -1.0
            v1a = -1.0
            v2a = -1.0
            v2b = -1.0
            if ( i2 .le. 4 ) then
               v1 = clxoq(i1)
               if ( i1 .ge. na(i2) ) v2 = clxoq(i1 - na(i2) + 1 )
            else 
               v1b = saxoq(i1)
               if ( i1 .ge. (na(i2)-8) ) v2b = saxoq(i1-na(i2)+9)
               if ( i1 .ge. (na(i2)-7) ) v1a = clxoq(i1-na(i2)+8)
               if ( i1 .ge. na(i2) ) v2a = clxoq(i1 - na(i2) + 1)
            endif   

c  **      check for good values -- add to sums and increment counters if 
c          good values
     
            if ( i2 .le. 4) then 
               if (v1 .ge. 0.0 ) then
                  sum(i2) = sum(i2) + v1
                  nave(i2) = nave(i2) + 1
               endif   
            else    
               if (v1a .ge. 0.0 ) then
                  cl_sum(i2) = cl_sum(i2) + v1a
                  cl_nave(i2) = cl_nave(i2) + 1
               endif     
               if (v1b .ge. 0.0 ) then
                  sa_sum(i2) = sa_sum(i2) + v1b
                  sa_nave(i2) = sa_nave(i2) + 1
               endif                 
            endif
                      
c  **      check for sufficient number of values to compute an acceptable
c          average 

            if ( i2 .le. 4 ) then
               nave_tot = nave(i2)
               tot_sum = sum(i2)
            else            
               nave_tot = cl_nave(i2) + sa_nave(i2)
               tot_sum = cl_sum(i2) + sa_sum(i2)
            endif     

            if ( (i1 .ge. navmin(i2)) .and. (nave_tot .ge. navmin(i2)) )
     +         then

c  **       check for elevated plume, increment counter if plume elvated

               if ( (i2 .eq. 1) .and. 
     +            ( v1 .gt. 0.0 ) .and.          
     +            ( v1 .le. minv(1) ) ) elevated = elevated + 1

c  **       sufficient number

               if ( tot_sum .gt. (i2 * minv(i2)) ) then

c  **         compute average 

                  ave(i2) = tot_sum / nave_tot

c  **         compute index for bin,  bin size = 1/25 order of mag.

                  ratio = alog10( ave(i2) ) - lowlim(i2)   
                  if ( ratio .ge. 0.0 ) then
                     i3 = ifix( 25 * ratio ) + 2
                  else
                     i3 = 1
                  endif
                  indx(i2) = i3

c  **         accumulate in proper bin

                  if ( i3 .ge. 2 ) then
                     if ( i3 .le. 101 ) then
                        distr(i3,i2) = distr(i3,i2) + 1.0
                     else
                        distr(102,i2) = distr(102,i2) + 1.0
                     endif
                  else
                     distr(1,i2) = distr(1,i2) + 1.0
                  endif
               else

c  **         not necessary to compute average, sum = 0

                  zero(i2) = zero(i2) + 1.
                  sum(i2) = 0.0 
               endif
            endif
 
c  **     remove oldest values if not missing and decrement counters 

            if ( i2. le. 4 ) then         
               if ( (i1 .ge. na(i2) ) .and.
     +            (v2 .ne. -1.) ) then
                  sum(i2) = sum(i2) - v2
                  nave(i2) = nave(i2) - 1
               endif
               if ( sum(i2) .lt. minv(i2) )  sum(i2) = 0.0        
            else              
               if ( i1 .ge. na(i2) ) then
                  if ( v2a .ne. -1 ) then
                     cl_sum(i2) = cl_sum(i2) - v2a
                     cl_nave(i2) = cl_nave(i2) - 1
                  endif
                  if( cl_sum(i2) .lt. minv(i2) ) cl_sum(i2) = 0.0 
               endif                  
               if ( i1 .ge. (na(i2)-8) ) then      
                  if ( v2b .ne. -1 ) then
                     sa_sum(i2) = sa_sum(i2) - v2b
                     sa_nave(i2) = sa_nave(i2) - 1
                  endif
                  if( sa_sum(i2) .lt. minv(i2) ) sa_sum(i2) = 0.0               
               endif           
            endif
           
         enddo 

         do j = 1,10
            ave(j) = 0.0
            indx(j) = 0
         enddo

      enddo

      return
      end
