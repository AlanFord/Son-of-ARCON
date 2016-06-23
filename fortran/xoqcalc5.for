      subroutine xoqcalc5

c***********************************************************************
c
c     xoqcalc5
c
c     description:  computes hourly centerline and sector-average x/q 
c                   for control room habitability assessments using the
c                   final version of the 1995 model for diffusion in
c                   the vicinity of buildings.  
c
c     other modules required:
c 
c        calls:        windyxoq, curvefix, wakecorr
c
c***********************************************************************

      implicit   none

      include   'param.inc'
      include   'met.inc' 
      include   'scenario.inc'
      include   'units.inc'
      include   'xoq.inc'

      integer*2  i1, dir

      real       pi, rhu, u10, testht, cxoq, sxoq, eff_ht, dwash

      pi = 3.141593

c  ** add space to qa file if qa output selected
       
      if( test_flg ) write(qa_unit,'(/2x,a/)') 'subroutine xoqcalc5'

      do i1 = first_met, last_met

c  **  set rhu and u10 to missing

        u10 = 999.0
        rhu = 999.0

c  **  see if stability class missing ... if it is skip x/q calculations

        if( (stab(i1) .lt. 1) .or. (stab(i1) .gt. 7) ) then

c  **     stability class missing

          clxoq(i1) = -1.0
          saxoq(i1) = -1.0
          miss_xoq = miss_xoq + 1

c  **     if qa output selected write out status 

          if( test_flg ) then
            write(qa_unit,'(2x,i5,2x,i2,30x,2(1pe10.2))') i1, stab(i1),
     &            clxoq(i1), saxoq(i1)
          endif
            
          cycle
        endif

c  **   check for missing wind speeds

        if( (spdl(i1) .eq. 9999) .and. (spdu(i1) .eq. 9999) ) then

c  **     wind speeds both missing no x/q calculations possible

          clxoq(i1) = -1.0      
          saxoq(i1) = -1.0
          miss_xoq = miss_xoq + 1

c  **     if qa output selected write out status 

          if( test_flg ) then
            write(qa_unit,'(2x,i5,2x,i2,2x,2f6.2,16x,2(1pe10.2))') 
     &                i1, stab(i1), u10, rhu, clxoq(i1), saxoq(i1)
          endif
 
          cycle

        else if( (spdl(i1) .ne. 9999) .and. (spdu(i1).ne.9999 )) then

c  **     both wind speeds good convert speed entries to m/s

          u10 = (spdl(i1) / 10.0) * cf( spd_type ) * c1(stab(i1))       
 
c  **     convert upper speed

          testht = ( mht1 + mht2 ) / 2.0
          if( rht .le. testht ) then
            rhu = (spdl(i1) / 10.0) * cf( spd_type ) * c2(stab(i1))
          else
            rhu = (spdu(i1) / 10.0) * cf( spd_type ) * c4(stab(i1)) 
          endif
          
        else if( spdl(i1) .ne. 9999 ) then

c  **     lower level good but not upper level

          u10 = (spdl(i1) / 10.0) * cf( spd_type ) * c1(stab(i1))
          rhu = (spdl(i1) / 10.0) * cf( spd_type ) * c2(stab(i1)) 

        else if( spdu(i1) .ne. 9999 ) then

c  **     upper speed good but not lower speed

          u10 = (spdu(i1) / 10.0) * cf( spd_type ) * c3(stab(i1))
          rhu = (spdu(i1) / 10.0) * cf( spd_type ) * c4(stab(i1))
      
        endif

c  **   check to see if calm

        if( ((rtype .le. 2) .and. (u10 .lt. umin))  .or.
     +      ((rtype .ge. 2) .and. (rhu .lt. umin)) )
     +      then

c  **     calm winds

          u10 = 0.0
          rhu = 0.0

c  **  see if wind directions both missing ... if they are skip 

        else if( (dirl(i1) .eq. 999) .and. (diru(i1) .eq. 999) ) then

c  **     wind directions both missing

          clxoq(i1) = -1.0
          saxoq(i1) = -1.0
          miss_xoq = miss_xoq + 1

c  **     if qa output selected write out status 

          if( test_flg ) then
            write(qa_unit,'(2x,i5,2x,i2,2x,2f6.2,2x,2i6, 2x,
     &                 2(1pe10.2))')  i1, stab(i1), u10, rhu, dirl(i1),
     &                 diru(i1), clxoq(i1), saxoq(i1)
          endif

          cycle

c  **   establish wind direction 

        else if( (dirl(i1) .eq. 999) .or. (diru(i1).eq.999) ) then
          dir = min(dirl(i1),diru(i1))
        else
          if( rht .lt. testht ) then
            dir = dirl(i1)
          else
            dir = diru(i1)
          endif
        endif

c  **   calculate downwash (negative if w0 < 1.5 rhu)

        if( rhu .ge. umin ) then
          dwash = amin1( 4.0 * srad * (w0 / rhu - 1.5), 0.0)
        else 
          dwash = 0.0
        endif

c  **   calculate effective release height 

        if( rtype .eq. 1 ) then
          eff_ht = rht - recht + t_diff 
        else
          eff_ht = amax1(rht + dwash, 0.0) - recht + t_diff
        endif

c  **   do x/q calculation        

        if( ((rtype .le. 2) .and. (u10 .lt. umin) ) .or.
     &      ((rtype .ge. 2) .and. (rhu .lt. umin) ) ) then

c  **   calm ... call windyxoq with umin

          calm_xoq = calm_xoq + 1

          call windyxoq( rtype, barea, dist, eff_ht, stab(i1), umin,
     &                   nsigy(stab(i1)), nsigz(stab(i1)), sigz0, sigy0,
     &                   f0, w0, sw_cnst, cxoq, sxoq )
          clxoq(i1) = cxoq
          saxoq(i1) = sxoq

c  **   determine if wind blowing from release point to receptor

        else if( dir1 .le. dir2 ) then

c  **     direction window doesn't straddle north 

          if( (dir1 .le. dir) .and. (dir .le. dir2) ) then

c  **       wind direction is in window  -- calculate x/q

            in_sect = in_sect + 1

            call windyxoq( rtype, barea, dist, eff_ht, stab(i1), rhu, 
     +                     nsigy(stab(i1)), nsigz(stab(i1)), sigz0,  
     &                     sigy0, f0, w0, sw_cnst, cxoq, sxoq)

            clxoq(i1) = cxoq 
            saxoq(i1) = sxoq

          else 

c  **       wind direction is outside window

            out_sect = out_sect + 1

            clxoq(i1) = 0.0
            saxoq(i1) = 0.0

          endif

        else

c  **     wind direction window straddles north

          if( (dir .le. dir2) .or. (dir .ge. dir1) ) then

c  **       wind direction is in window -- calculate x/q

            in_sect = in_sect + 1 

            call windyxoq( rtype, barea, dist, eff_ht, stab(i1), rhu,
     +                     nsigy(stab(i1)), nsigz(stab(i1)), sigz0, 
     +                     sigy0, f0, w0, sw_cnst, cxoq, sxoq)

            clxoq(i1) = cxoq 
            saxoq(i1) = sxoq

          else

c  **       wind direction is outside of the window

            out_sect = out_sect + 1

            clxoq(i1) = 0.0
            saxoq(i1) = 0.0
          endif

        endif                ! end of x/q calculations

c  **   if qa output selected write out status 

        if( test_flg ) then
          write(qa_unit,'(2x,i5,2x,i2,2x,2f6.2,2x,i6,8x,
     &               2(1pe10.2))')  i1, stab(i1), u10, rhu, dir,
     &               clxoq(i1), saxoq(i1)
        endif

      enddo                  ! met data do loop

c  **  update maximum and minimum values

      do  i1=first_met, last_met
        if( clxoq(i1) .gt. 0.0 ) then
          clmax = amax1( clmax, clxoq(i1) )
          clmin = amin1( clmin, clxoq(i1) ) 
          samax = amax1( samax, saxoq(i1) )
          samin = amin1( samin, saxoq(i1) )
        endif
      enddo

      first_xoq = first_met
      last_xoq = last_met
      tot_xoq = tot_xoq + (last_xoq - first_xoq) + 1

      return
      end
