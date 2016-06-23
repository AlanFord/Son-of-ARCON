      subroutine metread

c***********************************************************************
c
c     metread                                          
c
c     description:  subroutine reads meteorological data for control 
c                   room habitability assessments.  
c
c     other modules required:  none
c 
c***********************************************************************

      implicit none

      include 'param.inc'
      include 'met.inc'
      include 'units.inc'
      include 'xoq.inc'

      integer   stl, nmet
      integer*2 ldir, lspd, udir, uspd
      integer   i1, hr, mf

      character*5 site

c     read first set of meteorological data records

      write(*,'(a)') ' reading meteorological data '

      nmet = 0
      do i1 = first_met,last_met
        read(met_unit,1000,end=10,err=998) site, julday(i1), chour(i1),
     +            ldir, lspd, stl, udir, uspd
 1000   format(1x,a5,3x,i3,i2,2x,i3,i4,1x,i2,2x,i3,i4)
        nmet = nmet + 1

        hr = (julday(i1) - 1) * 24 + chour(i1) + 1

        if( (stl .ge. 1) .and. (stl .le. 7) ) then
          stab(i1) = stl
        else 
          stab(i1) = 99
        endif

        if( (ldir .ge. 1) .and. (ldir .le. 360) ) then
          dirl(i1) = ldir
        else 
          dirl(i1) = 999
        endif

        if( (lspd .ge. 0) .and. (lspd .le. 750) ) then
          spdl(i1) = lspd
        else
          spdl(i1) = 9999
        endif
 
        if( (udir .ge. 1) .and. (udir .le. 360) ) then
          diru(i1) = udir
        else if(dirl(i1) .ne. 999) then
          diru(i1) = dirl(i1)
        else
          diru(i1) = 999
        endif

        if( (uspd .ge. 0) .and. (uspd .le. 750) ) then
          spdu(i1) = uspd
        else
          spdu(i1) = 9999
        endif

        if( mod(i1,100) .eq. 0 ) write(*,'(a,i4)')
     +      '   hour # ', hr 
        cycle

   10   continue             ! close unit and check for additional files

        close(met_unit)
        if( next_met .le. nmetfile) then
          mf = next_met
          next_met = next_met + 1               

          open(met_unit,file=met_file(mf), status='old', err=999)
          read( met_unit,1000) site, julday(i1), chour(i1), ldir, lspd,
     +               stl, udir, uspd
       
          nmet = nmet + 1 
          hr = (julday(i1) - 1) * 24 + chour(i1) + 1

          if( (stl .ge. 1) .and. (stl .le. 7) ) then
            stab(i1) = stl
          else
            stab(i1) = 99
          endif

          if( (ldir .ge. 1) .and. (ldir .le. 360) ) then
            dirl(i1) = ldir
          else
            dirl(i1) = 999
          endif

          if( (lspd .ge. 0) .and. (lspd .le. 750) ) then
            spdl(i1) = lspd
          else
            spdl(i1) = 9999
          endif

          if( (udir .ge. 1) .and. (udir .le. 360) ) then
            diru(i1) = udir
          else if(dirl(i1) .ne. 999) then
            diru(i1) = dirl(i1)
          else
            diru(i1) = 999
          endif

          if( (uspd .ge. 0) .and. (uspd .le. 750) ) then
            spdu(i1) = uspd
          else
            spdu(i1) = 9999
          endif

          if( mod(i1,100) .eq. 0 )
     +        write(*,'(a,i5)') '   hour # ', i1
          cycle
        else
          end_met = .true.
          goto 20            ! no more met data jump out of do loop
        endif
      enddo

   20 continue

      last_met = first_met + nmet - 1
      tot_met = tot_met + nmet

      if( test_flg ) then
        write(qa_unit,'(/2x,a/)' ) 'subroutine metread'
        do i1 = first_met,last_met
          write(qa_unit, '(2x,i5,5x,2i7, i5, 2x,  2i7)') i1, 
     &         dirl(i1), spdl(i1), stab(i1), diru(i1), spdu(i1)
        enddo
      endif

      return

  998 write( log_unit,'(/2x,a,a, /5x,a,i4,a,i4)' ) 
     +  'error reading met. data file = ', met_file(mf),
     +  ' julian day =', julday(i1-1), ';  hour =',  chour(i1-1)
      stop 'error reading met. data'

  999 write( log_unit,'(/2x,a,a)' )  'unable to open met. data file = ',
     +  met_file(mf)
      stop 'error opening met. data file '

      end
