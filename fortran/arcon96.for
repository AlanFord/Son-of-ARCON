c**********************************************************************
c
c     son-of-arcon
c     derived from ARCON96 as described in NUREG/CR-6331 Rev. 1
c
c     ARCON96: originally by j. v. ramsdell, jr., c. a. simonen
c
c     description: program to estimate x/q for control room habitability
c                  assessments. 
c
c     relationship to other modules:
c
c        calls:    scenread  ---  read user input
c                  record    ---  record model input in log file
c                  init      ---  compute diffusion coefficients
c                                 and other constants that don't change
c                  metread   ---  read meteorological data 
c                  xoqcalc5  ---  calculate relative concentrations
c                  proc      ---  compute averages and accumulate
c                                 counts in bins  
c                  shift     ---  move old meteorological data and x/qs
c                                 to the beginning of the data arrays 
c                                 before reading new data
c                  summary   ---  convert counts in bins to frequency 
c                                 distributions and determine 95th %
c                                 x/q values 
c
c***********************************************************************

      implicit     none

      include      'param.inc'
      include      'met.inc'
      include      'scenario.inc'
      include      'units.inc'
      include      'xoq.inc' 

      logical*1    uo

      character*8  rtime
      character*10 rdate     

      integer*2    shr, smin, ssec, s100, ehr, emin, esec, e100
      integer*2    yy, mm, dd, hh, min, ss, hs
      integer*8    values(8)

      real          detime, proctime, sumtime, xoqtime

c     get date and time of code execution

      rdate = '  /  /    '
      rtime = '  :  :  '
      call date_and_time(values=values)
      mm = values(2)
      dd = values(3)
      yy = values(1)
      hh = values(5)
      min = values(6)
      ss = values(7)

      detime = 0.0
      proctime = 0.0
      xoqtime = 0.0
      sumtime = 0.0

c     open scenario input file

      open( scenunit, file=' ', status='old', err=997 )

c     read scenario data

      call scenread

      if( test_flg ) then
        open(unit=qa_unit,file=qa_file,status='unknown')
        write(*,'(2x,a,a)' ) 'qa_file = ', qa_file
      endif
   
c     open log file and output program name and run date

      open(unit=log_unit,file=log_file,status='new', err=998)

      write(log_unit,1)
    1 format(/
     +  ' program title:  son-of-arcon'//
     +  ' derived from:  appendix a of nureg/cr-6331 rev. 1 '/ )

      write(log_unit,'(1x,a,2x,a,2x,a,2x,a)') 'program run',
     +                rdate,'at',rtime

c     record user input on log file

      call record

c     compute variables that are functions of the scenario and 
c     stability but not functions of wind speed or direction

      call init

c     read and process data

      do while ( .not. end_met )            
 
c       check on status of met data file, open new file if necessary

        inquire( unit=met_unit, opened=uo )  

        if( .not. uo ) then

c       open meteorological data file if another file exists

          if( next_met .le. nmetfile ) then
            open( met_unit, file=met_file(next_met), status='old', 
     +            err=999 )  
            next_met = next_met + 1
          else               ! no more meteorological data files
            end_met = .true.
          endif
        endif

        call date_and_time(values=values)
        shr = values(5)
        smin = values(6)
        ssec = values(7)
        s100 = values(8)

c     read meteorological data files (<= 10,000 hours)

        call metread

        call date_and_time(values=values)
        ehr = values(5)
        emin = values(6)
        esec = values(7)
        e100 = values(8)
        detime = detime + 3600.*(ehr-shr) + 60.*(emin-smin)
     +                  + (esec-ssec) + (e100-s100)/100.

        if( test_flg ) then
          write(qa_unit,'(/2x,a)') 'son-of-arcon'
          write(qa_unit,'(/5x,a,i6)') 'first met = ', first_met
          write(qa_unit,'(5x,a,i6)') 'last met  = ', last_met
        endif

c     compute hourly x/q values

        call date_and_time(values=values)
        shr = values(5)
        smin = values(6)
        ssec = values(7)
        s100 = values(8)

        write(*,'(//a)') ' calculating x/q '

        call xoqcalc5  

        call date_and_time(values=values)
        ehr = values(5)
        emin = values(6)
        esec = values(7)
        e100 = values(8)
        xoqtime = xoqtime + 3600.*(ehr-shr) + 60.*(emin-smin)
     .                    + (esec-ssec) + (e100-s100)/100.

c     process first set of x/q's -- set cfd limits, accumulate values

        call date_and_time(values=values)
        shr = values(5)
        smin = values(6)
        ssec = values(7)
        s100 = values(8)

        write(*,'(/a)') ' processing x/q data'

        call proc

        call date_and_time(values=values)
        ehr = values(5)
        emin = values(6)
        esec = values(7)
        e100 = values(8)
        proctime = proctime + 3600.*(ehr-shr) + 60.*(emin-smin)
     .                      + (esec-ssec) + (e100-s100)/100.

        if( .not. end_met ) then

c       move residual met. data and x/q's to front of data arrays

          call shift 

        endif

      enddo                  !  end of data input and processing

c     summarize and output results of calculations     

      call date_and_time(values=values)
      shr = values(5)
      smin = values(6)
      ssec = values(7)
      s100 = values(8)

      write(*,'(/a)') ' summarizing calculations  '

      call summary

      call date_and_time(values=values)
      ehr = values(5)
      emin = values(6)
      esec = values(7)
      e100 = values(8)
      sumtime = 3600.*(ehr-shr) + 60.*(emin-smin) 
     .                      + (esec-ssec) + (e100-s100)/100.

c     normal end of program

      if( test_flg ) then
        write(qa_unit,'(/2x,a)') 'son-of-arcon'
        write( qa_unit,100)  detime, xoqtime, proctime, sumtime
  100   format( /5x, 'data entry time (sec)              = ', f10.2,
     .          /5x, 'x/q computation time (sec)         = ', f10.2,
     .          /5x, 'data processing time (sec)         = ', f10.2,
     .          /5x, 'summary time (sec)                 = ', f10.2 )
        close( qa_unit )
      endif

      write(log_unit,'(2x,/a)') ' normal program completion '
     
      go to 1000

c     error end of program

  997 stop   ' unable to open scenario input data file '
  998 stop   ' unable to open log file ... check for existing name'
  999 write( log_unit,'(2x,/a)' ) 
     +     ' unable to open meteorological data file '

 1000 continue          
      
      end


