      subroutine scenread

c***********************************************************************
c
c     scenread
c
c     description:  read data (except meteorological) from a user
c                   created file for arcon96.  run specification file
c                   name should be included as a command line argument
c                   for arcon96.  if the qa (expanded) output option is
c                   selected, the user will be prompted for a qa output
c                   file name.  
c
c     other modules required:  none
c
c***********************************************************************

      implicit none

      include      'param.inc'
      include      'met.inc'
      include      'scenario.inc'
      include      'units.inc'
      include      'xoq.inc'

      integer*2    i1

      character*1  yn

c ********  meteorological information  ***********************

c --- read: number of meteorological data files
c      and meteorological data file names

      read(scenunit,'(i5)') nmetfile
      do i1 = 1,nmetfile
        read(scenunit,'(a)') met_file(i1)
      enddo

c --- read height of lower level meteorological data (m)

      read(scenunit,'(f10.0)') mht1

c --- read height of upper level met data (m)

      read(scenunit,'(f10.0)') mht2

c --- read wind speed data type -- 1 = m/s; 2 = mph; 3 = kt  

      read(scenunit,'(i5)') spd_type

c ***  release information  *******************************

c --- release type -- 1 = ground level; 2 = roof top; 3 = elevated  

      read(scenunit,'(i5)') rtype

c --- release height (m)

      read(scenunit,'(f10.0)') rht

c --- building area for ground release (m**2)

      read(scenunit,'(f10.0)' ) barea

c --- vertical velocity of vent exhaust, m/s (elevated release)

      read(scenunit,'(f10.0)') w0

c --- read stack or vent flow (m**3/s), it may be zero 

      read(scenunit,'(f10.0)') f0

c --- read stack radius (m), it may be zero except for elevated releases
c           (rtype = 3)     

      read( scenunit, '(f10.0)')  srad

c *** receptor information

c --- read wind direction and window 

      read(scenunit,'(2i5)') wind_dir, window

c ***  calculate the upper and lower limits of the wind direction 
c      window...  note integer arithmetic... if window is even the
c      window width is actually window + 1 degrees wide; if window is
c      odd the window width is window - 1 degrees wide.

      dir1 = wind_dir - window / 2
      dir2 = wind_dir + window / 2

c *** put dir1 and dir2 into 0 to 360 degrees range     

      if( dir1 .lt. 0 ) dir1 = dir1 + 360
      if( dir2 .gt. 360 ) dir2 = dir2 - 360

c --- read distance from release point to intake (m) 

      read(scenunit,'(f10.0)') dist

c --- read receptor height 

      read(scenunit,'(f10.0)') recht

c --- read difference in plant grade between stack and intake
c          grade at release point - grade at receptor (m)

      read( scenunit, '(f10.0)') t_diff      

c *** input file names for output  *********************************

c --- read file name for results summary (log file) 

      read(scenunit,'(a)') log_file

c --- read file name for results cdf file 

      read(scenunit,'(a)') cfd_file

c **** get miscellaneous information -- typically default values *****

c --- read surface roughness length (m) 

      read(scenunit,'(f10.0)') z0

c --- read the minimum wind speed

      read(scenunit,'(f10.0)') umin

c --- read sector width constant

      read(scenunit, '(f10.0)') sw_cnst

c --- read x/q averaging intervals (hours)

      read(scenunit,'(10i4)') (na(i1), i1=1,10)

c --- read minimum number of hours for each averaging interval

      read(scenunit,'(10i4)') (navmin(i1), i1=1,10)

c --- read sigma y 0 and sigma z 0 for area source... this read differentiates
c     arcon96 input file from arcon95 input file

      read( scenunit,'(2f10.0)' ) sigy0, sigz0

c  ********  expanded output for test purposes  ******************

c --- read test output: yes or no ----

      read(scenunit,'(a)') yn
      test_flg = .false.
      if( (yn .eq. 'y') .or. (yn .eq. 'y') ) test_flg = .true.

      if( test_flg ) then
        write(*,'(2x,a\)' ) 'enter a name for the qa output file: '
        read(*,'(a)') qa_file
      endif

      return
      end
