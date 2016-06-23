      subroutine record

c***********************************************************************
c
c     subroutine record
c
c     description:  writes a summary of the input data to the arcon log
c                   file.  the input data are read using the scenread
c                   subroutine.  data are passed to record in the named
c                   common blocks.
c
c     other modules required:  none
c
c***********************************************************************

      implicit  none
  
      include   'param.inc'
      include   'met.inc'
      include   'scenario.inc'
      include   'units.inc'
      include   'xoq.inc'
 
      integer    i1
 
      write(log_unit,'(/2x,a)') '******* arcon input **********'

      write(log_unit,'(/5x,a,i2)' ) 
     +       'number of meteorological data files  = ', nmetfile
      write(log_unit,'(5x,a)' ) 'meteorological data file names'
      do i1 = 1,nmetfile
        write(log_unit,'(7x,a)') met_file(i1)
      enddo
      write(log_unit,'(/5x,a,f8.1)')
     +       'height of lower wind instrument (m)  = ', mht1
      write(log_unit,'(5x,a,f8.1)')
     +       'height of upper wind instrument (m)  = ', mht2
      select case( spd_type )
        case(1)
          write(log_unit,'(5x,a)')
     +       'wind speeds entered as meters/second'
        case(2)
          write(log_unit,'(5x,a)')
     +       'wind speeds entered as miles per hour'
        case(3)
          write(log_unit,'(5x,a)')
     +       'wind speeds entered as knots'
        case default
          write(log_unit,'(5x,a)')
     +       'wind speeds entered in unknown units'
      end select

      select case( rtype )
        case(1)
          write(log_unit,'(/5x,a)')
     +       'ground-level release'
        case(2)
          write(log_unit,'(/5x,a)')
     +       'vent release'
        case(3)
          write(log_unit,'(/5x,a)')
     +       'elevated release'
        case default
          write(log_unit,'(/5x,a)')
     +       'unknown release type'
      end select
      write(log_unit,'(5x,a,f8.1)')
     +       'release height (m)                   = ', rht
      write(log_unit,'(5x,a,f8.1)')
     +       'building area (m^2)                  = ', barea
      write(log_unit,'(5x,a,f9.2)')
     +       'effluent vertical velocity (m/s)     = ', w0
      write(log_unit,'(5x,a,f9.2)')
     +       'vent or stack flow  (m^3/s)          = ', f0
      write(log_unit,'(5x,a,f9.2)')
     +       'vent or stack radius (m)             = ', srad

      write(log_unit,'(/5x,a,5x,i3.3)' )
     +       'direction .. intake to source (deg)  = ', wind_dir
      write(log_unit,'(5x,a,5x,i3)' ) 
     +       'wind direction sector width (deg)    = ', window
      write(log_unit,'(5x,a,2x,i3.3,a,i3.3)')
     +       'wind direction window (deg)          = ', dir1, 
     +       ' - ', dir2
      write(log_unit,'(5x,a,f8.1)')
     +       'distance to intake (m)               = ', dist 
      write(log_unit,'(5x,a,f8.1)')
     +       'intake height  (m)                   = ', recht 
      write(log_unit,'(5x,a,f8.1)')
     +       'terrain elevation difference (m)     = ', t_diff 

      write(log_unit,'(/5x,a)') 'output file names ' 
      write(log_unit,'(7x,a)') log_file 
      write(log_unit,'(7x,a)') cfd_file 

      write(log_unit,'(/5x,a,f8.1)')
     +       'minimum wind speed (m/s)             = ', umin
      write(log_unit,'(5x,a,f9.2)')
     +       'surface roughness length (m)         = ', z0 
      write(log_unit,'(5x,a,f8.1)')
     +       'sector averaging constant            = ', sw_cnst 


      write(log_unit,'(/5x,a,f9.2)')
     +       'initial value of sigma y             = ', sigy0      
      write(log_unit,'(5x,a,f9.2)')
     +       'initial value of sigma z             = ', sigz0      

      if( test_flg ) then
        write(log_unit,'(/5x,a)')
     +     'expanded output for code testing selected'
        write(log_unit,'(5x,a,a)' ) 
     &      'qa output file = ', qa_file
      else   
        write(log_unit,'(/5x,a)')
     +     'expanded output for code testing not selected'
      endif

      return

      end
