      block data

      include 'param.inc'
      include 'met.inc'
      include 'scenario.inc'
      include 'units.inc'
      include 'xoq.inc'

c     set i/o units

      data scenunit / 1 /, met_unit / 2 /, log_unit / 3 /
      data qa_unit / 10 /, cfd_unit / 11 /

c     initialize the met data counters

      data end_met  / .false. /, next_met / 1 /, tot_met / 0 /

     +     first_met / 1 /, last_met / maxhours  /

c     initialize the xoq counters

      data first_xoq / 0 /, last_xoq / 0 /, tot_xoq / 0 /,
     +     calm_xoq / 0 /, in_sect / 0 /, out_sect / 0 /, elevated / 0 /

c     wind speed conversion factors

      data cf / 1.0, .447, .5144 /

c     initialize the upper and lower x/q bounds

      data clmax / 0.0 /, clmin / 1.0 /, samax / 0.0 /, samin / 1.0 /

c     set flag for calculation of bin limits

      data limits / .true. /

      end
