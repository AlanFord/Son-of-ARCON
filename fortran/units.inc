c***********************************************************************
c
c     units.inc
c
c     file name and unit designations
c
c     included in:  arcon96, blockdata, init, metread, proc, record,
c                   scenread, shift, summary, xoqcalc5, xoq95
c
c***********************************************************************

      logical*1     test_flg
 
      integer*1     log_unit, met_unit, scenunit, cfd_unit, qa_unit

      character*40  log_file, met_file(maxfiles), scenfile, cfd_file,
     &              qa_file

      common /files/ log_file, met_file, scenfile, cfd_file, qa_file

      common /funits/ log_unit, met_unit, scenunit, cfd_unit, qa_unit, 
     &                test_flg
