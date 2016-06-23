      function invmol2( stab, z0 )

c***********************************************************************
c
c     invmol2
c
c     description:
c
c        invmol computes an estimate of the inverse of the monin-obukhov 
c        length (1/l) based the relationship between turner stability  
c        class, z0, and 1/l shown in figure 5 in golder's paper in 
c        boundary layer meteorology 3:47-58.  the function returns a 
c        mid-range estimate of 1/l given the stability class and z0.
c
c     input:   turner stability                 ==> stab
c              surface roughness length         ==> z0
c
c     other modules required:  none
c
c***********************************************************************

      implicit   none 

      real       invmol2, lobnd, upbnd, z0,
     +           bnd1l(7), bnd1u(7),                         !  z0 .le. 0.01
     +           bnd2la(7), bnd2lb(7), bnd2ua(7), bnd2ub(7), ! 0.01< z0 < 0.1
     +           bnd3la(7), bnd3lb(7), bnd3ua(7), bnd3ub(7), ! 0.1< z0 < 1.0
     +           bnd4l(7), bnd4u(7)                          !  z0 .ge. 1.0
   
      integer    stab

      data bnd1l / -0.160, -0.106, -0.066, -0.024, 0.005, 0.010, 0.056 /
      data bnd1u / -0.106, -0.066, -0.024,  0.005, 0.010, 0.056, 0.120 /
      data bnd2la / -0.160, -0.058, -0.014, 0.0, 0.002, 0.008, 0.008 /
      data bnd2lb / 0.0, 0.024, 0.026, 0.012, -0.0015, -0.001, -0.024 / 
      data bnd2ua / -0.058, -0.014, 0.000, 0.002, 0.008, 0.008, 0.12 /
      data bnd2ub / 0.024, 0.026, 0.012, -0.0015, -0.001, -0.024, 0.0 / 
      data bnd3la / -0.16, -0.046, -0.008, -0.003, 0.002, 0.004, 0.014 /
      data bnd3lb / 0.0, 0.036, 0.032, 0.009, -0.0015, -0.005, -0.018 /
      data bnd3ua / -0.046, -0.008, -0.003, 0.002, 0.004, 0.014, 0.12 /
      data bnd3ub / 0.036, 0.032, 0.009, -0.0015, -0.005, -0.018, 0.0 /
      data bnd4l / -0.16, -0.046, -0.008, -0.003, 0.002, 0.004, 0.014 /
      data bnd4u / -0.046, -0.008, -0.003, 0.002, 0.004, 0.014, 0.12 /

      if( (z0 .gt. 0.01) .and. (z0 .le. 0.1) ) then 
        lobnd = bnd2la( stab ) + bnd2lb( stab ) * alog10( z0 )
        upbnd = bnd2ua( stab ) + bnd2ub( stab ) * alog10( z0 )
      else if( (z0 .gt. 0.1) .and. (z0 .lt. 1.0) ) then  
        lobnd = bnd3la( stab ) + bnd3lb( stab ) * alog10( z0 )
        upbnd = bnd3ua( stab ) + bnd3ub( stab ) * alog10( z0 )
      else if( z0 .le. 0.01 ) then  
        lobnd = bnd1l( stab ) 
        upbnd = bnd1u( stab ) 
      else if( z0 .ge. 1.0 ) then  
        lobnd = bnd4l( stab ) 
        upbnd = bnd4u( stab ) 
      else
        stop 'error in invmol'
      endif

      invmol2 = lobnd + 0.5 * (upbnd - lobnd)

      return
      end
