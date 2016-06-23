      subroutine nsigma1( x, stab, sigz0, sigy0, sigmaz, sigmay)

c*******************************************************************
c     nrcsig1                                            
c     
c     description: computes diffusion coefficients given the 
c                  distance from the source and atmospheric
c                  stability
c 
c     other modules required:  none
c
c*******************************************************************

      implicit     none

      integer      stab

      real         ay(7), az(7,3), bz(7,3), cz(7,3), x, sigmay, sigmaz,
     *             sigy0, sigz0, xv, xe1, xe2

      data ay/ 0.3658, 0.2751,0.2089,0.1471,0.1046,0.0722,0.0481/
      data az/ 0.192,  0.156, 0.116, 0.079, 0.063, 0.053, 0.032,
     +         0.00066,0.0382,0.113, 0.222, 0.211, 0.086, 0.052,
     +         0.00024,0.055, 0.113, 1.26,  6.73, 18.05, 10.83 /
      data bz/ 0.936, 0.922, 0.905, 0.881, 0.871, 0.814, 0.814,
     +         1.941,  1.149, 0.911, 0.725, 0.678, 0.74,  0.74,
     +         2.094,  1.098, 0.911, 0.516, 0.305, 0.18,  0.18 /
      data cz/ 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     +         9.27,   3.3,   0.0,  -1.7,  -1.3,  -0.35, -0.21, 
     +        -9.6,    2.0,   0.0, -13.,  -34.0, -48.6, -29.2  /

      if( sigy0 .gt. 0.0 ) then

c     calculate virtual distance from sigy0
      
         xv = ( sigy0 / ay( stab ) )**(1.0/0.9031) 
         xe1 = x + xv
      else
         xe1 = x
      endif

      sigmay = ay(stab) *  xe1**0.9031

c  **  sigma z computations 

      if( sigz0 .gt. 0.0 ) then

c     calculate virtual distance from sigz0

         xv = (sigz0 / az(stab,1)) ** (1.0/bz(stab,1))
         xe2 = x + xv       
         
         if( xe2 .gt. 100.0 ) then
            if( (sigz0 - cz(stab,2)) .gt. 0.0 ) then
               xv = ((sigz0 - cz(stab,2)) 
     &               / az(stab,2))**(1.0/bz(stab,2))
            else
               xv = 0.0
            endif   
            xe2 = x + xv
         endif
            
         if( xe2. gt. 1000.0) then
            if( (sigz0-cz(stab,3)) .gt. 0.0 ) then
               xv = ((sigz0 - cz(stab,3)) 
     &               / az(stab,3))**(1.0/bz(stab,3))
            else
               xv = 0.0
            endif   
            xe2 = x + xv
         endif 
         
      else 
      
         xe2 = x
      
      endif
      
      if( xe2 .le. 100.0 ) then
         sigmaz = az(stab,1) * xe2**bz(stab,1)
      else if( xe2 .le. 1000.0 ) then
         sigmaz = az(stab,2) * xe2**bz(stab,2) + cz(stab,2)
      else if( xe2 .gt. 1000.0 ) then
         sigmaz = az(stab,3) * xe2**bz(stab,3) + cz(stab,3)
      endif
     
      return
      end
