C$PROG ANAT      - Returns AMU of natural elements
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION ANAT(IZ)
C
      REAL*4 AN(92)
C
      DATA AN/
     &  1.0079,  4.0026,  6.9410,  9.0122, 10.8100, 12.0110,
     & 14.0067, 15.9994, 18.9984, 20.1790, 22.9898, 24.3050,
     & 26.9815, 28.0855, 30.9738, 32.0600, 35.4530, 39.9480,
     & 39.0983, 40.0800, 44.9559, 47.9000, 50.9415, 51.9960,
     & 54.9380, 55.8470, 58.9332, 58.7000, 63.5460, 65.3800,
     & 69.7200, 72.5900, 74.9216, 78.9600, 79.9040, 83.8000,
     & 85.4678, 87.6200, 88.9059, 91.2200, 92.9064, 95.9400,
     & 97.0000,101.0700,102.9055,106.4000,107.8680,112.4100,
     &114.8200,118.6900,121.7500,127.6000,126.9045,131.3000,
     &132.9054,137.3300,138.9055,140.1200,140.9077,144.2400,
     &145.0000,150.4000,151.9600,157.2500,158.9254,162.5000,
     &164.9304,167.2600,168.9342,173.0400,174.9670,178.4900,
     &180.9479,183.8500,186.2070,190.2000,192.2200,195.0900,
     &196.9665,200.5900,204.3700,207.2000,208.9804,209.9829,
     &  0.0000,222.0175,  0.0000,226.0254,227.0278,232.0381,
     &231.0359,238.0290/
C
      IF(IZ.GT.92) GO TO 100
      IF(IZ.LT.1)  GO TO 100
C
      ANAT=AN(IZ)
      RETURN
C
  100 ANAT=0
      RETURN
      END