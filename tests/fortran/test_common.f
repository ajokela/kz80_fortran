C Test COMMON blocks
      PROGRAM TESTCOM
      COMMON /BLK1/ A, N
      A = 3.14
      N = 42
      WRITE(*,*) 'A =', A
      WRITE(*,*) 'N =', N
      IF (N .NE. 42) STOP
      WRITE(*,*) 'COMMON TESTS PASSED'
      END
