C     Test program: DO loops
      PROGRAM DOLOOP
      INTEGER I, SUM

C     Simple DO loop
      SUM = 0
      DO 10 I = 1, 5
        SUM = SUM + I
10    CONTINUE
      WRITE(*,*) 'SUM 1-5 =', SUM

C     DO loop with step
      SUM = 0
      DO 20 I = 2, 10, 2
        SUM = SUM + I
20    CONTINUE
      WRITE(*,*) 'SUM EVENS =', SUM

C     Countdown loop
      WRITE(*,*) 'COUNTDOWN:'
      DO 30 I = 5, 1, -1
        WRITE(*,*) I
30    CONTINUE

      WRITE(*,*) 'DOLOOP TESTS PASSED'
      END
