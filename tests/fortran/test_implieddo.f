C     Test program: Implied DO loops
      PROGRAM IMPDO
      INTEGER I, ARR(5)

C     Initialize array
      DO 10 I = 1, 5
        ARR(I) = I * 10
   10 CONTINUE

C     Test simple implied DO
      WRITE(*,*) 'ARRAY VALUES:'
      WRITE(*,*) (ARR(I), I=1,5)

C     Test implied DO with step
      WRITE(*,*) 'REVERSE:'
      WRITE(*,*) (ARR(I), I=5,1,-1)

C     Test implied DO with subset
      WRITE(*,*) 'SUBSET:'
      WRITE(*,*) (ARR(I), I=2,4)

      WRITE(*,*) 'IMPLIEDDO TESTS PASSED'
      END
