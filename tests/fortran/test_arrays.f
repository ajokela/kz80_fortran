C     Test program: Arrays
      PROGRAM ARRAYS
      INTEGER ARR(5), I, SUM
      REAL VALS(3)

C     Integer array
      DO 10 I = 1, 5
        ARR(I) = I * 10
10    CONTINUE

      WRITE(*,*) 'ARRAY VALUES:'
      DO 20 I = 1, 5
        WRITE(*,*) 'ARR(', I, ') =', ARR(I)
20    CONTINUE

C     Sum array elements
      SUM = 0
      DO 30 I = 1, 5
        SUM = SUM + ARR(I)
30    CONTINUE
      WRITE(*,*) 'ARRAY SUM =', SUM

C     Real array
      VALS(1) = 1.5
      VALS(2) = 2.5
      VALS(3) = 3.5
      WRITE(*,*) 'VALS(2) =', VALS(2)

      WRITE(*,*) 'ARRAYS TESTS PASSED'
      END
