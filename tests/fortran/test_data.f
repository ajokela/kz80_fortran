C     Test program: DATA statements
      PROGRAM TDATA
      INTEGER I, J, K, L, ARR(5)
      REAL X, Y, Z

C     Test simple DATA initialization
      DATA I, J, K / 10, 20, 30 /

      WRITE(*,*) 'I =', I
      WRITE(*,*) 'J =', J
      WRITE(*,*) 'K =', K

      IF (I .NE. 10) STOP
      IF (J .NE. 20) STOP
      IF (K .NE. 30) STOP

C     Test real DATA initialization
      DATA X, Y / 3.14, 2.71 /

      WRITE(*,*) 'X =', X
      WRITE(*,*) 'Y =', Y

C     Test DATA with repeat count
      DATA ARR / 5*100 /

      WRITE(*,*) 'ARR(1) =', ARR(1)
      WRITE(*,*) 'ARR(5) =', ARR(5)

      IF (ARR(1) .NE. 100) STOP
      IF (ARR(5) .NE. 100) STOP

C     Test DATA with array element
      DATA L / 42 /

      IF (L .NE. 42) STOP

      WRITE(*,*) 'DATA TESTS PASSED'
      END
