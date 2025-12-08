C     Test program: Intrinsic functions
      PROGRAM INTRIN
      INTEGER I, J
      REAL A, B

C     ABS function
      I = -5
      J = ABS(I)
      WRITE(*,*) 'ABS(-5) =', J

      A = -3.14
      B = ABS(A)
      WRITE(*,*) 'ABS(-3.14) =', B

C     MOD function
      I = 17
      J = MOD(I, 5)
      WRITE(*,*) 'MOD(17,5) =', J

C     INT function (truncate real to integer)
      A = 7.89
      I = INT(A)
      WRITE(*,*) 'INT(7.89) =', I

      A = -2.5
      I = INT(A)
      WRITE(*,*) 'INT(-2.5) =', I

C     REAL function (convert integer to real)
      I = 42
      A = REAL(I)
      WRITE(*,*) 'REAL(42) =', A

C     SQRT function
      A = SQRT(4.0)
      WRITE(*,*) 'SQRT(4) =', A

      A = SQRT(16.0)
      WRITE(*,*) 'SQRT(16) =', A

      A = SQRT(2.0)
      WRITE(*,*) 'SQRT(2) =', A

C     SQRT with integer argument (auto-converted)
      I = 9
      A = SQRT(REAL(I))
      WRITE(*,*) 'SQRT(9) =', A

      WRITE(*,*) 'INTRINSICS TESTS PASSED'
      END
