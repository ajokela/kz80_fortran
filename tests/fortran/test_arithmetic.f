C     Test program: Arithmetic operations
      PROGRAM ARITH
      INTEGER I, J, K
      REAL A, B, C

C     Integer arithmetic
      I = 10
      J = 3
      K = I + J
      WRITE(*,*) 'I + J =', K
      K = I - J
      WRITE(*,*) 'I - J =', K
      K = I * J
      WRITE(*,*) 'I * J =', K
      K = I / J
      WRITE(*,*) 'I / J =', K

C     Real arithmetic
      A = 10.0
      B = 3.0
      C = A + B
      WRITE(*,*) 'A + B =', C
      C = A - B
      WRITE(*,*) 'A - B =', C
      C = A * B
      WRITE(*,*) 'A * B =', C
      C = A / B
      WRITE(*,*) 'A / B =', C

C     Mixed operations
      A = 2.5
      I = 4
      C = A * REAL(I)
      WRITE(*,*) 'A * I =', C

      WRITE(*,*) 'ARITH TESTS PASSED'
      END
