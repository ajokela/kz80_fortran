C     Test program: SUBROUTINE and FUNCTION
      PROGRAM SUBPRO
      INTEGER I, J, K
      REAL A, B, C

C     Test simple SUBROUTINE with no parameters
      CALL HELLO

C     Test SUBROUTINE with parameters
      I = 5
      J = 3
      CALL ADDNUM(I, J, K)
      WRITE(*,*) 'ADDNUM(5,3) =', K

C     Test FUNCTION returning integer
      I = DOUBLE(7)
      WRITE(*,*) 'DOUBLE(7) =', I

C     Test FUNCTION returning real
      A = SQUARE(4.0)
      WRITE(*,*) 'SQUARE(4.0) =', A

C     Test FUNCTION with multiple parameters
      I = MAXVAL(10, 25)
      WRITE(*,*) 'MAXVAL(10,25) =', I

      WRITE(*,*) 'SUBPROGS TESTS PASSED'
      END

C     SUBROUTINE with no parameters
      SUBROUTINE HELLO
      WRITE(*,*) 'HELLO FROM SUBROUTINE'
      RETURN
      END

C     SUBROUTINE with parameters
      SUBROUTINE ADDNUM(I, J, K)
      INTEGER I, J, K
      K = I + J
      RETURN
      END

C     FUNCTION returning integer (I-N implicit INTEGER)
      FUNCTION DOUBLE(N)
      INTEGER N, DOUBLE
      DOUBLE = N * 2
      RETURN
      END

C     FUNCTION returning real (starts with S, implicit REAL)
      FUNCTION SQUARE(X)
      REAL X, SQUARE
      SQUARE = X * X
      RETURN
      END

C     FUNCTION with multiple parameters
      FUNCTION MAXVAL(I, J)
      INTEGER I, J, MAXVAL
      IF (I .GT. J) THEN
        MAXVAL = I
      ELSE
        MAXVAL = J
      ENDIF
      RETURN
      END
