C     Test program: Multi-dimensional arrays
      PROGRAM MULTI
      INTEGER I, J, K, MAT(3,4)
      REAL CUBE(2,2,2)

C     Test 2D array declaration and access
      MAT(1,1) = 11
      MAT(2,3) = 23
      MAT(3,4) = 34

      WRITE(*,*) 'MAT(1,1) =', MAT(1,1)
      WRITE(*,*) 'MAT(2,3) =', MAT(2,3)
      WRITE(*,*) 'MAT(3,4) =', MAT(3,4)

C     Test 2D array with expressions
      I = 2
      J = 2
      MAT(I,J) = I * 10 + J
      WRITE(*,*) 'MAT(2,2) =', MAT(I,J)

C     Test 3D real array
      CUBE(1,1,1) = 1.0
      CUBE(2,2,2) = 8.0
      WRITE(*,*) 'CUBE(1,1,1) =', CUBE(1,1,1)
      WRITE(*,*) 'CUBE(2,2,2) =', CUBE(2,2,2)

C     Test loop filling 2D array
      DO 10 I = 1, 3
        DO 10 J = 1, 4
          MAT(I,J) = I * 10 + J
   10 CONTINUE

C     Verify some values
      IF (MAT(1,1) .EQ. 11 .AND. MAT(2,3) .EQ. 23) THEN
        WRITE(*,*) 'LOOP FILL OK'
      ELSE
        WRITE(*,*) 'LOOP FILL FAILED'
        STOP
      ENDIF

      WRITE(*,*) 'MULTIDIM TESTS PASSED'
      END
