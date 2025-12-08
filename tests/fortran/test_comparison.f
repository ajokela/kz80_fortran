C     Test program: Comparison operators
      PROGRAM COMPARE
      INTEGER A, B

      A = 5
      B = 10

C     Test each comparison operator
      IF (A .EQ. 5) WRITE(*,*) 'A .EQ. 5: TRUE'
      IF (A .NE. B) WRITE(*,*) 'A .NE. B: TRUE'
      IF (A .LT. B) WRITE(*,*) 'A .LT. B: TRUE'
      IF (B .GT. A) WRITE(*,*) 'B .GT. A: TRUE'
      IF (A .LE. 5) WRITE(*,*) 'A .LE. 5: TRUE'
      IF (B .GE. 10) WRITE(*,*) 'B .GE. 10: TRUE'

C     Test logical operators
      IF (A .GT. 0 .AND. B .GT. 0) THEN
        WRITE(*,*) 'BOTH POSITIVE'
      ENDIF

      IF (A .LT. 0 .OR. B .GT. 0) THEN
        WRITE(*,*) 'OR CONDITION MET'
      ENDIF

      IF (.NOT. A .EQ. 0) THEN
        WRITE(*,*) 'A IS NOT ZERO'
      ENDIF

      WRITE(*,*) 'COMPARE TESTS PASSED'
      END
