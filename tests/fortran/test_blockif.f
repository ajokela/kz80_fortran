C     Test program: Block IF statements
      PROGRAM BLOCKIF
      INTEGER X, Y

C     Simple IF-THEN-ENDIF
      X = 5
      IF (X .GT. 0) THEN
        WRITE(*,*) 'X IS POSITIVE'
      ENDIF

C     IF-THEN-ELSE-ENDIF
      X = -3
      IF (X .GE. 0) THEN
        WRITE(*,*) 'X >= 0'
      ELSE
        WRITE(*,*) 'X < 0'
      ENDIF

C     Nested IF
      X = 10
      Y = 5
      IF (X .GT. 0) THEN
        WRITE(*,*) 'X > 0'
        IF (Y .GT. 0) THEN
          WRITE(*,*) 'Y > 0 TOO'
        ELSE
          WRITE(*,*) 'Y <= 0'
        ENDIF
      ELSE
        WRITE(*,*) 'X <= 0'
      ENDIF

C     Complex condition
      X = 7
      IF (X .GT. 5 .AND. X .LT. 10) THEN
        WRITE(*,*) 'X BETWEEN 5 AND 10'
      ENDIF

      WRITE(*,*) 'BLOCKIF TESTS PASSED'
      END
