C Test FORMAT statements
      PROGRAM TESTFMT
      INTEGER I, J
      REAL X

C Test simple FORMAT
 100  FORMAT(I5)
      I = 42
      WRITE(*,100) I

C Test integer width formatting
 200  FORMAT(I3)
      J = 7
      WRITE(*,200) J

C Test with literal output for comparison
      WRITE(*,*) 'FORMAT TESTS PASSED'
      END
