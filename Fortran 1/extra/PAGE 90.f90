READ(5,11),A,B,C
11 FORMAT(3F10.2)
WRITE(6,12),A,B,C
12 FORMAT('THE COEFFCIENT ARE',3(2X,F10.2))
D=B**2-4*A*C
IF(D)10,20,30
10 WRITE(6,13)
13 FORMAT(1X,'THERE ARE NO REAL ROOTS')
GO TO 100
20 ROOT=-B/(2.0*A)
WRITE(6,23),ROOT,ROOT
23 FORMAT(1X,'THERE ARE TWO IDENTICAL ROOTS',2(3X,F10.2))
GO TO 100
30 ROOT1=(-B+SQRT(D))/(2.0*A)
ROOT2=(-B-SQRT(D))/(2.0*A)
WRITE(6,33),ROOT1,ROOT2
33 FORMAT(1X,'THERE ARE TWO ROOTS',2(3X,F10.2))
100 STOP
END
