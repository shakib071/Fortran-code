PRINT*,'ENTER MANY MANY FIBONACCI SEQUENCE YOU WANT'
READ*,N
I=1
J=1
L=3
!!BECAUSE 1ST AND 2ND ARE EVALUTED BEFORE
PRINT*,I
PRINT*,J
100 K=I+J
PRINT*,K
I=J
J=K
L=L+1
IF(L.LT.N) GO TO 100
STOP
END
