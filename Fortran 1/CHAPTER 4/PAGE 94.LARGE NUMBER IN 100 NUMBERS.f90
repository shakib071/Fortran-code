LAR=0
I=1
100 READ*,N
IF(N.GT.LAR) LAR=N
I=I+1
IF(I.LE.100) GO TO 100
PRINT*,'THE LARGER NUMBER IN 100 NUMBEERS IS ',LAR
STOP
END
