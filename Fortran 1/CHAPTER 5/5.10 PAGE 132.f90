SUM=0
DO 100 I=1,5
    SUM=SUM+I
100 CONTINUE
COST=4500
L=5
DO 200 J=1,5
    P=FLOAT(L)
    L=L-1
    DEP=(P/SUM)*COST
    PRINT*,'YEAR',J,'DEPRICIATION',DEP
200 CONTINUE
STOP
END
