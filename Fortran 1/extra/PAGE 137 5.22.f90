REAL ISUM
READ*,A,B,N
ISUM=(1/A)
DO 100 I=1,N
    K=(A+I*B)
    ISUM=ISUM+(1.0/K)
100 CONTINUE
PRINT*,ISUM
STOP
END
