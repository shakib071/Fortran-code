DO 100 I=100 ,999
    DO 200 J=2,I
        IF((I/J)*J.EQ.I)GO TO 100
    200 CONTINUE
PRINT*,I
100 CONTINUE
STOP
END


