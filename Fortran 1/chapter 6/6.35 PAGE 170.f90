DIMENSION I(25)
DO 100 K=1,25
    READ*,I(K)
100 CONTINUE
DO 200 K=1,25
    JJ=K+1
    DO 300 J=JJ,25
        IF(I(K)+I(J).EQ.15) PRINT*,I(K),I(J)
300 CONTINUE
200 CONTINUE
STOP
END
