DIMENSION N(25)
DO 100 I=1,25
    READ*,N(I)
100 CONTINUE
DO 300 J=1,24
    JJ=J+1
    DO 200 K=JJ,25
        IF(N(J)+N(K).NE.75) GO TO 200
        PRINT*,N(J),N(K)
200 CONTINUE
300 CONTINUE
STOP
END
