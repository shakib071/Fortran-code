J=1
DO 100 I=1,100
    IF(MOD(J,7).EQ.0)J=J+2
    PRINT*,J
IF(J.GE.99) GO TO 200
    J=J+2

100 CONTINUE
200 STOP
END
