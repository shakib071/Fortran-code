ISUM=0
200 DO 100 I=1,10
    IF(I.EQ.4)GO TO 100
    ISUM=ISUM+I
    PRINT*,I
100 CONTINUE
PRINT*,ISUM
STOP
END
