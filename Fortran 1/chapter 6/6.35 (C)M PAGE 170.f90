DIMENSION I(25)
DO 100 K=1,25
    READ*,I(K)
100 CONTINUE
ILAR=0
DO 200 J=1,25
    IF(I(J).GT.ILAR) GO TO 20
    20 IF(MOD(I(J),2).EQ.0) ILAR=I(J)
200 CONTINUE
IF(ILAR.EQ.0 ) PRINT*,'THERE IS NO EVEN INTEGER'
IF(ILAR.NE.0) PRINT*,'THE LARGEST EVEN INTEGER IS',ILAR
STOP
END

