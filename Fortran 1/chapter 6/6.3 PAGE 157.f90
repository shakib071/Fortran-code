!(a)
DIMENSION A(10)
SUM=0.0
DO 100 I=1,10
    READ*,A(I)
100 continue
DO 200 J=1,10
    SUM=SUM+(A(J)**2)
200 CONTINUE
PRINT*,'SUM OF THE SQUARES IS ',SUM
!(B)
PROD=1.0
DO 300 K=1,10
    PROD=PROD*(1-A(K))
300 CONTINUE
PRINT*,'THE PROD IS',PROD
STOP
END
