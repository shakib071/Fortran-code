INTEGER SCORE
DIMENSION SCORE(450)
DO 100 I=1,450
    READ*,SCORE(I)
100 CONTINUE
K=0
L=0
M=0
N=0
LL=0
MM=0
NN=0
DO 200 J=1,450
    IF(SCORE(J).LT.200)K=K+1
    IF(SCORE(J).GE.200.AND.SCORE(J).LT.300)L=L+1
    IF(SCORE(J).GE.300.AND.SCORE(J).LT.400)M=M+1
    IF(SCORE(J).GE.400.AND.SCORE(J).LT.500)N=N+1
    IF(SCORE(J).GE.500.AND.SCORE(J).LT.600)LL=LL+1
        IF(SCORE(J).GE.600.AND.SCORE(J).LT.700)NN=NN+1
    IF(SCORE(J).GE.700.AND.SCORE(J).LE.800)MM=MM+1
200 CONTINUE
PRINT*,'LESS THAN 200 IS ',K,'BETWEEN 200 TO 300 IS',L
PRINT*,'BETWEEN 300 TO 400',M,'BETWEEN 400 TO 500 IS',N
PRINT*,'BETWEEN 500 TO 600',LL,'BETWEEN 600 TO 700 IS',NN
PRINT*,'BETWEEN 700 TO 800',MM
STOP
END
