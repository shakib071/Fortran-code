READ(5,10),N
10 FORMAT(I5)
K=2
200 IF(MOD(N,K).EQ.0) GO TO 100
K=K+1
IF(K.LE.N/2) GO TO 200
IF(K.GT.N/2) GO TO 300
100 PRINT*,'N IS NOT PRIME'
STOP
300 PRINT*,'N IS PRIME'
STOP
END
