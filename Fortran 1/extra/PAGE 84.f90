REAL INT
! THE 'INT' IS INTERGER TYPE BUT WE FORCEFULLY CREATE IT REAL
READ(5,10)AMT
10 FORMAT(F15.2)
IF(AMT.LE.10000)GO TO 100
RATE = 0.06
GO TO 200
100 RATE=0.07
200 INT=AMT*RATE
WRITE(6,20),INT
20 FORMAT(1X,'THE INTERSEBT IS ',2X,F10.2)
STOP
END
