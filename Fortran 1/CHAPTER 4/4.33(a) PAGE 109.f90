PRINT*,'HOW MANY YEARS DO YOU WANT TO DEPOSIT'
READ*,N
AMOUNT=1000.0
RATE=0.07
I=1
100 AMOUNT=AMOUNT+(AMOUNT*RATE)
PRINT*,'YEAR:',I,'AMOUNT',AMOUNT
I=I+1
IF(I.LE.N) GO TO 100
STOP
END
