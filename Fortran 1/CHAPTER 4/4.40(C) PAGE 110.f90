I=0
J=1
! J IS TO COMPARE WITH N.ie.HOW MANY NUM READED
READ*,N
100 READ*,NUM
IF(NUM.EQ.100)I=I+1
J=J+1
IF(J.LE.N)GO TO 100
PRINT*,'THE NUMBER OF PERFECT NUMBER IS',I
STOP
END

