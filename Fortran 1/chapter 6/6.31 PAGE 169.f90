DIMENSION PINVEST(10),RETURNN(20)
N=10
RATE=.07
DO 100 I=1,10
    READ*,PINVEST(I)
100 CONTINUE
RETURNN(1)=PINVEST(1)
DO 200 I=2,10
    20 RETURNN(I)=RETURNN(I-1)+RETURNN(I-1)*RATE+PINVEST(I)
200 CONTINUE
DO 40 I=11,20
    RETURNN(I)=RETURNN(I-1)+RETURNN(I-1)*RATE
    40 CONTINUE
DO 300 I=1,20
    PRINT*,'YEAR',I,'AMOUNT',RETURNN(I)
300 CONTINUE
STOP
END
