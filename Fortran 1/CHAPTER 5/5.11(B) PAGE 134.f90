X=-4.0
DO 100 I=1,17
    Y=-4.0
    DO 200 J=1,17
    Z=X**3-3*X*Y*Y+2*X*Y+Y-2*Y**3
    PRINT*,'VALUE OF X:',X,'    Y IS:',Y,'   AND Z IS:',Z
    Y=Y+0.5
200 CONTINUE
    X=X+0.5
100 CONTINUE
STOP
END

