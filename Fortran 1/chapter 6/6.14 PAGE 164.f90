DIMENSION Y(21)
X=-10
DO 100 K=1,21
    Y(K)=2.0*X**4-5.0*X**3+6.0*X**2-8.0*X+9.0
    PRINT*,'FOR X=',X,'Y IS',Y(K)
    X=X+1
100 CONTINUE
STOP
END
