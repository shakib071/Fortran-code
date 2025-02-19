PRINT*,'ENTER HOW MANY ODD NUMBERS'
READ*,N
PRINT*,'         N             N^2        N^3'
DO 100 I=1,N,2
    J=I*I
    K=I**3
    PRINT*,I,J,K
100 CONTINUE
STOP
END
