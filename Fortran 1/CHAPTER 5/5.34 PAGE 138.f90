PRINT*,'ENTER HOW MANY NYUMBER'
READ*,N
J=0
K=0
PRINT*,'ENTER NUMBERS'
DO 100 I=1,N
    READ*,L
    IF(MOD(L,2).EQ.0)J=J+1
    IF(MOD(L,2).NE.0)K=K+1
 100 CONTINUE
 PRINT*,'NUMBER OF EVEN NUMBERS IS',J
 PRINT*,'NUMBER OF ODD NUMBERS IS',K
 STOP
 END
