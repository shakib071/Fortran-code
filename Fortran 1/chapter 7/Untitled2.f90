INTEGER ARR
DIMENSION ARR(10)

READ*,N
DO I=1,N
    READ*,ARR(I)
END DO
NN=N-1
DO J=1,NN
    JJ=N-J
    DO K=1,JJ
        IF(ARR(K)>ARR(K+1))then
            TEMP=ARR(K)
            ARR(K)=ARR(K+1)
            ARR(K+1)=TEMP
        END IF
    END DO
END DO

DO L=1,N
    PRINT*,ARR(L)
END DO
STOP
END

