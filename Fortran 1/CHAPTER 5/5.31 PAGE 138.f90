PRINT*,'ENTER HOW MANY NUMBER DO YOU WANT TO ENTER'
READ*,N
PRINT*,'ENTER NUMBERS'
READ*,I
READ*,J
IF(I.GE.J) LAR=I
ISMALL=J
DO 100 K=3,N
    READ*,L
    IF(L.GT.LAR)LAR=L
    IF(L.LT.SMALL)ISMALL=L

100 CONTINUE
PRINT*,'SMALLEST NUMBER IS',ISMALL,'HIGHEST NUMBER IS',LAR
STOP
END
