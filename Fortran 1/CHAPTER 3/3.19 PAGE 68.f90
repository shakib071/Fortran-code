READ(5,11),A,B,C
11 FORMAT(3F10.2)
SUR=2*(A*B+B*C+C*A)
VOL=A*B*C
WRITE(6,12),SUR,VOL
12 FORMAT(1X,'THR SURFACE AREA OF BOX',F10.2,2X,'VOLUME OF THE BOX IS',F10.2)
STOP
END
