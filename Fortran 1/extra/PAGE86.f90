 I=1
 100 READ(5,10),ID,S1,S2,S3
 10 FORMAT(I4,3F10.2)
 WRITE(6,10),ID,S1,S2,S3
 SUM=S1+S2+S3
 AVE=SUM/3.0
 WRITE(6,20),AVE
 20 FORMAT(2X,'THE AVERAGE IS',F6.2)
 I=I+1
 IF(I.LE.100) GO TO 100
 STOP
 END
