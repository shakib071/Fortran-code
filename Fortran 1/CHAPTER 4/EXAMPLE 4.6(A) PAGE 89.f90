REAL NET
INTEGER TYPE
PRINT*,'ENTER AND PAY'
READ*,ID,PAY
PRINT*,'ENTER 1 FOR SINGLE,2 FOR MARRIED,3 FOR MARRIED WITH CHILDREN'
READ*,TYPE
IF(TYPE-2)10,20,30
10 NET=PAY-9.75
GO TO 100
20 NET=PAY-16.25
GO TO 100
30 NET=PAY-24.50
GO TO 100
100 PRINT*,'ID',ID,'NET',NET
STOP
END
