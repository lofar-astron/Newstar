C+ Created from fcq.dsc on 000922 at 11:09:06 at duw01
C  FCQ_BD.FOR
C  WNB 000922
C
C  Revisions:
C
C       WNB 890724      Original version
C                                                                             
	BLOCK DATA FCQ_BD
C
C  Result:
C
C       Initialisation of fcq.def
C
C  FCQ.DSC defines the FCA (File Control Area) queue and the
C  FCA exit handler block.
C                                                                             
C
C  Parameters:
C                                                                             
C
C  FCQ common data:
C                                                                             
	INTEGER FCAQUE                          ! FCA QUEUE HEAD
	  DATA FCAQUE /0/
	INTEGER FCAEXH(1:6)                     ! EXIT HANDLER BLOCK
	  DATA FCAEXH /6*0/
C
C  FCQ common block:
C                                                                             
	COMMON /FCQ_COM/ FCAQUE,FCAEXH
C
C
	END
C-                                                                            
