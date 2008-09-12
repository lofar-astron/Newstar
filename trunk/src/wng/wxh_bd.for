C+ Created from wxh.dsc on 970828 at 16:58:05 at daw18
C  WXH_BD.FOR
C  WNB 970828
C
C  Revisions:
C
C       WNB 910828      Original version
C                                                                             
	BLOCK DATA WXH_BD
C
C  Result:
C
C       Initialisation of wxh.def
C
C  WXH.DEF is an INCLUDE file for exit and other handlers.
C  
C                                                                             
C
C  Parameters:
C                                                                             
C
C  WXH common data:
C                                                                             
	INTEGER XHED                            ! HEAD OF EXIT HANDLER LIST
	  DATA XHED /0/
	INTEGER XHCC(0:1)                       ! CONTROL C: 0: INHIBIT
	  DATA XHCC /0,0/                       ! 1: SEEN DURING INHIBIT
	INTEGER XHRS(0:4)                       ! RESERVED
	  DATA XHRS /0,0,0,0,0/
C
C  WXH common block:
C                                                                             
	COMMON /WXH_COM/ XHED,XHCC,XHRS
C
C
	END
C-                                                                            
