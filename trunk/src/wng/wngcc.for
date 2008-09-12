C+ WNGCC.FOR
C  JPH 941005
C
C  Revisions:
C	JPH 960621	WNGCCN
C
C
	SUBROUTINE 	 WNGCCD()
C
C		Routine to control control-C handling:
C
C	Entry		Sets XHCC to	Return/value/action
C			[0]	[1]
C
C	WNGCCD		1	0	Inhibit
C	WNGCCC		*	0	True if control-C seen (XHCC[1] was >0)
C	WNGCCE		0	0	Exit if XHCC[1] was >0, else reenable
C	WNGCCN		*	*	Control-C count: XHCC[1]
C	WNGCCS(N_J:I)	*	N	Simulate N control-Cs
C  		
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WXH_DEF'
C
C  Parameters
C
	INTEGER	N				! count
C
C-
C
CC	print*,'D' 
	XHCC(0)=1
	XHCC(1)=0				! inhibit
	RETURN
C
C
	ENTRY WNGCCE
CC	print*, 'E', xhcc
 	XHCC(0)=0				! enable
	IF (XHCC(1).NE.0) CALL WNGEX
	XHCC(1)=0
	RETURN
C
C
	ENTRY WNGCCS(N)
CC	print*, 'S', xhcc, '+', N
	XHCC(1)=XHCC(1)+N
	END
C
C
	LOGICAL FUNCTION WNGCCC()
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WXH_DEF'
C
CC	print *, 'C', xhcc
 	WNGCCC=XHCC(1).NE.0			! seen?
	XHCC(1)=0				! clear
C
	RETURN
	END
C
C
	INTEGER FUNCTION WNGCCN()
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WXH_DEF'
C
C
 	WNGCCN=XHCC(1)				! nr seen
CC	print*, 'N', xhcc
C
	RETURN
	END
