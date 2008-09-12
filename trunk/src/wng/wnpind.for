C+ WNPIND.FOR
C  WNB 910624
C
C  Revisions:
C
	SUBROUTINE WNPIND(WQDJ,ROUT)
C
C  Initialise a device
C
C  Result:
C
C	CALL WNPIND( WQDJ_J(0:*):I, ROUT:I)
C				Initialise device in area WQDJ, using the
C				device routine ROUT.
C				Set the device in system device queue
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
	INCLUDE	'WQD_O_DEF'		!DEVICE AREA
C
C  Parameters:
C
C
C  Entry points:
C
C
C  Arguments:
C
	INTEGER WQDJ(0:*)		!DEVICE AREA
	EXTERNAL ROUT			!DEVICE ROUTINE
C
C  Function references:
C
	INTEGER WNGARA			!GET ADDRESS
C
C  Data declarations:
C
C-
C
C FILL LENGTH AND INDEX TABLE POINTERS
C
	WQDJ(WQD_LEN_J)=WQD_SVP_1+LB_J			!FIXED PART
	WQDJ(WQD_OPLI_J)=WQDJ(WQD_LEN_J)		!POINTERS
	WQDJ(WQD_LEN_J)=WQDJ(WQD_LEN_J)+3*LB_E*(WQDJ(WQD_NPLIX_J)+1)
	WQDJ(WQD_OPMI_J)=WQDJ(WQD_LEN_J)
	WQDJ(WQD_LEN_J)=WQDJ(WQD_LEN_J)+3*LB_E*(WQDJ(WQD_NPMIX_J)+1)
	WQDJ(WQD_OTXI_J)=WQDJ(WQD_LEN_J)
	WQDJ(WQD_LEN_J)=WQDJ(WQD_LEN_J)+3*LB_E*(WQDJ(WQD_NTXIX_J)+1)
	WQDJ(WQD_OFAI_J)=WQDJ(WQD_LEN_J)
	WQDJ(WQD_LEN_J)=WQDJ(WQD_LEN_J)+3*LB_E*(WQDJ(WQD_NFAIX_J)+1)
	WQDJ(WQD_OCLI_J)=WQDJ(WQD_LEN_J)
	WQDJ(WQD_LEN_J)=WQDJ(WQD_LEN_J)+3*LB_E*(WQDJ(WQD_NCLIX_J)+1)
	WQDJ(WQD_LEN_J)=WQDJ(WQD_LEN_J)+LB_J		!END WORD
C
C SET DEVICE ROUTINE
C
	WQDJ(WQD_DVRT_J)=WNGARA(ROUT)
C
C ZERO USER DATA
C
	DO I=0,NUSED
	  WQDJ(WQD_USE_J+I)=0
	END DO
C
C LINK IN DEVICE LIST
C
	WQDJ(WQD_QUE_J)=WQG_DVLST			!OLD LINK
	WQG_DVLST=WNGARA(WQDJ)				!NEW LINK
C
	RETURN
C
C
	END
