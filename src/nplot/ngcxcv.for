C+ NGCXCV.FOR
C  WNB 920826
C
C  Revisions:
C
	SUBROUTINE NGCXCV
C
C  Convert NGF file from external machine to local format
C
C  Result:
C
C	CALL NGCXCV	will convert a NGF file from VAX to local format
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGC_DEF'
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'GFH_T_DEF'
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
	INCLUDE 'SGH_T_DEF'
	INCLUDE 'NGF_O_DEF'		!SET HEADER
	INCLUDE 'NGF_T_DEF'
C
C  Parameters:
C
	INTEGER DBUFL			!LENGTH DATA BUFFER
	  PARAMETER (DBUFL=1024)
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
C
C  Data declarations:
C
	INTEGER CVT			!CONVERSION TYPE
	INTEGER*2 DBH_T(0:1,0:1)	!DATA TRANSLATION
	  DATA DBH_T/4,0,0,1/
	BYTE GFH(0:GFHHDL-1)		!GENERAL FILE HEADER
	BYTE SGH(0:SGHHDL-1)		!SUB-GROUP HEADER
	  INTEGER SGHJ(0:SGHHDL/4-1)
	  EQUIVALENCE (SGH,SGHJ)
	BYTE NGF(0:NGFHDL-1)		!SET HEADER
	  INTEGER*2 NGFI(0:NGFHDL/2-1)
	  INTEGER NGFJ(0:NGFHDL/4-1)
	  EQUIVALENCE (NGF,NGFI,NGFJ)
	REAL DBUF(0:DBUFL-1)
C-
C
C INIT
C
C
C GENERAL FILE HEADER
C
	IF (.NOT.WNFRD(FCAOUT,GFHHDL,GFH,0)) THEN !READ GENERAL FILE HEADER
 10	  CONTINUE
	  CALL WNCTXT(F_TP,'!/I/O error on NGF file')
	  GOTO 900				!READY
	END IF
	IF (GFH(GFH_DATTP_B).EQ.0) GFH(GFH_DATTP_B)=1 !ASSUME VAX INPUT
	IF (GFH(GFH_DATTP_B).EQ.PRGDAT) THEN
	  CALL WNCTXT(F_TP,'!/Data already converted')
	  GOTO 800
	END IF
	CVT=GFH(GFH_DATTP_B)			!INPUT TYPE
	CALL WNTTTL(GFHHDL,GFH,GFH_T,CVT)	!CONVERT
	GFH(GFH_DATTP_B)=PRGDAT			!SET CURRENT DATA TYPE
	IF (.NOT.WNFWR(FCAOUT,GFHHDL,GFH,0)) GOTO 10 !REWRITE HEADER
C
C GROUP HEADERS
C
	J=1					!LEVEL 1
	J1=GFH_LINKG_1				!CURRENT GROUP
	J2=GFH_LINKG_1				!CURRENT LINK HEAD
 22	CONTINUE
	IF (.NOT.WNFRD(FCAOUT,SGHHDL,SGH,J1)) GOTO 10 !READ CURRENT
 20	CONTINUE
	IF (SGHJ(SGH_LINK_J).EQ.J2) THEN	!END OF LIST
	  J=J-1					!DECREASE LEVEL
	  IF (J.EQ.0) GOTO 21			!READY
	  J1=SGHJ(SGH_HEADH_J)-SGH_LINKG_1+SGH_LINK_1 !LOWER HEADER ADDR.
	  IF (.NOT.WNFRD(FCAOUT,SGHHDL,SGH,J1)) GOTO 10 !READ IT
	  J2=SGHJ(SGH_HEADH_J)			!NEW LINK HEAD
	  GOTO 20				!CONTINUE
	END IF
	J1=SGHJ(SGH_LINK_J)			!NEXT ENTRY
	IF (.NOT.WNFRD(FCAOUT,SGHHDL,SGH,J1)) GOTO 10 !READ IT
	CALL WNTTTL(SGHHDL,SGH,SGH_T,CVT)	!CONVERT IT
	IF (.NOT.WNFWR(FCAOUT,SGHHDL,SGH,J1)) GOTO 10 !WRITE IT
	IF (SGHJ(SGH_DATAP_J).EQ.0) THEN	!MORE LEVELS
	  IF (SGHJ(SGH_LINKG_J).EQ.J1+SGH_LINKG_1) GOTO 20 !NO NEXT LEVEL
	  J=J+1					!NEXT LEVEL
	  IF (J.GT.8) GOTO 10			!TOO MANY LEVELS
	  J2=J1+SGH_LINKG_1			!NEW HEADER PTR
	  J1=J2					!NEXT CURRENT
	  GOTO 22				!CONTINUE
	END IF
	GOTO 20					!MORE
 21	CONTINUE
C
C DO SETS
C
	IF (.NOT.WNFRD(FCAOUT,8,NGF,GFH_LINK_1)) GOTO 10 !READ SET HEADER START
30	CONTINUE
	J=NGFJ(NGF_LINK_J)			!NEXT IN LIST
	IF (J.EQ.GFH_LINK_1) GOTO 800		!ALL DONE
	IF (.NOT.WNFRD(FCAOUT,NGFHDL,NGF,J)) GOTO 10 !READ SET HEADER
	CALL WNTTTL(NGFHDL,NGF,NGF_T,CVT)	!CONVERT IT
	IF (.NOT.WNFWR(FCAOUT,NGFHDL,NGF,J)) GOTO 10 !WRITE SET HEADER
C
C PLOTS
C
	J=NGFJ(NGF_DPT_J)			!POINTER TO DATA
	I2=NGFJ(NGF_SCN_J)			!PLOT LENGTH
	DO WHILE (I2.GT.0)			!DO PER LINE
	  I3=MIN(I2,DBUFL)			!DO THIS TIME
	  I1=2*LB_E*I3				!IN BYTES
	  DBH_T(1,0)=I1				!TRANSLATION LENGTH
	  IF (.NOT.WNFRD(FCAOUT,I1,DBUF,J)) GOTO 10 !READ DATA
	  CALL WNTTTL(I1,DBUF,DBH_T,CVT)	!CONVERT
	  IF (.NOT.WNFWR(FCAOUT,I1,DBUF,J)) GOTO 10 !WRITE DATA
	  J=J+I1				!UPDATE POINTER
	  I2=I2-I3				!UPDATE COUNT
	END DO					!NEXT LINE
	GOTO 30					!NEXT SET
C
C READY
C
 800	CONTINUE
C
	RETURN
C
C ERROR
C
 900	CONTINUE
C
	RETURN					!READY
C
C
	END
