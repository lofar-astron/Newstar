C+ NMOCVX.FOR
C  WNB 900905
C
C  Revisions:
C
	SUBROUTINE NMOCVX
C
C  Convert MDL file from VAX to local format
C
C  Result:
C
C	CALL NMOCVX	will convert a MDL file from VAX to local format
C
C PIN references:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'GFH_T_DEF'
	INCLUDE 'MDH_O_DEF'		!MODEL HEADER
	INCLUDE 'MDH_T_DEF'
	INCLUDE 'MDL_O_DEF'		!MODEL
	INCLUDE 'MDL_T_DEF'
C
C  Parameters:
C
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
	BYTE GFH(0:GFHHDL-1)		!GENERAL FILE HEADER
	  INTEGER GFHJ(0:GFHHDL/4-1)
	  EQUIVALENCE (GFH,GFHJ)
	BYTE MDH(0:MDHHDL-1)		!MODEL HEADER
	  INTEGER MDHJ(0:MDHHDL/4-1)
	  EQUIVALENCE (MDH,MDHJ)
	BYTE MDL(0:MDLHDL-1)		!MODEL LINE
C-
C
C INIT
C
C
C GENERAL FILE HEADER
C
	IF (.NOT.WNFRD(FCAOUT,GFHHDL,GFH,0)) THEN !READ GENERAL FILE HEADER
 10	  CONTINUE
	  CALL WNCTXT(F_TP,'!/I/O error on MDL file')
	  GOTO 900				!READY
	END IF
	IF (GFH(GFH_DATTP_B).EQ.0) GFH(GFH_DATTP_B)=1 !ASSUME VAX INPUT
	CVT=GFH(GFH_DATTP_B)			!CONVERSION TYPE
	IF (GFH(GFH_DATTP_B).EQ.PRGDAT) THEN
	  CALL WNCTXT(F_TP,'!/Data already converted')
	  GOTO 800
	END IF
	CALL WNTTTL(GFHHDL,GFH,GFH_T,CVT)	!CONVERT
	GFH(GFH_DATTP_B)=PRGDAT			!SET CURRENT DATA TYPE
	IF (.NOT.WNFWR(FCAOUT,GFHHDL,GFH,0)) GOTO 10 !REWRITE HEADER
C
C MODEL HEADER
C
	IF (.NOT.WNFRD(FCAOUT,MDHHDL,MDH,
	1		GFHJ(GFH_LINK_J))) GOTO 10 !READ HEADER
	CALL WNTTTL(MDHHDL,MDH,MDH_T,CVT)	!CONVERT IT
	IF (.NOT.WNFWR(FCAOUT,MDHHDL,MDH,
	1		GFHJ(GFH_LINK_J))) GOTO 10 !WRITE IT
C
C SOURCES
C
	DO I=0,MDHJ(MDH_NSRC_J)-1		!ALL SOURCES
	 IF (.NOT.WNFRD(FCAOUT,MDLHDL,MDL,
	1		MDHJ(MDH_MODP_J)+I*MDLHDL)) GOTO 10 !READ
	 CALL WNTTTL(MDLHDL,MDL,MDL_T,CVT)	!TRANSLATE
	 IF (.NOT.WNFWR(FCAOUT,MDLHDL,MDL,
	1		MDHJ(MDH_MODP_J)+I*MDLHDL)) GOTO 10 !WRITE
	END DO
C
C READY
C
 800	CONTINUE
	CALL WNFCL(FCAOUT)			!CLOSE FILE
C
	RETURN
C
C ERROR
C
 900	CONTINUE
	CALL WNFCL(FCAOUT)			!CLOSE FILE
	RETURN					!READY
C
C
	END
