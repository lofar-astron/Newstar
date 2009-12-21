C+ NGCNVS.FOR
C  WNB 920826
C
C  Revisions:
C	HjV 930311	Change some text
C
	SUBROUTINE NGCNVS
C
C  Convert NGF file to newest format
C
C  Result:
C
C	CALL NGCNVS	will convert a NGF file to newest version
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGC_DEF'
	INCLUDE 'NGF_O_DEF'		!SET HEADER
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
	LOGICAL NGCSTH			!GET A SET WITH NO VERSION CHECK
C
C  Data declarations:
C
	INTEGER SET(0:7,0:1)		!ALL SETS
	INTEGER NGFP			!SET HEADER POINTER
	INTEGER SNAM(0:7)		!SET NAME
	BYTE NGF(0:NGFHDL-1)		!SET HEADER
	  INTEGER*2 NGFI(0:NGFHDL/2-1)
	  INTEGER NGFJ(0:NGFHDL/4-1)
	  REAL NGFE(0:NGFHDL/4-1)
	  EQUIVALENCE (NGF,NGFI,NGFJ,NGFE)
C-
C
C INIT
C
	DO I=0,7				!SET SET *.*.*.*.*.*.*
	  DO I1=0,1
	    SET(I,I1)=0
	  END DO
	END DO
	SET(0,0)=1				!1 LINE
	DO I=0,7
	  SET(I,1)=-1				!*
	END DO
C
C DO ALL SETS
C
	DO WHILE (NGCSTH(FCAOUT,SET,NGF,NGFP,SNAM))	!GET SET
C
C MAKE FROM VERSION 1
C
	  IF (NGFI(NGF_VER_I).EQ.1) THEN		!STILL VERSION 1
	    NGFI(NGF_LEN_I)=NGFHDL			!NEW LENGTH
	    NGFI(NGF_VER_I)=NGFHDV			!NEW VERSION
	  END IF					!VERSION 1
C
C FINISH
C
	  IF (.NOT.WNFWR(FCAOUT,NGFHDL,NGF(0),NGFP)) THEN !REWRITE SET HEADER
10	    CONTINUE
	    CALL WNCTXT(F_TP,'!/Error rewriting Plot Set(s)')
	    GOTO 900
	  END IF
	END DO
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
	RETURN						!READY
C
C
	END