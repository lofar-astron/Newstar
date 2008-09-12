C+ WNDSTI.FOR
C  WNB 910909
C
C  Revisions:
C	JPH 930513	comments
C	WNB 931015	Use SSH
C
	SUBROUTINE WNDSTI(FCA,SNAM)
C
C  Find an index for a set
C  Note: "Set" rwefers to the generic "set" concept in Newstar, which may be a 
C SCN-file sector, a .WMP-file map or whatever. The only assumption made is
C that the offsets are as defined in the PARAMETER statements below.
C
C  Result:
C
C	CALL WNDSTI( FCA_J:I, SNAM_J(0:*):O)
C				If SNAM is not an index (hence a # specifier)
C				replace SNAM with an index.

C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SSH_O_DEF'		!SET RELATED
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!FILE TO SEARCH
	INTEGER SNAM(0:SOF__N-1)	!FULL SET NAME
C
C  Function references:
C
	LOGICAL WNDSTH			!GET AN INDEX
	LOGICAL WNFRD			!READ DISK
C
C  Data declarations:
C
	INTEGER SETS(0:SOF__N-1,0:1)	!SEARCH PATTERN
	INTEGER SNAM1(0:SOF__N-1)	!NAME FOUND
	BYTE SSH(0:SSH__L-1)		!PART SET HEADER
	  INTEGER SSHJ(0:SSH__L/LB_J-1)
	  EQUIVALENCE (SSH,SSHJ)
C-
C
C INIT
C
	IF (SNAM(SOF_SPEC).NE.SOF_M_SPEC) GOTO 900	!NOT A # SPECIFIER
	DO I=0,SOF__N-1
	  SETS(I,0)=0					!SEARCH PATTERN
	  SETS(I,1)=SOF_M_ALL				!SET ALL
	END DO
	SETS(SOF_0_NLINE,0)=1
C
C FIND
C
	DO WHILE (WNDSTH(FCA,SETS,0,J,SNAM1))		!SEARCH
	  IF (.NOT.WNFRD(FCA,SSH__L,SSH,J)) GOTO 900	!STOP
	  IF (SNAM(0).EQ.SSHJ(SSH_SETN_J)) THEN		!FOUND
	    DO I=0,SOF__N-1				!SET INDEX
	      SNAM(I)=SNAM1(I)
	    END DO
	    GOTO 900					!READY
	  END IF
	END DO
C
 900	CONTINUE
	RETURN
C
C
	END
