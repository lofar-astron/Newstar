C+ NMOGSH.FOR
C  WNB 910623
C
C  Revisions:
C
	SUBROUTINE NMOGSH(ODESJ)
C
C  Get general header data
C
C  Result:
C
C	CALL NMOGSH( ODESJ_J(0:*):O)
C			will copy general header to ODESJ
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'MDH_O_DEF'			!MODEL HEADER
C
C  Entries:
C
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER ODESJ(0:*)			!OUTPUT HEADER
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C COPY HEADER
C
	CALL WNGMV(MDHHDL,GDESJ,ODESJ)
C
	RETURN
C
C
	END
