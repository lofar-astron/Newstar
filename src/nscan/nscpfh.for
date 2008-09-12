C+ NSCPFH.FOR
C  WNB 900810
C
C  Revisions:
C	HjV 930311	Change some text
C	CMV 930921	Do not display GFH version (confuses with set header version)
C
	SUBROUTINE NSCPFH(PTYPE,INFCA)
C
C  Show SCN file header
C
C  Result:
C
C	CALL NSCPFH ( PTYPE_J:I, INFCA_J:I)
C					Show on output PTYPE the file header
C					of file INFCA.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (f_p, f_t ETC)
	INTEGER INFCA			!FILE DESCRIPTOR
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
C
C  Data declarations:
C
	BYTE GFH(0:GFHHDL-1)		!FILE HEADER
C-
C
C GET HEADER
C
	IF (.NOT.WNFRD(INFCA,GFHHDL,GFH,0)) THEN
	  CALL WNCTXT(PTYPE,'Read error on input node')
	  RETURN
	END IF
C
C SHOW HEADER
C
	CALL WNCTXT(PTYPE,'!/File description of node !AD\:!/',
	1		GFH(GFH_NAME_1),GFH_NAME_N)
	CALL WNCTXT(PTYPE,'Created: !AD !AD  Revision(!UJ): !AD !AD',
	1		GFH(GFH_CDAT_1),GFH_CDAT_N,
	1		GFH(GFH_CTIM_1),GFH_CTIM_N,
	1		GFH(GFH_RCNT_1),
	1		GFH(GFH_RDAT_1),GFH_RDAT_N,
	1		GFH(GFH_RTIM_1),GFH_RTIM_N)
	CALL WNCTXT(PTYPE,'File contains !UJ datasectors in !UJ groups!/',
	1		GFH(GFH_NLINK_1),GFH(GFH_NLINKG_1))
C
	RETURN
C
C
	END
