C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_LENG
C.Keywords:	PPD File, Parameter Description, Length
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variable used:
C	INTEGER*4	PPDPD$LENG	! (m) significant length of description
C
C.Version:	900415 FMO - creation
C.Version:	930510 HjV - Change some INTEGER*2 into
C				and change PPDPD_HLEN (+3 i.s.o. +1 !!!)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_LENG_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Initialize the description length
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C	- The parameter description consists of a number of fixed fields
C	  (PPDPD_HLEN+3 bytes) and possibly a number of variable-length fields
C	  described by the fixed fields PPDPD$xxLEN and PPDPD$xxOFF.
C	  PPDPD$LENG gives the current total length in bytes.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	PPDPD$LENG = PPDPD_HLEN+3
C
	PPD_LENG_INIT = PPD_SUCCESS
	RETURN
	END
