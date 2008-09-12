C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_READ
C.Keywords:	String, Read
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	880921 FMO - creation
C.Version:      960628 WNB - Solaris problem
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_READ_B (STR,BVAL)	! byte
C	          ENTRY    STR_READ_I (STR,IVAL)	! integer*2
C	          ENTRY    STR_READ_J (STR,JVAL)	! integer*4
C	          ENTRY    STR_READ_R (STR,RVAL)	! real*4
C	          ENTRY    STR_READ_D (STR,DVAL)	! real*8
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	STR_READ_I, STR_READ_J, STR_READ_R, STR_READ_D
C
	CHARACTER*(*)	STR		! (i) string to be decoded
	BYTE		BVAL		! (o) decoded value
	INTEGER*2	IVAL		! (o) decoded value
	INTEGER*4	JVAL		! (o) decoded value
	REAL*4		RVAL		! (o) decoded value
	REAL*8		DVAL		! (o) decoded value
C
C.Purpose:	Decode a character string into an integer or real value
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	warning		0	in case of any error
C.Notes:
C-------------------------------------------------------------------------
C
	INTEGER*4	STR_SIGLEN
C
	CHARACTER*7	FORM
	  DATA FORM /'(F00.0)'/
	CHARACTER*5     FORMI
	  DATA FORMI /'(I00)'/
	INTEGER*4	LSTR
C
C
C	ENTRY STR_READ_B (STR,BVAL)
C
	LSTR = STR_SIGLEN (STR)
	WRITE (FORMI(3:4),'(I2.2)') LSTR
	READ (STR(:LSTR),FORMI,ERR=999) BVAL
	STR_READ_B = 1
	RETURN
C
C
	ENTRY STR_READ_I (STR,IVAL)
C
	LSTR = STR_SIGLEN (STR)
	WRITE (FORMI(3:4),'(I2.2)') LSTR
	READ (STR(:LSTR),FORMI,ERR=999) IVAL
	STR_READ_I = 1
	RETURN
C
C
	ENTRY STR_READ_J (STR,JVAL)
C
	LSTR = STR_SIGLEN (STR)
	WRITE (FORMI(3:4),'(I2.2)') LSTR
	READ (STR(:LSTR),FORMI,ERR=999) JVAL
	STR_READ_J = 1
	RETURN
C
C
	ENTRY STR_READ_R (STR,RVAL)
C
	LSTR = STR_SIGLEN (STR)
	WRITE (FORM(3:4),'(I2.2)') LSTR
	READ (STR(:LSTR),FORM,ERR=999) RVAL
	STR_READ_R = 1
	RETURN
C
C
	ENTRY STR_READ_D (STR,DVAL)
C
	LSTR = STR_SIGLEN (STR)
	WRITE (FORM(3:4),'(I2.2)') LSTR
	READ (STR(:LSTR),FORM,ERR=999) DVAL
	STR_READ_D = 1
	RETURN
C
C
C
 999	STR_READ_B = 0
	RETURN
	END
