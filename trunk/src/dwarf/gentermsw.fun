C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_TERMSW
C.Keywords:	Device, Inquire
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	900227 FMO - creation
C.Version:	920528 GvD - use GEN_ISATERM iso. ISATTY to isolate system-dep.
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_TERMSW (DEVICE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DEVICE		! (i) device name
C
C.Purpose:	Determine whether the device is the terminal
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	is terminal
C	warning		0	is not a terminal
C.Notes:
C	- If SYS$INPUT or SYS$COMMAND, check whether logical unit 5 (the
C	  standard input device) is a terminal.
C	- If SYS$OUTPUT, check whether logical unit 6 (the standard output
C	  device) is a terminal.
C	- In all other cases, return 0.
C-------------------------------------------------------------------------
C
	INTEGER		STR_UPCASE
	LOGICAL		GEN_ISATERM
C
	CHARACTER*11	UPCDEV
	INTEGER		IS
C
C
	UPCDEV = DEVICE
	IS = STR_UPCASE (UPCDEV)
	IF (UPCDEV.EQ.'SYS$INPUT' .OR. UPCDEV.EQ.'SYS$COMMAND') THEN
		IF (GEN_ISATERM(5)) THEN
			GEN_TERMSW = 1
		ELSE
			GEN_TERMSW = 0
		END IF
	ELSE IF (DEVICE.EQ.'SYS$OUTPUT') THEN
		IF (GEN_ISATERM(6)) THEN
			GEN_TERMSW = 1
		ELSE
			GEN_TERMSW = 0
		END IF
	ELSE
		GEN_TERMSW = 0
	END IF
	RETURN
	END
