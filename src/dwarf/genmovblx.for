C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_MOVBLX
C.Keywords:	Block, Move
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	880626 FMO - creation
C.Version:	890111 FMO - corrected declaration errors (types J,R,D)
C.Version:	900906 FMO - use byte block moves everywhere
C			- use only 2 entry points per function (compiler error)
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION MOVE_BLB   (SRCARR,DSTARR,NELEM)
	          ENTRY    GEN_MOVBLB (SRCARR,DSTARR,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_MOVBLB
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be moved
C
C.Purpose:	Move a BYTE array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	MOVE_BLB = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_MOVBLK (SRCARR,DSTARR,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*2	NELEM		! (i) nr of elements to be moved
C
C.Purpose:	Move a BYTE array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	GEN_MOVBLK = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION MOVE_BLI   (SRCARR,DSTARR,NELEM)
	          ENTRY    MOVE_BLW   (SRCARR,DSTARR,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	MOVE_BLW
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move an INTEGER*2 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*2
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	MOVE_BLI = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_MOVBLI (SRCARR,DSTARR,NELEM)	! archaic form
	          ENTRY    GEN_MOVBLW (SRCARR,DSTARR,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_MOVBLW
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move an INTEGER*2 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*2
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	GEN_MOVBLI = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION MOVE_BLJ   (SRCARR,DSTARR,NELEM)
	          ENTRY    MOVE_BLL   (SRCARR,DSTARR,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	MOVE_BLL
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move an INTEGER*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*4
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	MOVE_BLJ = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_MOVBLJ (SRCARR,DSTARR,NELEM)	! archaic form
	          ENTRY    GEN_MOVBLL (SRCARR,DSTARR,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_MOVBLL
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move an INTEGER*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*4
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	GEN_MOVBLJ = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION MOVE_BLR   (SRCARR,DSTARR,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move a REAL*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*4
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	MOVE_BLR = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_MOVBLR (SRCARR,DSTARR,NELEM)	! archaic form
	          ENTRY    GEN_MOVBLF (SRCARR,DSTARR,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_MOVBLF
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move a REAL*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*4
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	GEN_MOVBLR = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION MOVE_BLD   (SRCARR,DSTARR,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move a REAL*8 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*8
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	MOVE_BLD = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_MOVBLD (SRCARR,DSTARR,NELEM)	! archaic form
	          ENTRY    GEN_MOVBLQ (SRCARR,DSTARR,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_MOVBLQ
C
	LOGICAL*1	SRCARR(*)	! (i) source array
	LOGICAL*1	DSTARR(*)	! (m) destination array
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Move a REAL*8 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM*8
		DSTARR(I) = SRCARR(I)
	ENDDO
C
	GEN_MOVBLD = 1
	RETURN
	END
