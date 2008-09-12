C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_CLRBLX
C.Keywords:	Block, Clear
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	880626 FMO - creation
C.Version:	900906 FMO - at most 2 entry points per function
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLEAR_BLB  (ARRAY_B,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY_B(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear a BYTE array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_B(I) = 0
	ENDDO
C
	CLEAR_BLB = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CLRBLB (ARRAY_B,NELEM)	! archaic form
	          ENTRY    GEN_CLRBLK (ARRAY_B,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_CLRBLK
C
	BYTE		ARRAY_B(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear a BYTE array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_B(I) = 0
	ENDDO
C
	GEN_CLRBLB = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLEAR_BLI  (ARRAY_I,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*2	ARRAY_I(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear an INTEGER*2 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_I(I) = 0
	ENDDO
C
	CLEAR_BLI = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CLRBLI (ARRAY_I,NELEM)	! archaic form
	          ENTRY    GEN_CLRBLW (ARRAY_I,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_CLRBLW
C
	INTEGER*2	ARRAY_I(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear an INTEGER*2 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_I(I) = 0
	ENDDO
C
	GEN_CLRBLI = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLEAR_BLJ  (ARRAY_J,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	ARRAY_J(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear an INTEGER*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_J(I) = 0
	ENDDO
C
	CLEAR_BLJ = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CLRBLJ (ARRAY_J,NELEM)	! archaic form
	          ENTRY    GEN_CLRBLL (ARRAY_J,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_CLRBLL
C
	INTEGER*4	ARRAY_J(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear an INTEGER*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_J(I) = 0
	ENDDO
C
	GEN_CLRBLJ = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLEAR_BLR  (ARRAY_R,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	REAL*4		ARRAY_R(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear a REAL*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_R(I) = 0
	ENDDO
C
	CLEAR_BLR = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CLRBLR (ARRAY_R,NELEM)	! archaic form
	          ENTRY    GEN_CLRBLF (ARRAY_R,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_CLRBLF
C
	REAL*4		ARRAY_R(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear a REAL*4 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_R(I) = 0
	ENDDO
C
	GEN_CLRBLR = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLEAR_BLD  (ARRAY_D,NELEM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	REAL*8		ARRAY_D(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear a REAL*8 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_D(I) = 0
	ENDDO
C
	CLEAR_BLD = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CLRBLD (ARRAY_D,NELEM)	! archaic form
	          ENTRY    GEN_CLRBLQ (ARRAY_D,NELEM)	! archaic form
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GEN_CLRBLQ
C
	REAL*8		ARRAY_D(*)	! (m) array to be cleared
	INTEGER*4	NELEM		! (i) nr of elements to be cleared
C
C.Purpose:	Clear a REAL*8 array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C.Notes:
C---------------------------------------------------------------------------
C
C
	DO I = 1,NELEM
		ARRAY_D(I) = 0
	ENDDO
C
	GEN_CLRBLD = 1
	RETURN
	END
