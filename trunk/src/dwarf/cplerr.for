C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CPL_ERR
C.Keywords:	Compiler Utility, Error Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	CPL$ERRBUF(*,2)	! (m) error array
C						(i,1) = source-line number
C						(i,2) = error code
C	INTEGER*4	CPL$ERRNTOT	! (m) nr of entries in error buffer
C	INTEGER*4	CPL$ERRNERR	! (m) total nr of error entries
C	INTEGER*4	CPL$ERRNWARN	! (m) total nr of warning entries
C
C.Version:	900407 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_ERR_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
C
C.Purpose:	Initialize the error buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	CPL$ERRNWARN = 0
	CPL$ERRNERR = 0
	CPL$ERRNTOT = 0
C
	CPL_ERR_INIT = CPL_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_ERR_PUT (STAT,LINENR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
	INTEGER*4	STAT		! (i) status code
	INTEGER*4	LINENR		! (i) source-line number
C
C.Purpose:	Put error/warning code and source-line nr in error buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	fatal	CPL_ERRCNTEXC	maximum error count exceeded
C.Notes:
C	- If the status code is true, the function just returns with the
C	  success status.
C	- Otherwise, DWARF's normal message buffer will be cleared,
C	  the status code will be stored in the CPL error buffer,
C	  the counts will be incremented, and
C	  the function returns with the success status.
C-------------------------------------------------------------------------
C
	INTEGER		MSG_SET
C
	INTEGER*4	IS
C
C
	IF (IAND(STAT,1).EQ.0) THEN
		CPL$ERRNTOT = CPL$ERRNTOT+1
		IF (CPL$ERRNTOT.GE.CPL__ERRNMAX) GOTO 999
		CPL$ERRBUF(CPL$ERRNTOT,1) = LINENR
		CPL$ERRBUF(CPL$ERRNTOT,2) = STAT
		IF (IAND(STAT,7).EQ.0) THEN
			CPL$ERRNWARN = CPL$ERRNWARN+1
		ELSE
			CPL$ERRNERR = CPL$ERRNERR+1
		ENDIF
	ENDIF
C
	CPL_ERR_PUT = CPL_SUCCESS
	RETURN
C
 999	CPL_ERR_PUT = MSG_SET(CPL_ERRCNTEXC,0)
	CPL$ERRBUF(CPL$ERRNTOT,1) = LINENR
	CPL$ERRBUF(CPL$ERRNTOT,2) = CPL_ERRCNTEXC
	CPL$ERRNERR = CPL$ERRNERR+1
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_ERR_SORT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
C
C.Purpose:	Sort the error array on ascending source-line number
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C.Notes:
C	The sort-algorithm is "quickersort" as described in
C	"Communications of the ACM" nr 271
C-------------------------------------------------------------------------
C
	INTEGER*4	k,q,m,p,lt(32),ut(32),qh,t(2),x(2)
C
C
	j = CPL$ERRNTOT
	i = 1
	m = 1
C
 10	IF (j-i.le.1) goto 100
C
C					Segment has more then 2 elements
C					- split it
C
	p = (i+j)/2
	t(1) = CPL$ERRBUF(p,1)
	t(2) = CPL$ERRBUF(p,2)
C
C					t are the values of
C					the mid-array elements :
C					t(1) = source line number
C					t(2) = error-code
C
	CPL$ERRBUF(p,1) = CPL$ERRBUF(i,1)
	CPL$ERRBUF(p,2) = CPL$ERRBUF(i,2)
	q = j
C
C					- Look for an element CPL$ERRBUF(k,1)
C					  > t(1) from segment begin
C					- Look for an element CPL$ERRBUF(q,1)
C					  < t(1) from segment end
C					- If found:
C						- interchange the entries
C						- look for next pair
C
	DO k = i+1,q
		IF (i+1.le.q .AND. CPL$ERRBUF(k,1).gt.t(1)) then
			DO q = q,k,-1
				IF (q.ge.k .AND. CPL$ERRBUF(q,1).lt.t(1)) then
					x(1) = CPL$ERRBUF(k,1)
					x(2) = CPL$ERRBUF(k,2)
					CPL$ERRBUF(k,1) = CPL$ERRBUF(q,1)
					CPL$ERRBUF(k,2) = CPL$ERRBUF(q,2)
					CPL$ERRBUF(q,1) = x(1)
					CPL$ERRBUF(q,2) = x(2)
					qh = q-1
					goto 45
				ENDIF
			ENDDO
			q = k-1
			goto 60
 45			q = qh
		ENDIF
	ENDDO
C
C					The pointer from segment start and
C					the one from segment end met each other
C
 60	CPL$ERRBUF(i,1) = CPL$ERRBUF(q,1)
	CPL$ERRBUF(i,2) = CPL$ERRBUF(q,2)
	CPL$ERRBUF(q,1) = t(1)
	CPL$ERRBUF(q,2) = t(2)
C
C					The segment has been split in 3 parts
C					(the middle of only 1 element)
C					- save begin- and end-positions of the
C					  largest segment in arrays lt and ut
C					- reset i and j (the begin- and end-
C					  positions of the next segment)
C
	IF (q*2.gt.i+j) then
		lt(m) = i
		ut(m) = q-1
		i = q+1
	else
		lt(m) = q+1
		ut(m) = j
		j = q-1
	ENDIF
C
C					Update and split this new segment
C
	m = m+1
	goto 10
C
C
C
 100	IF (i.lt.j) then
C
C					Segment is less then 2 elements long
C
		IF (CPL$ERRBUF(i,1).gt.CPL$ERRBUF(j,1)) then
C
C					Segment is 2 elements long
C
			x(1) = CPL$ERRBUF(i,1)
			x(2) = CPL$ERRBUF(i,2)
			CPL$ERRBUF(i,1) = CPL$ERRBUF(j,1)
			CPL$ERRBUF(i,2) = CPL$ERRBUF(j,2)
			CPL$ERRBUF(j,1) = x(1)
			CPL$ERRBUF(j,2) = x(2)
		ENDIF
	ENDIF
C
C					If lt and ut describe more segments
C					to be sorted: repeat the process,
C					if not: ready
C
	m = m-1
	IF (m.gt.0) then
		i = lt(m)
		j = ut(m)
		goto 10
	ENDIF
C
	CPL_ERR_SORT = CPL_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_ERR_GETMSG (LINENR,TEXT,LTEXT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
	INTEGER*4	LINENR		! (i) source-line number
	CHARACTER*(*)	TEXT		! (o) message text
	INTEGER*4	LTEXT		! (o) signif. length of text
C
C.Purpose:	Get the (next) message text associated with the source-line nr
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	false	status from GEN_GETMSG
C.Notes:
C	If there is no (more) entry for the line nr in the error buffer,
C	LTEXT = 0 will be returned.
C	When LINENR = 0 is given, the internal error pointer is reset.
C-------------------------------------------------------------------------
C
	INTEGER		GEN_GETMSG, MSG_SET
C
	INTEGER*4	PTR, IS, OUTADR
		DATA PTR /1/
		SAVE PTR
C
C
	TEXT = ' '
	LTEXT = 0
C
C					Reset pointer
C
	IF (LINENR.EQ.0) THEN
		PTR = 1
		CPL_ERR_GETMSG = CPL_SUCCESS
		RETURN
	ENDIF
C
C					Move pointer to the right linenr
C
	DO WHILE (PTR.LE.CPL$ERRNTOT .AND. CPL$ERRBUF(PTR,1).LT.LINENR)
		PTR = PTR+1
	ENDDO
C
C					Get the message text
C
	IF (PTR.LE.CPL$ERRNTOT .AND. CPL$ERRBUF(PTR,1).EQ.LINENR) THEN
		IS = GEN_GETMSG (CPL$ERRBUF(PTR,2),LTEXT,TEXT,15,OUTADR)
		IF (IAND(IS,1).EQ.0) GOTO 999
		PTR = PTR+1
	ENDIF
C
	CPL_ERR_GETMSG = CPL_SUCCESS
	RETURN
C
 999	CPL_ERR_GETMSG = MSG_SET (IS,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_ERR_GETSUM (NERR,NWARN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
	INTEGER*4	NERR		! (o) nr of error entries in errbuf
	INTEGER*4	NWARN		! (o) nr of warning entries in errbuf
C
C.Purpose:	Get the nr of error/warning entries in the error buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
C
C
	NERR = CPL$ERRNERR
	NWARN = CPL$ERRNWARN
C
	CPL_ERR_GETSUM = CPL_SUCCESS
	RETURN
	END
