C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	UDF_UNIT
C.Keywords:	UDF File, Units
C.Author:	Kasper Kombrink (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	840614 KK  - creation
C.Version:	850225 JPH - correct errors in table (there were many)
C.Version:	860917 GVD - wavelength group added to distance group;
C			added a few new units (MHz, kmh, mile, nautmile, etc.)
C.Version:	880929 GVD - added entry READ_UNITG_ALL
C.Version:	900111 FMO - Fortran version
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	931220 WNB - add SAVE for compiler cv/dw
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION READ_UNIT (UNIT,GROUP,FACTOR)
C	          ENTRY    READ_UNITG (GROUP,UNITLIST)
C	          ENTRY    READ_UNITG_ALL (GROUPLIST)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	READ_UNITG, READ_UNITG_ALL
C
	CHARACTER*(*)	UNIT		! (i) unit code
	CHARACTER*(*)	GROUP		! (o/i) unit group
	REAL*8		FACTOR		! (o) conversion factor
	CHARACTER*(*)	UNITLIST	! (o) list of all units in the group
	CHARACTER*(*)	GROUPLIST	! (o) list of all group names
C
C.Purpose:	Get information about a unit, unit group, or all groups
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	UDF_SUCCESS	success
C	error	UDF_UNINOTFND	invalid unit code
C	error	UDF_GRPNOTFND	invalid unit group
C	fatal	UDF_STRTOOSML	overflow in output string
C.Notes:
C	- UNITLIST and GROUPLIST are comma-separated lists.
C	- If UNIT(i) and UNIT(j) are units in the same group:
C		FACTOR(i) UNIT(i) = FACTOR(j) UNIT(j)
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, COMMA
		PARAMETER (BLANK = ' ')
		PARAMETER (COMMA = ',')
C
	INTEGER*4	NUNITS			! nr of unit codes
		PARAMETER (NUNITS=59)
	CHARACTER*10	G(NUNITS)		! unit group names
	CHARACTER*8	U(NUNITS)		! unit names
	REAL*8		F(NUNITS)		! conversion factors
	COMMON /UDF_UNIT_COM/ F,U,G
C
C
	DATA G(1) ,U(1) ,F(1)  /'ANGLE'     ,'RAD'    ,
	1						6.283185307179586477/
	DATA G(2) ,U(2) ,F(2)  /'ANGLE'     ,'DEG'    ,360	/
	DATA G(3) ,U(3) ,F(3)  /'ANGLE'     ,'CIR'    ,1	/
	DATA G(4) ,U(4) ,F(4)  /'ANGLE'     ,'DMS'    ,360	/
	DATA G(5) ,U(5) ,F(5)  /'ANGLE'     ,'HMS'    ,24	/
	DATA G(6) ,U(6) ,F(6)  /'ANGLE'     ,'ARCMIN' ,2.160D4	/
	DATA G(7) ,U(7) ,F(7)  /'ANGLE'     ,'ARCSEC' ,1.296D6	/
C
	DATA G(8) ,U(8) ,F(8)  /'FREQUENCY' ,'GHZ'    ,1D-9	/
	DATA G(9) ,U(9) ,F(9)  /'FREQUENCY' ,'MHZ'    ,1D-6	/
	DATA G(10),U(10),F(10) /'FREQUENCY' ,'KHZ'    ,1D-3	/
	DATA G(11),U(11),F(11) /'FREQUENCY' ,'HZ'     ,1	/
C
	DATA G(12),U(12),F(12) /'DISTANCE'  ,'PM'     ,1D15	/
	DATA G(13),U(13),F(13) /'DISTANCE'  ,'ANG'    ,1D13	/
	DATA G(14),U(14),F(14) /'DISTANCE'  ,'NM'     ,1D12	/
	DATA G(15),U(15),F(15) /'DISTANCE'  ,'MICRON' ,1D9	/
	DATA G(16),U(16),F(16) /'DISTANCE'  ,'MMM'    ,1D9	/
	DATA G(17),U(17),F(17) /'DISTANCE'  ,'MUM'    ,1D9	/
	DATA G(18),U(18),F(18) /'DISTANCE'  ,'MM'     ,1D6	/
	DATA G(20),U(20),F(20) /'DISTANCE'  ,'M'      ,1D3	/
	DATA G(21),U(21),F(21) /'DISTANCE'  ,'KM'     ,1	/
	DATA G(22),U(22),F(22) /'DISTANCE'  ,'INCH'   ,39398.4	/
	DATA G(23),U(23),F(23) /'DISTANCE'  ,'FT'     ,3283.2	/
	DATA G(19),U(19),F(19) /'DISTANCE'  ,'CM'     ,1D5	/
	DATA G(24),U(24),F(24) /'DISTANCE'  ,'YARD'   ,1094.4	/
	DATA G(25),U(25),F(25) /'DISTANCE'  ,'YRD'    ,1094.4	/
	DATA G(26),U(26),F(26) /'DISTANCE'  ,'MILE'   ,.621371192237334/
	DATA G(27),U(27),F(27) /'DISTANCE'  ,'NAUTMILE',.54	/
	DATA G(28),U(28),F(28) /'DISTANCE'  ,'AU'     ,
	1						6.68458581303615D-9/
	DATA G(29),U(29),F(29) /'DISTANCE'  ,'LY'     ,
	1						1.05702323231362D-13/
	DATA G(30),U(30),F(30) /'DISTANCE'  ,'PC'     ,
	1						3.24077884989914D-14/
	DATA G(31),U(31),F(31) /'DISTANCE'  ,'KPC'    ,
	1						3.24077884989914D-17/
	DATA G(32),U(32),F(32) /'DISTANCE'  ,'MPC'    ,
	1						3.24077884989914D-20/
C
	DATA G(33),U(33),F(33) /'VELOCITY'  ,'KMS'    ,1	/
	DATA G(34),U(34),F(34) /'VELOCITY'  ,'KMH'    ,3600	/
	DATA G(35),U(35),F(35) /'VELOCITY'  ,'MS'     ,1D3	/
	DATA G(36),U(36),F(36) /'VELOCITY'  ,'VC'     ,3.335635D-6/
	DATA G(37),U(37),F(37) /'VELOCITY'  ,'PCY'    ,1.022495D-6/
C
	DATA G(38),U(38),F(38) /'TIME'      ,'SEC'    ,1	/
	DATA G(39),U(39),F(39) /'TIME'      ,'MSEC'   ,1D3	/
	DATA G(40),U(40),F(40) /'TIME'      ,'MUSEC'  ,1D6	/
	DATA G(41),U(41),F(41) /'TIME'      ,'MMSEC'  ,1D6	/
	DATA G(42),U(42),F(42) /'TIME'      ,'NSEC'   ,1D9	/
	DATA G(43),U(43),F(43) /'TIME'      ,'MIN'    ,
	1						1.666666666666667D-2/
	DATA G(44),U(44),F(44) /'TIME'      ,'HR'     ,
	1						2.777777777777778D-4/
	DATA G(45),U(45),F(45) /'TIME'      ,'DAY'    ,
	1						1.157407407407407D-5/
	DATA G(46),U(46),F(46) /'TIME'      ,'YR'     ,3.1687536D-8/
	DATA G(47),U(47),F(47) /'TIME'      ,'SSEC'   ,1.002737875/
	DATA G(48),U(48),F(48) /'TIME'      ,'SYR'    ,3.1688765D-8/
C
	DATA G(49),U(49),F(49) /'FLUX'      ,'WU'     ,200	/
	DATA G(50),U(50),F(50) /'FLUX'      ,'MJY'    ,1D3	/
	DATA G(51),U(51),F(51) /'FLUX'      ,'JY'     ,1	/
	DATA G(52),U(52),F(52) /'FLUX'      ,'MUJ'    ,1D6	/
C
	DATA G(53),U(53),F(53) /'WEIGHT'    ,'KG'     ,1	/
	DATA G(54),U(54),F(54) /'WEIGHT'    ,'GR'     ,1D3	/
	DATA G(55),U(55),F(55) /'WEIGHT'    ,'MG'     ,1D6	/
	DATA G(56),U(56),F(56) /'WEIGHT'    ,'LBS'    ,2.20462262184878/
	DATA G(57),U(57),F(57) /'WEIGHT'    ,'OZ'     ,35.2739907229404/
C
	DATA G(58),U(58),F(58) /'NODIM'     ,BLANK    ,1	/
	DATA G(59),U(59),F(59) /'NODIM'     ,'1'      ,1	/
C
	INTEGER*4	STR_MATCH_A, STR_COPY_U
	INTEGER		MSG_SET
C
	INTEGER*4	IS, LG, LL, PTR, MATCH
C
C
C					Initialize output arguments
C
	GROUP = BLANK
	LG = 0
	FACTOR = 1
C
C					Check whether valid unit code
C
	IF (UNIT.EQ.BLANK) THEN
		MATCH = NUNITS-1
	ELSE
		IS = STR_MATCH_A (UNIT,NUNITS,U,MATCH)
		IF (IAND(IS,1).EQ.0) GOTO 991
	ENDIF
C
C					Fill output arguments
C
	PTR = 1
	IS = STR_COPY_U (BLANK,G(MATCH),PTR,GROUP,LG)
	IF (IS.LT.0) GOTO 992
	FACTOR = F(MATCH)
C
C					Return
C
	READ_UNIT = DWC_SUCCESS
	RETURN
C
 991	READ_UNIT = MSG_SET (UDF_UNINOTFND,1)
	CALL WNCTXT(DWLOG,DWMSG,UNIT)
	RETURN
C
 992	READ_UNIT = MSG_SET (UDF_STRTOOSML,0)
	RETURN
C
C
C	================
	ENTRY READ_UNITG (GROUP,UNITLIST)
C	================
C
C					Initialize output argument
C
	UNITLIST = BLANK
	LL = 0
C
C					Check whether valid unit group
C
	IS = STR_MATCH_A (GROUP,NUNITS,G,MATCH)
	IF (MATCH.EQ.0) GOTO 993
C
C					Fill output argument
C
	PTR = 1
	IS = STR_COPY_U (BLANK,U(MATCH),PTR,UNITLIST,LL)
	IF (IS.LT.0) GOTO 994
	MATCH = MATCH+1
	DO WHILE (MATCH.LE.NUNITS .AND. G(MATCH).EQ.G(MATCH-1))
		PTR = 1
		IS = STR_COPY_U (BLANK,COMMA//U(MATCH),PTR,UNITLIST,LL)
		IF (IS.LT.0) GOTO 994
		MATCH = MATCH+1
	ENDDO
C
C					Return
C
	READ_UNITG = DWC_SUCCESS
	RETURN
C
 993	READ_UNITG = MSG_SET (UDF_GRPNOTFND,1)
	CALL WNCTXT(DWLOG,DWMSG,GROUP)
	RETURN
C
 994	READ_UNITG = MSG_SET (UDF_STRTOOSML,0)
	RETURN
C
C
C	====================
	ENTRY READ_UNITG_ALL (GROUPLIST)
C	====================
C
C					Initialize output argument
C
	GROUPLIST = BLANK
	LL = 0
C
C					Fill output argument
C
	PTR = 1
	IS = STR_COPY_U (BLANK,G(1),PTR,GROUPLIST,LL)
	DO J = 2,NUNITS
	    IF (G(J).NE.G(J-1)) THEN
		PTR = 1
		IS = STR_COPY_U (BLANK,COMMA//G(J),PTR,GROUPLIST,LL)
	    ENDIF
	ENDDO
	IF (IS.LT.0) GOTO 995
C
C					Return
C
	READ_UNITG_ALL = DWC_SUCCESS
	RETURN
C
 995	READ_UNITG_ALL = MSG_SET (UDF_STRTOOSML,0)
	RETURN
	END
