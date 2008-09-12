!+ NGCALC.PSC
!  WNB 920819
!
!  Revisions:
!       WNB 920902      Add transpose option
!       WNB 921021      Add A3 plotter
!       WNB 921104      Text Select ifrs, HA_range max/min; J2000
!       WNB 921211      Make PSC
!       JEN 930308      INCLUDE=NSETS_PEF, remove keyord SETS,PLOTS
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) SCAN_NODE, CALC_NODE
!       JEN 930312      Remove keyword(s) SELECT_IFRS, POLAR, HA_RANGE
!       HjV 930426      Change name keywords PLOT_SET, OUT_NODE
!       WNB 930628      Change PLOT_ACTION into SET_ACTION
!       WNB 930630      Add SHIFT calculation; more functions
!       WNB 930824      Remove TELESCOPES
!       HjV 930914      Add keyword EDIT
!       WNB 931216      New EDIT format; use NSHOW.PEF
!       HjV 940428      Add IF options
!       CMV 940805      Add BASEline option
!       CMV 940811      Add MAX_BASE and POLY_USE
!       JPH 940812      Private version of SELECT_XYX with specific HELP
!                       Individual selection of INCLUDEd keywords
!       JPH 940823      Remove DELETE option. Add FULL option
!       JPH 940913      Remove () from promts. Reactivate DELETE option
!       JPH 940922      Correct text on comparisons in EXPRESSION help
!       JPH 941005      NGF_NODE keywords absorbed from NCOMM_PEF, use only
!                       NGF_NODE and OUTPUT_NGF_NODE
!                       May text improvements
!                       Remove WILDCARDS where inappropriate
!	CMV 941027	Remove option formatting
!	HjV 941031	Add MDLNODE_PEF
!	CMV 941122	Add HA_RANGE for SELECT
!	HjV 950530	Changed SET_ACTION in SECTOR_ACTION, add EDIT
!	HjV 950705	Use PLOTTER_PEF
!
!  Get action
!       Ref:    NGCDAT
!
KEYWORD=ACTION
	DATA_TYP=C
	IO=I
	LENGTH=24
	SWITCH=LOOP,NULL_VALUES
	CHECKS=ABBREV_OPTIONS
	SEARCH=L,P
	PROMPT="type of action |"
	OPTIONS=EXTRACT,COPY,CALC,MERGE,COMBINE,TRANS,BASE,MONGO,PLOT,-
SHOW,BRIEF,FULL,DELETE,NODE,CVX,NVS,QUIT
	HELP="
Specify the action to be performed:
.
   Creating cuts from external data:
	EXTRACT extract information from SCN file into cuts
	COPY    copy cuts to other NGF file, retaining their indices
		 (this action is mainly useful for discarding cuts no longer
		  needed and thereby freeing disk space)
.
   Viewing the contents of the .NGF file:
	SHOW    show information in cut headers and data
	BRIEF   show summary per group/field showing numbers of polarisations,
		interferometers, cuts and data points
.
   Calculations:
	CALC    Perform one of a set of predefined algorithms on individual cuts
	MERGE   Merge a number of cuts into a single new cut.
		NOTE: Overlapping points will be averaged so you may use MERGE
		      to average e.g. a set of interferometers with data points
		      at coincident hour angles.
	COMBINE Combine cut(s) in user-specified expression
.
   Sorting:
     Starting from a cut in the HA direction, these actions produce a new cut
in the frequency or baseline direction for each hour angle present in any of
the input cuts. Use the HA_RANGE parameter to limit the number of output cuts.
     The output cuts are in a primitive format in which the data are transposed
but NOT the coordinate axes. These wrong axes appear in plots of the transposed
cuts and must be interpreted as indicated below. In calculations, however,
coordinates will be interpreted correctly (provided you specify a sensible
calculation).
.
     Transposing a transposed group of cuts reproduces the original cuts. You
may also try to combine TRANS and BASE operations but sensible results are not
guaranteed.
.
	TRANS   transpose frequency and HA axes.
		In the transposed cuts, each channel is represented by an hour
		angle of <channel number>*0.125 deg.
		Each hour-angle bin of 0.125 deg is represented by a channel
		number, which starts at 0 for the lowest hour angle present.
.
	BASE    transpose interferometer and hour-angle axes.
		In the transposed cuts, each baseline bin of 10 metres is
		represented by an HA bin of .125 deg at HA = <baseline>/10*.125
		deg; baselines landing in the same bin are averaged.
		Each hour-angle bin of .125 deg is represented by a baseline
		sequence number of HA*.125.
.
   Plotting:
	MONGO   output cut data in a MONGO-readable file
	PLOT    plot data in NGF cut(s)
.
   Miscellaneous:
	DELETE  delete cuts. (Only the index linkage is removed but the cut data
		 remain in the file: Use COPY to free the file space.)
		BEWARE: This action is IRREVERSIBLE.
	NODE    switch to other NGF file
	QUIT    terminate NGCALC
.
   Data-format conversions:
	CVX     convert NGF file from other machine's format to local format
	NVS     update to latest NGF-file format "
!
!  Get input/output NGF node
!       Ref:
!
KEYWORD=NGF_NODE
	DATA_TYP=C
	IO=I
	LENGTH=80
	SWITCHES=LOOP,NULL_VALUES
	SEARCH=L,P
	PROMPT="Input[+output] 'node' name"
	HELP="
This is the file from which input cuts will be read.
.
Except in COPY operations, it is also the file to which new cuts will be
written.
.
Enter ** (or e.g. /user0/mydata/**) to get a list of files, then enter #<n> to
select the <n>-th file from such that list."
!
!  Get output NGF node
!       Ref: NGCCOP
!
KEYWORD=OUTPUT_NGF_NODE
	DATA_TYP=C
	IO=I
	LENGTH=80
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="Output 'node' name"
	HELP="
This is the file to which cuts will be copied.

The source and output files MUST be different.
.
Enter ** (or e.g. /user0/mydata/**) to get a list of files, enter #nn to select
the nn-th file from such a list."
!
!  Get extract action
!       Ref:    NGCEXT
!
KEYWORD=EXTRACT_TYPE
	DATA_TYP=C
	IO=I
	LENGTH=24
	SWITCH=LOOP,NULL_VALUES
	CHECKS=ABBREV_OPTIONS
	SEARCH=L,P
	OPTIONS=TCOR,ICOR,IFDATA,DATA,MODEL,WEIGHT,QUIT
	DEFAULTS=QUIT
	PROMPT="data type"
	HELP="
Specify the type of data to extract:
.
	DATA    Observed visibilities
	WEIGHT  Visibility weights
	MODEL   Model visibilities
.
	TCOR    Telescope gain/phase corrections
	ICOR    Interferometer gain/phase corrections
	IFDATA  'IF' data: gain/system-temperature parameters from the WSRT
		 on-line system; you will be asked for details later.
.
	QUIT    quit EXTRACT action
"
!
!  Get IF action
!       Ref:    NGCEXT
!
KEYWORD=IF_MODE
	DATA_TYP=C
	IO=I
	LENGTH=8
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP,NULL_VALUES
	SEARCH=L,P
	OPTIONS=TPON,TPOFF,TNOISI,TSYS,TSYSI,GAIN,GNCAL,TSYSI,TNOISI,RGAINI
	PROMPT="type of IF data"
	HELP="
Specify type of 'IF' data to extract:
.
   Quantities related to telescope noise sources:
	TPON    total-power data for noise source on
	TPOFF   total-power data for noise source off
	TNOISI  constant noise source temperature
.
   Quanitities related to telescope system temperatiures:
	TSYS    system temperatures
	TSYSI   constant system temperature
.
   Quantities related to interferometer gain correction:
	GAIN    IF gains
	GNCAL   gain correction method
	RGAINI  constant receiver gain
"
!
!  Get calculation action
!       Ref:    NGCCAL
!
KEYWORD=CALC_TYPE
	DATA_TYP=C
	IO=I
	LENGTH=24
	SWITCH=LOOP
	CHECKS=ABBREV_OPTIONS
	SEARCH=L,P
	OPTIONS=AVER,SMOOTH,POLY,CPOLY,DPOLY,SHIFT,NULL,QUIT
	DEFAULTS=QUIT
	PROMPT="calculation type"
	HELP="
Specify the action to be performed on individual cut(s).
.
	AVER    average cut and report results
.
    The following actions will create a new cut for each input cut with the same
    index numbers except for a new SEQ number.
.
	SMOOTH  create new cut(s) by smoothing cut data with a triangular
		 function (whose halfwidth you will specify)
.
	POLY    fit N+1 coefficients of an Nth-order polynomial
		   P= c0 +c1*HA +c2*HA**2 +... +cN*HA**N
		through cut data and create new cut(s) from the residuals
.
	CPOLY   as POLY, but fitting only N selected coefficients of an
		 Mth-order polynomial. M>N; the missing coefficients are held
		 at 0. (This method can be used, e.g., to fit a polynomial for
		 which you know a priori that it must be even.)
.
	DPOLY   subtract values of a polynomial (yet to be specified) from cut
		 data and create new cut(s) from the results
.
	SHIFT   create new cuts with visibility values transformed to a
		 different sky position
.
	NULL    create new cuts with data points in an HA range (yet to be
		 selected) deleted
.
	QUIT    leave CALC

Note:   All calculations are done with complex numbers. To use e.g. only
	amplitude, convert it first with an expression= AMPL(#..) in
	a COMBINE action.
"
!
!  Get smoothing width
!       Ref:    NGCCAL
!
KEYWORD=HA_WIDTH
	DATA_TYP=R
	IO=I
	SWITCH=LOOP
	SEARCH=L,P
	PROMPT="smoothing width"
	UNITS=DEG
	HELP="
Specify smoothing halfwidth of triangular smoothing function in degrees of HA."
!
!  Get polynomial degree
!       Ref:    NGCCAL
!
KEYWORD=POLY_N
	DATA_TYP=J
	IO=I
	SWITCH=LOOP
	SEARCH=L,P
	CHECKS=MAXIMUM,MINIMUM
	MAXIMUM=10
	MINIMUM=0
	PROMPT="number of coefficients to fit"
	HELP="
Specify number of coefficients of the polynomial to be fitted to the data.
.
For CALC=POLY, this is 1 + the polynomial's degree.
.
For CALC=DPOLY it is the number of non-zero coefficients. You will be prompted
later for there orders."
!
!  Get degrees of polynomial to fit
!       Ref:    NGCCAL
!
KEYWORD=POLY_USE
	DATA_TYP=J
	NVALUES=11
	IO=I
	SWITCH=LOOP,NULL_VALUE
	SEARCH=L,P
	PROMPT="orders of coefficients to fit"
	HELP="
Specify the orders of the polynomial coefficients to fit to. Default is
0,...,<N>, where <N> is the number you gave for POLY_N.
.
Example:
    To fit a 6th-degree even polynomial, specify POLY_N=3 and POLY_USE=0,2,4,6."
!
!  Get polynomial coefficients
!       Ref:    NGCCAL
!
KEYWORD=POLY_COEF
	DATA_TYP=R
	NVALUES=11
	IO=I
	SWITCH=LOOP,NULL_VALUE
	SEARCH=L,P
	PROMPT="polynomial coefficients"
	HELP="
Specify the coefficients of the polynomial to be subtracted:
.
	P= c0 +c1*HA +c2*(HA**2) +... +cN*(HA**N)
"
!
!  Get l,m shifts
!       Ref:    NGCCAL
!
KEYWORD=SHIFT
	DATA_TYP=R
	IO=I
	NVALUES=2
	SWITCHES=LOOP,VECTOR
	SEARCH=L,P
	PROMPT="l,m shifts to apply to data (arcsec)"
	HELP="
Specify the l,m shifts in arcsec to be applied to data"
!
!  Get plot device, plot-format
!       Ref:    NGCPLT
!-
INCLUDE=PLOTTER_PEF
!-
!
!  Get HA scale
!       Ref:    NGCPLT
!
KEYWORD=HA_SCALE
	DATA_TYP=R
	IO=I
	SWITCH=LOOP
	CHECKS=MINIMUM
	MINIMUM=0.
	SEARCH=L,P
	DEFAULT=15.
	PROMPT="hour-angle plot scale degree/cm"
!
!  Get Baseline range
!       Ref:    NGCPLT
!
KEYWORD=BAS_RANGE
	DATA_TYP=R
	NVALUES=2
	SWITCHES=VECTOR,NULL_VALUES,WILD_CARDS
	CHECKS=MAXIMUM,MINIMUM,NON_DESCENDING
	MINIMUM=0,3000
	MAXIMUM=0,3000
	UNITS=M,KM
	IO=I
	SEARCH=L,P
	DEFAULT=0,3000
	PROMPT="Baseline range (metres)"
	HELP="
Enter the the lower and upper ends of baseline range to plot."
!
!  Get Baseline scale
!       Ref:    NGCPLT
!
KEYWORD=BAS_SCALE
	DATA_TYP=R
	IO=I
	SWITCH=LOOP
	CHECKS=MINIMUM
	MINIMUM=0.
	SEARCH=L,P
	DEFAULT=150.
	PROMPT="Baseline plot scale (metres/cm)"
!
!  Get plot scale
!       Ref:    NGCPLT
!
KEYWORD=SCALE
	DATA_TYP=R
	IO=I
	SWITCH=LOOP
	CHECKS=MINIMUM
	MINIMUM=0.
	SEARCH=L,P
	DEFAULTS=10.
	PROMPT="plot scale (data units/mm)"
	HELP="
Specify the scale in units/mm."
!
!  Get plot offset
!       Ref:    NGCPLT
!
KEYWORD=OFFSET
	DATA_TYP=R
	IO=I
	SWITCH=LOOP
	SEARCH=L,P
	DEFAULTS=0.
	PROMPT="plot offset (data units)"
	HELP="
Example:
	If you enter 100, <data values>-100 will be plotted.
"
!
!  Get input plot
!       Ref:    NGCCOB
!
KEYWORD=USE_NGF_SET
	DATA_TYP=C
	IO=I
	LENGTH=32
	NVALUES=1
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="Cut to use"
	HELP="
Define the cut represented by each of the variables of the form
#<n> and ##<n> in your expression. You are being prompted for each such
variable that is present in the expression. Your reply must designate ONE cut."
!
!  Get expression
!       Ref:    NGCCOM
!
KEYWORD=EXPRESSION
	DATA_TYP=C
	IO=I
	LENGTH=80
	SWITCH=LOOP,NULL_VALUE,WILD_CARD
	SEARCH=L,P
	PROMPT="expression"
	HELP="
Specify the expression defining the new output cut. It is safest to enclose the
expression in double quotes.
.
Each data point in the output cut will be set to the value of your expression,
using the values of the corresponding data point in each of the input cuts in
the expression. Elements of an expression can be:
.
	References to input cuts of the form #<number> or ##<number> (see NOTE
	 below). You will be prompted later to identify one cut to be associated
	 with each of the numbers you use in the expression. For each output
	 point, a reference refers to the input point at the same hour angle.
.
	Real constants, e.g. 5, -1.23E-12
.
	Symbolic constants
		PI [=3.14..], EE [=2.71..], CC [=light velocity in km/s],
		DRAD [=180/PI]
.
	Standard functions of real or complex argument (angles always in
	 degrees):
		SIN(x), COS(x), ASIN(x), ACOS(x), ATAN(x), ATAN(real,imag)
		EXP(x), EXP10(x), EXP2(x), LOG(x), LOG10(x), LOG2(x)
		ABS(X), SQRT(X(
		REAL(x), IMAG(x), AMPL(x), PHASE(x), IMUL(x) (mmultiply x by i)
.
	Standard functions of a real argument that will be performed separately
	 on the real and imaginary components of a complex argument
		FLOOR(x), CEIL(x), ROUND(x), INT(x), FRACT(x)
.
	Cut-data-point coordinates:
		RA [right-ascension], DEC [declination], HA [hour-angle],
		UT [univ.time], FQ [freq. in MHz], BL [baseline in m],
		UU [U in lambda], VV [V in lambda]
.
	Operators, in order of decreasing priority (results of logical and
	 relation expressions are 0. (False) or 1. (True)):
.
		unary +,-
		**,^ (power)
		*, /, +, -
		>=,<=,=,<,>,<> (comparisons: if either value to be compared is
			   complex, absolute values are used in the comparison)
		<> (not)        & (and)         ! (or)
.
Example:
	#1*(REAL(#1)<0)+#2*(REAL(#2)>0): for each hour angle, take the data
		value from cut #1 if it is <0 and the data value from cut #2 if
		it is >0, and add them to get the data point at the same hour
		angle in the output cut
.
NOTE:
	You may loop over sectors in the standard way. Each of the input
sectors in the expression will be incremented at the start of a new loop cycle.
You may, however, inhibit this incrementing by using a double '#', e.g. ##2."
!
!  Get data action
!       Ref:    NGCPRT
!
KEYWORD=SECTOR_ACTION
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCHES=NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Sector-header action |"
        OPTIONS=SHOW,EDIT; NEXT,CONT,QUIT
        HELP=" Specify interaction with this sector header:
.
   Show details of the sector header:
        SHOW    show entire sector header
        EDIT    edit fields (values) in the Sector header by name
.
   Navigation:
        NEXT:   proceed to the header for the next sector selected
        CONT:   descend into the scans of this sector
        QUIT:   return to the file-header level"
!
!  Get data action
!       Ref:    NGCPRT
!
KEYWORD=DATA_ACTION
	DATA_TYP=C
	IO=I
	LENGTH=24
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP
	SEARCH=L,P
	PROMPT="data representation"
	OPTIONS=SHOW,AMPLITUDE,PHASE,QUIT
	HELP="
Specify action to perform:
.
	SHOW    show detailed cut data
.
	AMPL    show amplitude of cut data
	PHASE   show phase of cut data
.
	QUIT    quit data part"
!
!  Get cut type
!       Ref:    NGCMON
!
KEYWORD=PLOT_TYPE
	DATA_TYP=C
	IO=I
	LENGTH=24
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP
	SEARCH=L,P
	OPTIONS=COS,SIN,AMPLITUDE,PHASE
	PROMPT="cut-data component"
	HELP="
Specify the data component for the output cut:
.
	COS     real part
	SIN     imaginary part
	AMPL    ABS(data)
	PHASE   ARG(data)"
!
!  Get MONGO file name
!       Ref:    NGCMON
!
KEYWORD=MONGO_FILE
	DATA_TYP=C
	IO=I
	LENGTH=80
	SWITCH=LOOP
	SEARCH=L,P
	PROMPT="Mongo file name"
	HELP="
Specify the name of the file to be used in Mongo plotting."
!
!  Get max baseline to use
!       Ref:    NGCBAS
!
KEYWORD=MAX_BASE
	DATA_TYP=R
	IO=I
	SWITCH=LOOP,NULL_VALUE
	SEARCH=L,P
	PROMPT="max. baseline to include (metres)"
	UNITS=M
	HELP="
Specify maximum baseline to include in output cut."
!
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=NGFSETS_PEF
!
INCLUDE=SCNNODE_PEF:SCN_NODE            !
INCLUDE=SCNSETS_PEF                     !
INCLUDE=SELECT_PEF:SELECT_TELS,SELECT_IFRS,HA_RANGE
!-
INCLUDE=MDLNODE_PEF
!-
INCLUDE=NMODEL_PEF
!-
INCLUDE=NSHOW_PEF:FILE_ACTION,EDIT
!
! Polarisation selection - modified from SELECT_PEF
!! TO BE REPLACED WITH WNDPOH
!
KEYWORD=SELECT_XYX
	DATA_TYP=C
	LENGTH=4
	IO=I
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP,WILD_CARDS
	OPTIONS=XYX,XY,Y,X,YX,YYX,XXY
	SEARCH=L,P
	PROMPT="polarisation(s)"
	HELP="
Select the polarisation(s) to be used. Your answer will be interpreted
according to the data type you will select hereafter
.
		Interferometer          Telescope
		==============          =========
	XYX     XX, YX, YX and YY       X and Y
	XY      XX and YY               X and Y
	X       XX only                 X only
	Y       YY only                 Y only
	YX      XY and YX               none
	YYX     YX                      none
	XXY     XY                      none
"
!
