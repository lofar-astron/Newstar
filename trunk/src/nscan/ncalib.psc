!+ NCALIB.PSC
!  WNB 900306
!
!  Revisions:
!       WNB 910612      Add loops
!       WNB 910812      Add ALIGN
!       WNB 910820      Add extinction, refraction, Faraday rotation
!       WNB 910827      Add QDETAILS
!       WNB 910828      Add RUN
!       WNB 910909      Add DATAB and INFIX
!       WNB 910913      New (de-)apply and loops
!       WNB 910923      Add some polarisation info
!       WNB 911003      Add VZERO_OPTION
!       WNB 911007      Include instrum. pol.
!       WNB 911009      Add RENORM SET option
!       WNB 911230      NMODEL
!       WNB 920626      Add DCLOW, change Rotation measure description
!       WNB 921104      Text Select ifrs; HA range; J2000
!       WNB 921120      Change HA_INTEGRATION
!       WNB 921201      Allow gain/phase separate zero
!       WNB 921211      Make PSC
!       WNB 921217      Add CCOPY
!       JEN 930308      INCLUDE=NSETS_PEF, remove keyword SETS
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) SCAN_NODE
!       JEN 930312      Remove keyword(s) SELECT_IFRS, POLARISATION, HA_RANGE
!       HjV 930426      Change name keywords INPUT_SETS, INPUT_NODE
!       WNB 930602      Add CLK; IREFRACT_FILE, CLOCK_CORR
!       WNB 930603      Add BASEL_POLE, BASEL_DX, -DY, -DZ, FREQ_CORR
!       WNB 930617      Add SHIFT SET option; SHIFT
!       WNB 931123      Use CHECKS for complex loop also
!       WNB 931126      Add COMPLEX_ONLY
!       JPH 931201      Improve prompts for BASEL_*, FREQ_CORR
!       WNB 931216      UNITS=M removed (not accepted anymore on atleast dw,cv)
!       WNB 931221      UNITS=M back
!       CMV 940223      Add DSHIFT for differential shifts
!       CMV 940428      Add option INIT to SET_OPTION
!       CMV 940429      Add option IFRA en IFRM to SET_OPTION, add missing
!                       options to ZERO, add and change some text
!       JPH 940912      Conform to format requirements of doc_key
!                       Improve HELP texts
!                       Remove parentheses from prompt texts
!                       Remove SHOW_LEVEL 3, make 1,1 the default
!       JPH 941020      OPTIONS formats. - Remove invalid NULL_VALUEs,
!                        WILD_CARDS
!       JPH 941109      MDLNODE_PEF
!       JPH 941116      NULL_VALUES back in FARADAY_FILE, IREFRACT_FILE
!       JPH 941216      Some WILD_CARDS back. Remove all UNITS
!       JPH 941219      Donot use / as option delimiter
!       JPH 950124      Help texts
!       CMV 940927      Reorganise SET options, add option SET OTH MULTIPLY
!       CMV 950202      Restore above changes (removed by JPH)
!       WNB 950614      Add SAVE_RESIDUALS
!       WNB 950614      Change Complex loop count
!       HjV 950623      Add option ICOPY for SET_OPTION (Copy MIFR-corrections)
!       CMV 950725      Correct units for IREF
!       CMV 951212      Change text for FARAD
!       JPH 960513      Expand text for BASEL_DX, _DY, _DZ, _POLE
!       JPH 960802      WGT_FACTOR
!       JPH 970205      GAIN_NORM negative-values option
!	WNB 080711	Add INVERT SET option
!
!
!  Get overall action
!       Ref:    NCADAT
!
KEYWORD=OPTION
        DATA_TYP=C
        IO=I
        LENGTH=12
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Type of action"
        OPTIONS= REDUNDANCY,POLAR,SET; SHOW; QUIT
        HELP="
Specify type of action to perform:
.
   REDUNDANCY    create telescope corrections by solving the
                  redundancy/align/selfcal equations
   POLAR         operations on polarisation corrections
   SET           operations on all other corrections
.
   SHOW          show (in logfile) average corrections in specified sector(s)
   QUIT          finish"
!
!  Get polarisation action
!       Ref:    NCADAT
!
KEYWORD=POLAR_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="action on polarisation corrections|"
        OPTIONS=-
CALC,VZERO; COPY,SET,ZERO; SHOW,EDIT; QUIT
        HELP="
Specify action to perform on polarisation corrections:
.
   Calculate corrections from calibrator visibilities. The new correction values
   are ADDED to existing ones:
.
        CALC     calculate dipole corrections
.
        VZERO    select the set of operations dealing with the phase-zero
                 difference ('PZD') between the X and Y channel groups
.
   Copy/set corrections. The new corrections values generally OVERWRITE the
   existing ones:
.
        COPY     create dipole corrections in target sectors by copying them
                  from one source sector
.
        SET      set dipole corrections manually
.
        ZERO     zero dipole corrections
.
   Inspection:
.
        SHOW     show dipole corrections
.
        EDIT     edit dipole corrections
.
   Other:
.
        QUIT     exit from POLAR"
!
!  Get VZERO action
!       Ref:    NCADAT
!
KEYWORD=VZERO_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="VZERO action"
        OPTIONS=CALC,COPY,MANUAL; SCAN,APPLY,ASK; QUIT
        HELP="
This branch of NCALIB implements methods to define the unknown X-Y phase-zero
difference ('PZD') in the parallel-dipole configuration.
.
Correction values may be saved in an arbitrary selection of target sectors,
selected by the SCN_<xxx> parameters. The new corrections are OTH telescope
phases which will be ADDED to any existing ones in each of the sectors you
specify for output.
.
In determining the corrections, the program uses a collection of input sectors,
which may coincide with the target sectors or be selected by the USE_SCN_
parameters (for the COPY option). It is assumed that the source for these is a
calibrator with significant Stokes U and 'negligibly' small Stokes V. If this
condition is not fulfilled, the result will be meaningless.
.
The PZD is a phase value that is ADDED to the existing OTH phase correction of
the X channels. For this reason, you must be cautious to
.
        - avoid more than one COPY to the same target data and
        - avoid ruining a calibrator observation by doing an APPLY on it. (If
           you should do this by accident, use MANUAL to restore the observation
           to its previous state.)
.
Specify action to perform:
.
   Actions recommended for routine use:
.
        CALC    calculate PZD from the selected node and sectors (SCN_
                 parameters) and display it with its mean error. Use this option
                 to check if a calibrator observation is suitable for
                 determining the PZD.
.
        COPY    as APPLY, but use X-Y phase-difference value derived from
                 a calibrator observation (USE_SCN_<xxx> parameters). Use this
                 option to set the correction for your observations
.
        MANUAL  manually input phase-difference value
.
   Other actions:
.
        SCAN    as CALC, but per individual scan
.
        APPLY   as CALC, then apply the correction to the sectors selected
                 (SCN_<xxx> parameters).
                NOTE that once you have performed this operation, a CALC or COPY
                 operation on the same input data will yield PZD=0, so these
                 data can no longer be used to calibrate another observation.
.
        ASK     as APPLY, but display and ask for confirmation before modifying
                 the existing values
.
        QUIT    exit from VZERO "
!
!  Get set action
!       Ref:    NCADAT
!
KEYWORD=SET_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="SET action|"
        OPTIONS=-
QUIT; COPY,CCOPY,ICOPY,LINE; ZERO,MANUAL,INIT,RENORM,INVERT;| -
EXT,REF, IREF,FAR, IFR,MIFR; SHIFT, CLK, OTHER
        HELP="
Specify action to perform to alter correction values.
.
   Note: POLARISATION corrections are NOT included here. They must be handled
    sparately through the POLAR main option.
.
These option write correction values in (a) set(s) of target sectors (selected
by the SCN_<xxx> parameters). These corrections are copied/calulated from
corrections /data in the same file or another one (USE_SCN_<xxx> parameters).
The calculations will use all source sectors you specify to obtain a single set
of corrections. Unless stated otherwise below, the new values will be ADDED to
the existing ones in each of the target sectors.
.
   Transfer of corrections from a reference source:
.
        COPY    copy all corrections from somewhere else
                 (all corrections in the input set(s) are averaged)
        CCOPY   copy corrections from surrounding (in time) observations
                 in the input sets with the same frequency channel
                 (corrections for the two input sets are averaged).
        ICOPY   copy all MIFR corrections from somewhere else
                 (all corrections in the input set(s) are averaged)
        LINE    copy all corrections from corresponding continuum channel
.
   Initialisation:
.
        ZERO    zero corrections
        MANUAL  copy input values into the OTH telescope corrections for
                 selected scans
        INIT    make an initial estimate of telescope corrections and shift the
                 present values accordingly
        RENORM  renormalise telescope corrections
	INVERT	invert the signs of corrections. I.e. divide, rather
		 than multiply during application 
.
   Manual-input actions. Each option represents a correction type for which
   your values will be requested.

   - For the following actions, your values will OVERWRITE the existing ones:

        EXT     set extinction in selected scans
        REF     set refraction in selected scans
        IREF    set ionospheric refraction in selected scans
        FAR     set Faraday rotation in selected scans
        IFR     set additive interferometer corrections in selected scans
        MIFR    set multiplicative interferometer corrections in selected scans
        CLK     set clock correction in selected scans
        SHIFT   set (de-apply!) coordinate shift in selected sectors
.
        QUIT    exit SET "
!
!
!  Get set action
!       Ref:    NCADAT
!
KEYWORD=OTH_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=6
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        OPTIONS=MULT,POLE,DX,DY,DZ,FREQ,QUIT
        PROMPT="SET action (others)|"
        HELP="
Specify action to perform to ADD corrections into the OTHer telescope
corrections:

        MULT    set an extra gain factor (multiply existing values)
        DX      add telescope X correction in selected scans
        DY      add telescope Y correction in selected scans
        DZ      add telescope Z correction in selected scans
        POLE    add baseline pole correction in selected scans
        FREQ    add frequency offset correction in selected scans
.
        QUIT    exit SET "
!
!  Get input sets
!       Ref:    NCADAT
!
KEYWORD=USE_SCN_SETS
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=64
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Sets of input .SCN-file sectors: grp.obs.fld.chn.seq|"
        HELP="
Specify the .SCN-file sectors to be used as input in the calculations. "
!
!  Should calibrators be copied with equal length?
!       Ref:    NCADAT
!
KEYWORD=CAL_EQUAL
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        DEFAULT="NO /NOASK"
        PROMPT="Copy with equal length?"
        HELP="
If YES, all calibrators specified with USE_SCN_SETS will be made of equal
length, that is: the number of HA scans used for each calibrator will be the
same (and equal to the smallest number available).
.
This is necessary when calibrators have frequencies that have been offset (plus
and minus respectively) to the observing frequency of the target source. "
!
!!! WHY???
!
!
!  Get scan node
!       Ref:    NCADAT
!
KEYWORD=USE_SCN_NODE
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Input .SCN-file name"
        HELP="
Specify the input .SCN file from which the corrections should be calculated or
copied. A wildcard value ('*') indicates the same as the output .SCN file."
!
!  Select model weight type
!       Ref:    NCADAT
!
KEYWORD=MWEIGHT_TYPE
        DATA_TYP=C
        IO=I
        LENGTH=10
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES
        SEARCH=L,P
        PROMPT="Type of weight function|"
        OPTIONS=-
STEP,ISTEP; GAUSSIAN,IGAUSSIAN; TRIANGLE,ITRIANGLE
!               NCADAT saves last input as default for repeat
        HELP="
MWEIGHT_TYPE and MWEIGHT_DATA allow you to weigh baselines according to their
lengths, in order to take advantage of source characteristics that are a priori
known. The defaults for these parameters give all baselines the same weight.
.
This parameter selects the form of the weighting function; its position and
half-width will be defined by MWEIGHT_DATA. The following shapes can be chosen:
.
   STEP          a step function: 1 out to some radius, 0 beyond
   GAUSSIAN      a Gaussian
   TRIANGLE      a triangular function: linearly decreasing from 1 to 0
.
   Each of these can be prefixed with an 'I' to invert it, i.e. ISTEP means
    '1 minus STEP' "
!
!  Select model weight data
!       Ref:    NCADAT
!
KEYWORD=MWEIGHT_DATA
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,VECTOR,WILD_CARDS
        NVALUES=2
        CHECKS=MINIMUM
        MINIMUM=0,0
        SEARCH=L,P
        PROMPT="Centre, halfwidth (metres)"
!               NCADAT saves last input as default for repeat
        HELP="
Specify the centre and the halfpower-halfwidth in metres of the model weight
function to be applied.
.
Only positive values are accepted. Note that the function you define may extend
over negative baselines but actual baselines are positive by definition."
!
!  Get align/selfcal type
!       Ref:    NCADAT
!
KEYWORD=ALIGN_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Type of Selfcal desired"
        OPTIONS=SELFCAL,ALIGN
        HELP="
Selfcal is the generic name for all methods that use a source model in
combination with your visibilities to estimate the telecope gain/phase
corrections. Since you have selected a model, NCALIB assumes that you want to
use it, by doing a Selfcal. (If you had not, it would propose a REDUNdancy-only
solution.)
.
The alternative you have here is to use the source model to recover the unknown
position shift that scans suffer when they are processed with the  redundancy
constraints only (REDUN solution). In this operation the source model is only
used as a position reference. The telescope phases are shifted by an amount
that is a linear function of position along the array; the telescope gains do
not change at all.
.
To summarise, the choices you have are:
.
   SELFCAL       use the model to constrain in position a redundancy solution
                  that will be made now
   ALIGN         use the model to constrain a redundancy solution that was made
                  earlier "
!
!  Force freedom?
!       Ref:    NCADAT
!
KEYWORD=FORCE_FREEDOM
        DATA_TYP=L
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=2
        SEARCH=L,P
        PROMPT="  Manual constraints for gain (1st value)|-
and phase (2nd value)? YES/NO"
!                NCADAT saves last input as default for repeat
        HELP="
Specify if you want to force the constraint equations for the align solution.
If you reply NO, the program will determine them by itself from the
interferometers selected.
.
Your first reply refers to Gain, the second one to Phase."
!
!  Select gain freedom
!       Ref:    NCADAT
!
KEYWORD=GAIN_FREEDOM
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,VECTOR,WILD_CARDS
        NVALUES=14
        SEARCH=L,P
        PROMPT="Gain grouping"
        HELP="
Define the telescope groups for which the gains will be solved independently.
.
To solve e.g. separate gains for the 10 fixed telescopes (0 through 9) and the
 4 movable ones (A through D), specify:
.
        1,1,1,1,1,1,1,1,1,1, 2,2,2,2
.
* means one group containing all telescopes (i.e. 1,1,...,1,1). "
!!              Is * equivalent to NO ?
!
!  Select phase freedom
!       Ref:    NCADAT
!
KEYWORD=PHASE_FREEDOM
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=14
        SEARCH=L,P
        PROMPT="Phase grouping"
        HELP="
Define the telescope groups for which the phases will be solved independently.
.
To solve e.g. separate gains for the 10 fixed telescopes (0 through 9) and the
 4 movable ones (A through D), specify:
.
        1,1,1,1,1,1,1,1,1,1, 2,2,2,2
.
* means one group containing all telescopes (i.e. 1,1,...,1,1). Up to 14 groups
may be defined. "
!!              Is * equivalent to NO ?
!
!  Select gain telescopes
!       Ref:    NCADAT
!
KEYWORD=GAIN_NORM
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,WILD_CARDS
        NVALUES=14
        SEARCH=L,P
        PROMPT="Gain reference telescopes"
        HELP="
Define the telescopes to be used as reference for renormalising the telescope
gains.
.
Your reply should be an array of values (separated by commas) for telescopes 0
through D; 1 means that you select the telescope, 0 that you don't. Trailing 0s
may be omitted.
.
A wildcard ('*') means 'all', i.e. 1,1,1,1,1,1,1,1,1,1,1,1,1,1
.
Example:
        0,1,0,1,0 means 'use telescopes 1 and 3 as reference'
.
NOTES:
.
Renormalisation adjusts the gains so that the average for the fixed telescopes
selected equals that for the moving telescopes. If either group is absent from
yous selection, the average gain of the other group is made 0.
.
Consequently, proper gain renormalisation is possible only for the 'standard'
WSRT configuration: If there are any fixed-fixed or movable-movable
interferometers, a message will be given and the operation aborted.
.
You may, however, insist by putting a -1 instead of any of the 1s in your
reply. The renormalisation will then be performed as if the fixed-movable
interferometers were absent, and a warning given that the gain corrections for
those interferometers are jeopardized.    
"
!
!  Select phase telescopes
!       Ref:    NCADAT
!
KEYWORD=PHASE_NORM
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,WILD_CARDS
        NVALUES=14
        SEARCH=L,P
        PROMPT="Phase reference telescopes"
        HELP="
Define the telescopes to use as reference for renormalising the telescope
phases.
.
Your reply should be an array of values (separated by commas) for telescopes 0
through D; 1 means that you select the telescope, 0 that you don't. Trailing 0s
may be omitted.
.
A wildcard ('*') means 'all', i.e. 1,1,1,1,1,1,1,1,1,1,1,1,1,1
.
Example:
        0,1,0,1,0 means 'use telescopes 1 and 3 as reference"
!
!  HA integration
!       Ref:    NCADAT
!
KEYWORD=HA_INTEGRATION
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,WILD_CARD
        CHECKS=MAXIMUM
        MAXIMUM=3600.
        SEARCH=L,P
        PROMPT="Integration time (sec)"
        HELP="
Specify the time interval over which you want to integrate (if possible) before
calibrating. The value you specify will be rounded down to a multiple of the
hour-angle interval between successive scans.
.
'*' and '0' mean do not integrate, i.e. calibrate per scan.
.
The largest value allowed is 3600 (= 1 hour). "
!!              What happens if any point is missing?
!!              UT/ST ??
!
!  Do we want to save Selfcal residuals?
!       Ref:    NCADAT
!
KEYWORD=SAVE_RESIDUALS
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        DEFAULT="NO"
        PROMPT="Save the interferometer residuals as interferometer errors?"
        HELP="
If YES, the interferometer residuals after Selfcal, Align or Redun will be
saved for later use as Multiplicative Interferometer errors. "
!
!
!  Output level
!       Ref:    NCADAT
!
KEYWORD=SHOW_LEVEL
        DATA_TYP=J
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=0,0
        MAXIMUM=4,4
        SEARCH=L,P
        DEFAULTS=1,1 /ASK
        PROMPT="Levels of terminal (1st value)| and log (2nd value) output"
        HELP="
Specify the level of the type and print output you want:
.
   0     none
   1     sector numbers, errors and summary of results only (the summary in the
          log file includes phase constraints and solution details which are not
          shown on the terminal)
   2     detailed report (several pages!) per integration interval (parameter
          HA_INTEGRATION) - intended primarily for debugging purposes "
!
!  Get corrections to zero
!       Ref:    NSCSAZ
!
KEYWORD=ZERO
        DATA_TYP=C
        IO=I
        LENGTH=8
        NVALUES=12
        CHECKS=ABBREV_OPTIONS
        SWITCHES=WILD_CARDS
        SEARCH=L,P
        PROMPT="Corrections (plural) to be zeroed or inverted |"
        OPTIONS=-
ALL,NONE; RED,ALG,OTH; IFR,MIFR; EXT,REF,IREF,FAR, CLK,SHIFT;| -
NOGAIN,NOPHASE|
        DEFAULT=NONE
        HELP="
Specify any number of corrections you want to zero or invert:
.
   Generic:
        ALL or * All corrections
        NONE     None of the corrections: Return to previous prompt
.
   Telescope gains and phases:
        RED      Redundancy telescope corrections
        ALG      Align telescope corrections
        OTH      'Other' telescope gain/phase corrections
.
   Interferometer gains and phases:
        IFR      Additive interferometer corrections
        MIFR     Multiplicative interferometer corrections
.
   Corrections for the instrument as a whole:
        EXT      Extinction correction
        REF      Refraction correction
        IREF     Ionospheric refraction correction
        FAR      Faraday rotation
        CLK      Clock correction
        SHIFT    Coordinate shifts (de-apply!)
.
You may restrict the above to phase-only or gain-only by appending AT THE END
of your reply either of:
.
        NOGAIN   Zero only phases
        NOPHASE  Zero only gain"
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=GAIN_X
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
!       CHECKS=MAXIMUM,MINIMUM
!       MAXIMUM=100,100,100,100,100,100,100,100,100,100,100,100,100,100
!       MINIMUM=.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01
        SEARCH=L,P
        PROMPT="Gain corrections X (factors) |"
        HELP="
Specify the X gain corrections per telescope as factors (1= no change).
.
The values you give will be multiplied with the existing gain factors if you
choose SET option MULT, they will replace the existing factors if you choose
SET option MANUAL."
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=GAIN_Y
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
!       CHECKS=MAXIMUM,MINIMUM
!       MAXIMUM=100,100,100,100,100,100,100,100,100,100,100,100,100,100
!       MINIMUM=.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01
        SEARCH=L,P
        PROMPT="Gain corrections Y (factors) |"
        HELP="
Specify the Y gain corrections per telescope as factors (1= no change).
.
The values you give will be multiplied with the existing gain factors if you
choose SET option MULT, they will replace the existing factors if you choose
SET option MANUAL."
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=PHASE_X
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Phase corrections X (deg) |"
        HELP="
Specify the X phase corrections to be added per telescope (0 = no change).
.
The values you give will be added to the existing OTHer phases if you choose
SET option MULT, they will replace the existing phases if you choose SET option
MANUAL."
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=PHASE_Y
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Phase corrections Y (deg) |"
        HELP="
Specify the Y phase corrections to be added per telescope (0 = no change).
.
The values you give will be added to the existing OTHer phases if you choose
SET option MULT, they will replace the existing phases if you choose SET option
MANUAL."
!
!======================= IFR CORRECTIONS ==========================
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=GAIN_XX
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Interferometer gain corrections XX (factors) |"
        HELP="
Specify the gain corrections for XX as factors (1= no change). You will be
prompted for values, which will OVERWRITE the existing interferometer gain
corrections. "
!
KEYWORD=GAIN_XY
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer gain corrections XY (factors) |"
        HELP="
Specify the gain corrections for XY as factors (1= no change). You will be
prompted for values, which will OVERWRITE the existing interferometer gain
corrections. "
!
KEYWORD=GAIN_YX
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer gain corrections YX (factors) |"
        HELP="
Specify the gain corrections for YX as factors (1= no change). You will be
prompted for values, which will OVERWRITE the existing interferometer
corrections. "
!
KEYWORD=GAIN_YY
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer gain corrections YY (factors) |"
        HELP="
Specify the gain corrections for YY as factors (1= no change). You will be
prompted for values, which will OVERWRITE the existing interferometer
corrections. "
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=PHASE_XX
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer phase corrections XX (deg) |"
        HELP="
Specify the phase corrections for XX. You will be prompted for values, which
will OVERWRITE the existing interferometer corrections. "
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=PHASE_XY
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer phase corrections XY (deg) |"
        HELP="
Specify the phase corrections for XY. You will be prompted for values, which
will OVERWRITE the existing interferometer corrections. "
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=PHASE_YX
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer phase corrections YX (deg) |"
        HELP="
Specify the phase corrections for YX. You will be prompted for values, which
will OVERWRITE the existing interferometer corrections. "
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=PHASE_YY
        DATA_TYP=R
        IO=I
        NVALUES=1
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="interferometer phase corrections YY (deg) |"
        HELP="
Specify the phase corrections for YY. You will be prompted for values, which
will OVERWRITE the existing interferometer corrections. "
!
!  Get corrections
!       Ref:    NCADAT
!
KEYWORD=IFR_XX
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="additive interferometer corrections cos,sin XX (W.U.) |"
        HELP="
Specify the additive interferometer corrections for XX in Westerbork Units
(W.U.). The values you specify will OVERWRITE any existing values."
!
KEYWORD=IFR_XY
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="additive interferometer corrections cos,sin XY (W.U.) |"
        HELP="
Specify the additive interferometer corrections for XY in Westerbork Units
(W.U.). The values you specify will OVERWRITE any existing values."
!
KEYWORD=IFR_YX
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="additive interferometer corrections cos,sin YX (W.U.) |"
        HELP="
Specify the additive interferometer corrections for YX in Westerbork Units
(W.U.). The values you specify will OVERWRITE any existing values."
!
KEYWORD=IFR_YY
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="additive interferometer corrections cos,sin YY (W.U.) |"
        HELP="
Specify the additive interferometer corrections for YY in  Westerbork Units
(W.U.). The values you specify will OVERWRITE any existing values."
!
!  Get extinction coefficient
!       Ref:    NCADAT
!
KEYWORD=EXTINCTION
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=3
        SEARCH=L,P
        DEFAULT=.00557,.00461,-.000544
        PROMPT="Extinction-1: quadratic's coefficients|"
        HELP="
Specify the coefficients A, B and C in the quadratic representation of the
zenith extinction oefficient as a function of frequency F in GHz:
.
        EXT= (1+A) + B*F +C*F*F"
!
!  Get refraction coefficient
!       Ref:    NCADAT
!
KEYWORD=REFRACTION
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=3
        SEARCH=L,P
        DEFAULT=.00031,0.,0.
        PROMPT="Refraction-1: quadratic's coefficients|"
        HELP="
Specify the coefficients A, B and C in the quadratic representation of the
refraction coefficient as a function of frequency F in GHz:
.
        EXT= (1+A) + B*F +C*F*F"
!
!  Get clock correction
!       Ref:    NCADAT
!
KEYWORD=CLOCK_CORR
        DATA_TYP=R
        IO=I
        SWITCH=LOOP
        NVALUES=1
        SEARCH=L,P
        PROMPT="Clock correction (sec)"
        HELP="
Specify the clock correction in seconds to be applied to data. The new value
will be ADDED to the existing one."
!
!  Get baseline-pole correction
!       Ref:    NCADAT
!
KEYWORD=BASEL_POLE
        DATA_TYP=R
        IO=I
        SWITCH=LOOP
        NVALUES=1
        SEARCH=L,P
        PROMPT=-
"Increment to baseline-pole declination correction (deg) |"
        HELP="
Specify the baseline-pole declination correction in degrees.
.
The correction will be added as an HA-dependent phase to the OTH telescope
corrections. It can only be undone by either zeroing the entire OTH corrections
by means of the SET ZERO option (which will destroy whatever other corrections
have been stored there), or by inserting the same BASEL_POLE value with
opposite sign..
.
The default value 0 leaves the corrections unchanged."
!
!  Get frequency correction
!       Ref:    NCADAT
!
KEYWORD=FREQ_CORR
        DATA_TYP=R
        IO=I
        SWITCH=LOOP
        NVALUES=1
        SEARCH=L,P
        PROMPT="Increment to frequency correction (MHz)"
        HELP="
Specify the 'frequency correction' in MHz.
.
This is an idiosyncratic way of representing a correction to the metric scale
of the interferometer array: A scale correction with a factor (1+x) is
represented by correcting the nominal observing frequency F with an additive
term -x.F.
.
Clearly this does not represent a real frequency shift since the observing
frequency is precisely defined by the settings of local-oscillator frequencies
and fringe-stopping parameters during the observation.
.
The correction will be ADDED as an hour-angle-dependent phase to the OTH
telescope corrections, and can only be undone by either zeroing these
corrections, or inserting the same BASEL_POLE value with opposite sign. "
!
!  Get baseline X correction
!       Ref:    NCADAT
!
KEYWORD=BASEL_DX
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Increments to telescope X positions (mm) |"
        HELP="
Specify the X telescope corrections in mm, one value per telescope.
.
The correction will be added as an HA-dependent phase to the OTH telescope
corrections. It can only be undone by either zeroing the entire OTH corrections
by means of the SET ZERO option (which will destroy whatever other corrections
have been stored there), or by inserting the same BASEL_DX value with opposite
sign.
.
The default value 0,0,...0,0 leaves the corrections unchanged."
!
!  Get baseline Y correction
!       Ref:    NCADAT
!
KEYWORD=BASEL_DY
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Increments to telescope Y positions (mm) |"
        HELP="
Specify 14 Y telescope corrections in mm, one value per telescope.
.
The correction will be added as an HA-dependent phase to the OTH telescope
corrections. It can only be undone by either zeroing the entire OTH corrections
by means of the SET ZERO option (which will destroy whatever other corrections
have been stored there), or by inserting the same BASEL_DY value with opposite
sign.
.
The default value 0,0,...0,0 leaves the corrections unchanged."
!
!  Get baseline Z correction
!       Ref:    NCADAT
!
KEYWORD=BASEL_DZ
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="increments to telescope Z positions (mm) |"
        HELP="
Specify the Z telescope corrections in mm, one value per telescope.
.
The correction will be added as an HA-dependent phase to the OTH telescope
corrections. It can only be undone by either zeroing the entire OTH corrections
by means of the SET ZERO option (which will destroy whatever other corrections
have been stored there), or by inserting the same BASEL_DZ value with opposite
sign.
.
The default value 0,0,...0,0 leaves the corrections unchanged."
!
!  Get l,m shifts
!       Ref:    NCADAT
!
KEYWORD=SHIFT
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Shift to de-apply to data (arcsec)"
        HELP="
Specify the l,m shift in arcsec to be applied to data whenever the DE_APPLY
SHIFT correction is requested. (Ignore the 'de-apply) in the prompt.)
.
In the case where the shift is time-dependent (e.g. for a planet) it is
approximated by a linear function
.
        Total shift =  SHIFT + DSHIFT * (HA-HAB)
.
In this case the SHIFT you specify here is the value at the meridian, DSHIFT is
a second parameter for which you will be prompted.
!               \whichref{}{}
!!              HA-HAB or just HA?
"
!
!  Get l,m differential shifts
!       Ref:    NCADAT
!
KEYWORD=DSHIFT
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Shift rate (dl/dt, dm/dt, arcsec/day)"
        HELP="
Specify the SHIFT rate in arcsec per sidereal day to be applied to the data
whenever the DE_APPLY SHIFT correction is requested.
.
This parameter is used for a first-order approximation to the proper motion of
a solar-system object through the formula:
.
        Total shift =  SHIFT + DSHIFT * (HA-HAB)
.
where hour angles are measured as fractions of a full circle and HAB is the
starting HA of the sector.
!               \whichref{}{}.
"
!
!  Faraday rotation
!       Ref:    NCADAT
!
KEYWORD=FARADAY_FILE
        DATA_TYP=C
        IO=I
        LEN=160
        SWITCH=LOOP,NULL_VALUES
        SEARCH=L,P
        PROMPT="Faraday-rotation data file"
        HELP="
Specify the name of a file with Faraday rotation data. The file must be an
ASCII file with lines
.
        <hour angle in degrees>, <faraday rotation in degrees at 1GHz>
.
The file will be used to calculate Faraday rotation values for each scan by
interpolation between the values in this file and scaling to the observing
frequency. The calculated values will be stored in the scans as corrections. "
!  Ionospheric refraction
!       Ref:    NCADAT
!
KEYWORD=IREFRACT_FILE
        DATA_TYP=C
        IO=I
        LEN=160
        SWITCH=LOOP,NULL_VALUES
        SEARCH=L,P
        PROMPT="Ionospheric refraction data file"
        HELP="
Specify the name of a file with ionospheric refraction data. The file must be
an ASCII file with lines
.
        <hour angle in degrees>, <refraction in degrees/km at 1GHz>
.
The file will be used to calculate ionospheric refraction for each scan by
interpolation between the values in this file and scaling to the observing
frequency. The calculated values will be stored in the scans as corrections. "
!
!  Dipole position
!       Ref:    NCAPOL
!
KEYWORD=POL_ROTAN
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCH=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Dipole positions (deg)|"
        HELP="
Specify the position angles of the dual-dipole assemblies per telescope in
degrees."
!
!  Dipole orthogonality
!       Ref:    NCAPOL
!
KEYWORD=POL_ORTHOG
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCH=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Dipole orthogonalities (deg)|"
        HELP="
Specify the deviations from orthogonality in the dual-dipole assemblies per
telescope in degrees."
!
!  Dipole ellipticity (X)
!       Ref:    NCAPOL
!
KEYWORD=POL_X_ELLIPS
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCH=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="X-dipole ellipticities (%)|"
        HELP="
Specify the X-dipole ellipticities per telescope in %."
!
!  Dipole ellipticity (Y)
!       Ref:    NCAPOL
!
KEYWORD=POL_Y_ELLIPS
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCH=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Y-dipole ellipticities (%)|"
        HELP="
Specify the Y-dipole ellipticities per telescope in %."
!
!  Get X-Y phase difference
!       Ref:    NCAPVZ
!
KEYWORD=VZERO_PHASE
        DATA_TYP=R
        IO=I
        SWITCH=LOOP
        SEARCH=L,P
        PROMPT="X-Y phase-zero difference 'PZD' (deg)"
        HELP="
Specify the 'phase-zero difference' (PZD) in degrees.
.
In the parallel-dipole configuration, performing a Redundancy or Selfcal fit of
telescope errors to the visibilities introduces an unknown phase offset for the
set of telescope X channels and another unknown phase offset for the Y dipoles.
For correctly determining Stokes U and V, the (equally unknown) difference
between these must be corrected for.
.
This is a tricky problem and you should remain suspicious of the results that
you get. "
!!                      Sort this out
!
!  Specify details wanted
!       Ref:    NCADAT
!
KEYWORD=QDETAILS
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES
        SEARCH=L,P
        PROMPT="More details? (YES/NO)"
        HELP="
Specify if you want to specify details of the solution procedure yourself
rather than relying on NCALIB's expert defaults."
!
!  Baseline deviation check
!       Ref:    NCADAT
!
KEYWORD=BASEL_CHECK
        DATA_TYP=R
        IO=I
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=.001
        MAXIMUM=10.
        DEFAULTS=.5
!!      UNITS=M
        SEARCH=L,P
        PROMPT="Redundant-baseline difference allowed (metres)"
        HELP="
Specify the maximum difference between baselines for them still to be
considered identical in Redundancy calculations. "
!
!  Minimum allowable weight
!       Ref:    NCADAT
!
KEYWORD=WEIGHT_MIN
        DATA_TYP=R
        IO=I
        CHECKS=MINIMUM,MAXIMUM
        MAXIMUM=1.
        MINIMUM=0.
        DEFAULTS=0.01
        SEARCH=L,P
        PROMPT="Relative minimum weight accepted"
        HELP="
Specify the minimum relative weight of a data point that is still acceptable.
.
The weight is relative to the maximum weight in the same scan, and in most
cases can be seen as the minimum data amplitude accepted as fraction of the
maximum in the scan."
!
!  Force phase zero
!       Ref:    NCADAT
!
KEYWORD=FORCE_PHASE
        DATA_TYP=R
        IO=I
        NVALUES=14
        SWITCH=VECTOR
        SEARCH=L,P
        DEFAULTS=0,0,0,0,0,0,0,0,0,0,0,0,0,0
         PROMPT="Define phase-zeroes"
        HELP="
Define initial phase zeros per telescope.
.
This is useful for pathological cases where NCALIB on its own cannot correctly
resolve the 360-deg phase ambiguities. "
!
!  Continuity in solution
!       Ref:    NCADAT
!
KEYWORD=CONTINUITY
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        DEFAULTS=YES
        PROMPT="Continuity in solution (YES/NO)?"
        HELP="
Specify if you want continuity in gain solution. If not, the initial guess for
the solution will be 0 (gain) and the forced phases (phase), else the solution
found in a previous scan. "
!
!  Continuity in solution
!       Ref:    NCADAT
!
KEYWORD=FLW_FACTOR
        DATA_TYP=R
        IO=I
        SEARCH=L,P
        DEFAULTS=1
        PROMPT="Weight for points flagged with WGT flag"
        HELP="
The WGT flag directs NCALIB to multiply the data weight with an additional
weighing factor.
.
You may use this feature to reduce the weight of data affected by interference
without rejecting it completely. The effect of this in a Selfcal solution is
that it prevents the interference from bad interferometers to infect the
healthy ones.
.
A value of 1 will give all interferometers the weight they have in the .SCN
file. "
!
!  Select solution type
!       Ref:    NCADAT
!
KEYWORD=SOLVE
        DATA_TYP=L
        IO=I
        NVALUES=2
        SWITCH=VECTOR
        SEARCH=L,P
        DEFAULTS=YES,YES
        PROMPT="Solve for gain, phase (YES/NO, 2 values)?"
        HELP="
Specify if you want solutions for gain and for phase."
!
!  Complex solution
!       Ref:    NCADAT
!
KEYWORD=COMPLEX
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        DEFAULTS=YES
        PROMPT="Complex solution (YES/NO)?"
        HELP="
Specify if you want a complex solution of gains-plus-phases."
!
!  Complex solution only
!       Ref:    NCADAT
!
KEYWORD=COMPLEX_ONLY
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        DEFAULTS=NO
        PROMPT="Complex solution only (YES/NO)?"
        HELP="
Specify if you want ONLY a complex solution.
.
Note: will only work if an initial guess for the gains (e.g. from a calibrator)
has been specified "
!
!  Check deviations
!       Ref:    NCADAT
!
KEYWORD=CHECKS
        DATA_TYP=R
        IO=I
        NVALUES=3
        SWITCH=VECTOR
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=5,1E-6,1.
        MAXIMUM=50.,1.,20.
        DEFAULTS=20,1E-3,3
        SEARCH=L,P
        PROMPT="Iterations, gain deviation, relative mean error: 3 values|"
        HELP="
Specify:
.
   - the maximum number of iterations in the complex solution;
.
   - the relative allowable gain deviation for successive complex solutions;
.
   - the mean error allowed per scan, relative to the average mean error
        for all scans already solved."
!
!  "Channel Zero" for LINE option
!       Ref:    NCADAT
!
KEYWORD=CALCHAN
        DATA_TYP=J
        IO=I
        NVALUES=1
        SWITCH=LOOP
        CHECKS=MINIMUM
        MINIMUM=0
        SEARCH=L,P
        DEFAULTS=1,1 /ASK
        PROMPT="Channel to take calibration from (Channel Zero)"
        HELP="
Enter the number of the channel to take calibration data from.
Typically this is channel 0: the average channel for line datasets"
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF
!-
INCLUDE=MDLNODE_PEF     !
INCLUDE=NMODEL_PEF
!-
