!+ NPLOT.PSC
!  WNB 910617
!
!  Revisions:
!       WNB 910828      Add RUN
!       WNB 910909      Add DATAB and INFIX
!       WNB 910913      New (de-)apply and loops
!       WNB 911007      Include instrum. pol.
!       WNB 911217      Change halftone
!       WNB 911219      Change plot devices
!       WNB 911220      Add pol, ruled surface
!       WNB 911230      NMODEL
!       WNB 920423      Text for DLM coordinate option
!       WNB 920626      Add DCLOW, change Rotation measure description
!       WNB 920820      Change range explanation
!       WNB 920831      Add PLUVO
!       WNB 921021      Add A3 plotter
!       WNB 921104      Text Select ifrs; J2000
!       WNB 921211      Make PSC
!       HjV 921222      X11-plotter option partly available (NOT halftone)
!       JEN 930308      INCLUDE=NSETS_PEF, remove keyword SETS
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) SCAN_NODE, MAP_NODE
!       JEN 930312      Remove keyword(s) SELECT_IFRS, POLARISATION, HA_RANGE
!       WNB 930402      Add DISPLAY info
!       HjV 930426      Change name keyword ANGLE_SET
!       HjV 930722      Change PLUVO in IFR_MODE
!       WNB 930824      Remove TELESCOPES
!       HjV 940119      Add AP and CS to DATA_TYPES, add keyword
!                       PLOT_PER_PAGE, add BAND to IFR_MODE
!       CMV 940420      Add option NAME to PLOT_POSITIONS
!       CMV 940425      Add IF options
!       CMV 940622      Add EDIT option to PLOT_POSITIONS, add SOURCES, TEXT
!       CMV 940622      Add INTERFEROMETER option to OPTION
!       CMV 940628      Add ISYS option to IF_MODE
!       CMV 940817      Options to ignore pixel coordinate axes
!       JPH 940913      Correct ANGLE_WMP_SET prompt
!                       Remove () from promts
!       JPH 941025      NCOMM, NSETS --> SCNNODE/SETS, WMP_NODE/SETS, SELECT
!                       Fix damage from automatic line merging
!       JPH 941206      PLOTTER_PEF. Help texts, prompt formatting
!       JPH 950215      Typo
!       JPH 950818      HjV 941031 from master copy: Add MDLNODE_PEF
!                       HjV 950711 from master copy: Use PLOTTER_PEF, add
!                       annotation PLOT_HEADING
!                       Text mods
!       JPH 960126      Correct help text on IFR_MODE (bug 201)
!       HjV 960201      Correct wrong change made by JPH for PLOT_HEADING
!       JPH 960523      Change ST_ options in IFR_MODE into S_ options in OPTION
!       JPH 960619      Replace S_ option selection by OPTION=SPECIAL and
!                        HA_MODE
!       JPH 960805      ANNOTATION
!       JPH 961115      HA integration
!	WNB 970529	Add COORD_PREC
!	WNB 970605	Make default COORD_PREC 256
!
!
!
!  Get plot device
!       Ref:    NPLDAT
!
INCLUDE=PLOTTER_PEF     !
!
!  Get overall action
!       Ref:    NPLDAT
!
KEYWORD=OPTION
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Type of data to plot|"
        OPTIONS= MAP; DATA, RESID, MODEL; TELESC, INTERFM, IFDATA;|-
SPECIAL; QUIT
        DEFAULT="QUIT"
        HELP="
Specify type of data to plot:
.
   .WMP-file data:
.
        MAP              image(s) from .WMP file
.
   .SCN-file visibilities:
        DATA             observed visibilities
        MODEL            model visibilities
        RESIDUAL         visibility residuals (after correction of all known
                          errors and division by the visibilities of a source
                          model (yet to be specified)
                                (sets I=1, QUV=0)
.
   .SCN-file correction parameters:
.
        TELESCOPE        telescope phase/gain corrections
        INTERFEROMETER   interferometer phase/gain corrections (i.e. all
                          corrections combined per interferometer)
! I rechecked this 960523 - JPH
        IFDATA           IF-data: total powers, system temperatures etc.
.
   Plotting versus sidereal time i.s.o. hour angle:
.
        SPECIAL          will prompt for a special mode
.
        QUIT             terminate NPLOT "
!
!  Get special hour-angle plotting mode
!       Ref:    NPLDAT
KEYWORD=HA_MODE
        DATA_TYP=C
        IO=I
        LENGTH=10
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Special HA plot coordinates"
        OPTIONS=ST,IST, SEQUENCE,ISEQUENCE
        DEFAULT=""
        HELP="
This parameter selects a coordinate conversion for the vertical (HA) plot
coordinate in plots of .SCN-file entities.
.
    ST          Sidereal time i.s.o. HA. This is useful for plotting a series of
                 observations (e.g. calibrators-object-calibrators) in a time
                 sequence, e.g. to survey interference. Vertical coordinate is
                 ST in degrees
.
    SEQUENCE    Pseudo sidereal time: Sidereal time is forced into an ascending
                 sequence: When the start ST for a sector is less than that of
                 the one just plotted, it is changed to make the new sector
                 follow the previous one contiguously. Within each sector,
                 vertical scale size is that of HA or ST, but the sectors are
                 displaced in ST. Sectors are plotted in order of their index.
                This mode is useful to stuff a lot of information into a single
                 plot, e.g. to check for interference, but the plot may become
                 too confusing.
.
    I<xxx>      The prefix I indicates that you want to integrate scans; you
                 will be prompted for the HA interval over which to integrate.
                 As currently implemented, this mode is effective only for plots
                 that have HA or (pseudo)ST as vertical coordinate. The plot
                 scale for this coordinate will not be affected.
.
NPLOT will set plotting mode according to your reply and return to the OPTION
prompt. The mode will remain in force until you change it or NPLOT exits.   "
!
! Get integration time
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
!
! Get plot annotation
!
KEYWORD=ANNOTATION
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Annotation text, up to 80 characters in double quotes"
        HELP="
This text will be displayed on all plots for this NPLOT run until you change it
"
!
!  Get IF action
!       Ref:    NPLDAT
KEYWORD=IF_MODE
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Parameter to plot"
        OPTIONS=-
TPON,TPOFF, TSYS,ISYS, GAIN;  GNCAL, TSYSI,TNOISI, RGAINI
        HELP="
Specify action to perform:
.
   Telescope parameters:
.
        TPON          total power data (noise source off)
        TPOFF         total power data (noise source on)
        TSYS          system temperatures
        ISYS          system temperatures (X+Y)
        GAIN          IF gains
.
   Interferometer parameters:
.
        GNCAL         gain correction method
        TSYSI         constant system temperature
        TNOISI        constant noise source temperature
        RGAINI        constant receiver gain"
!
!  Get PLUVO action
!       Ref:    NPLDAT
!
KEYWORD=IFR_MODE
        DATA_TYP=C
        IO=I
        LENGTH=16
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        OPTIONS=NORMAL, INVERT, SORT;  SPECTRAL, BAND
        PROMPT="Select data cross-section"
        HELP="
Specify the cross section through the visibility data cube to be plotted:
        Consider the interferometers arranged in an upper-triangular matrix:
.
                00 01 02 ... 0B 0C 0D
                   11 12 ... 1B 1C 1D
                      22 ... 2B 2C 2D
                             :  :  :
                             BB BC BD
                                CC CD
                                   DD
.
Then the possible plotting modes are           
.
    visibilities as function of hour angle per interferometer:
.
        NORMAL  interferometer order in the matrix is row by row
.
        INVERT  interferometer order in the matrix is column by column  .      
        SORT    interferometers in order of ascending baseline
.
    other cross sections of the hour-angle/interferometer/channel cube:
.
        SPECTRAL visibilities as function of spectral channel and hour angle,
                  per interferometer (the WSRT 'PLUVO' format)
!!                      i.e. one plot per ifr?
.
        BAND    visibilities as function of channel and interferometer,
                 interferometer order as for NORMAL
!!                      for one HA or averaged?
"
!
!
!  Get angle input set
!       Ref:    NPLDAT
!
KEYWORD=ANGLE_WMP_SET
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=1
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="ONE position-angle map: grp.fld.chn.pol.0.seq"
        HELP="
Select a .WMP-file image holding polarisation position angles. If the image you
specify contains anything else, your plot will be garbage.
.
The .WMP-file indices are:
.
        group.field.channel.polarisation.type(=0).sequence_number"
!
!  Get data types to plot
!       Ref:    NPLDAT
!
KEYWORD=DATA_TYPES
        DATA_TYP=C
        LENGTH=16
        NVALUES=6
        SWITCH=LOOP,NULL_VALUES,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        PROMPT="Visibility component to plot"
        OPTIONS=AMPLITUDE,PHASE; AGAIN,PGAIN; COSINE,SINE;  AP,CS
        DEFAULT=AMPLITUDE
        HELP="
Specify the visibility component to be plotted.
.
        The quantity plotted depends on the data selected.  For TEL or INTERF
and for redund. RES       :       gain-1(%), phase(deg) For DATA:
                  ampl (WU), phase(deg For Selfcal RES with external model:

.
   Standard representations of complex data:
.
                        Model           Resid           Int.model
        AMPLITUDE
        PHASE
        COSINE
        SINE
.
   Instrumental gain/phase for DATA_TYPE=RES:
        AGAIN   Re log(data/model) * 100 = gain in %
        PGAIN   Im log(data/model) * 180/pi = phase in deg
.
   Old WSRT 'PLOTAP' formats:
.
        AP         amplitude/phase plots, one pair per page
        CS         cosine/sine plots, one pair per page
.

"
!
!  Get number of plots per page
!       Ref:    NPLDAT
!
KEYWORD=PLOTS_PER_PAGE
        DATA_TYPE=J
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR
        CHECKS=MAXIMUM,MINIMUM
        MAXIMUM=5,3
        MINIMUM=1,1
        DEFAULT=1,1 /ASK
        SEARCH=L,P
        PROMPT="Number of plots per page in hor. and vert. directions"
        HELP="
Specify number of plots to be plotted on one page."
!
!  Get map data types to plot
!       Ref:    NPLDAT
!
KEYWORD=DATA_TYPE
        DATA_TYP=C
        LENGTH=16
        SWITCH=LOOP,NULL_VALUES,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=DATA; SLOPE
        DEFAULT=DATA /ASK
        PROMPT="data types to plot"
        HELP="
Specify the data type(s) to be plotted.
.
   DATA or *    plot the data as given in the map
   SLOPE        plot the horizontal slope of the data (This option is still
                 experimental!)
!!                      purpose?
"
!
!  Get plot types
!       Ref:    NPLDAT
!
KEYWORD=PLOT_TYPE
        DATA_TYP=C
        LENGTH=16
        NVALUES=4
        SWITCH=LOOP,NULL_VALUES,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        PROMPT="Data representation(s)"
        OPTIONS=CONTOUR, HALFTONE, RULED;  POLARISATION
        HELP="
Specify the (combination of) data representations:
.
        CONTOUR      Contour plot
        HALFTONE     Halftone plot
        *            Equivalent to CONTOUR,HALFTONE
        RULED        Ruled-surface
.
        POLARISATION Pseudo-vectors of linear polarisation. This requires two
                      input maps, one holding polarisation strengths
                      sqrt(Q*Q+U*U) and the other position angles atan(U/Q)/2.
!               {\em Such maps are prepared with
!                \whichref{NMAP FIDDLE xxx}{} }
"
!
!  Get scale amplitude
!       Ref:    NPLDAT
!
KEYWORD=SCALE_AMPL
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM
        MINIMUM=0.
        SEARCH=L,P
        DEFAULTS=10.
        PROMPT="Magnitude scale (W.U./mm or %/mm)"
        HELP="
Specify the magnitude scale:
.
  in Westerbork Units /mm       for source/model visibilities and visibility
                                 residuals
  in percent/mm                 for telescope corrections
.
The ugly default for DATA_TYPE=RES is 4 times the maximum of the Redun/Align
amplitude noises recorded in the sector headers selected. For other DATA_TYPEs
it is a value that is likely to give reasonable output.  "
!!                      this must be incomplete!
!
!  Get scale phase
!       Ref:    NPLDAT
!
KEYWORD=SCALE_PHASE
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM
        MINIMUM=0.
        SEARCH=L,P
        DEFAULTS=1.
        PROMPT="Phase scale (W.U./mm or deg/mm)"
        HELP="
Specify the phase scale:
.
  in Westerbork Units /mm  for residuals
  in degrees/mm            for source/model visibilities; for telescope
                            corrections.
.
The default is a value that is likely to give reasonable output. "
!!                      verify
!
!  Get scale HA
!       Ref:    NPLDAT
!
KEYWORD=HA_SCALE
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM
        MINIMUM=0.
        SEARCH=L,P
        DEFAULT=1.
        PROMPT="HA plot scale (degree/cm)"
        HELP="
Specify the hour-angle scale in degree/cm."
!
!  Get scale polarisation
!       Ref:    NPLDAT
!
KEYWORD=POL_SCALE
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM
        MINIMUM=0.
        SEARCH=L,P
        PROMPT="Polarisation pseudo-vector length scale W.U./cm"
        HELP="
Specify the polarisation pseudo-vector length scale in Westerbork Units /cm."
!
!  Get scale ruled
!       Ref:    NPLDAT
!
KEYWORD=RULE_SCALE
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM
        MINIMUM=0.
        SEARCH=L,P
        PROMPT="Ruled-surface height scale (W.U./cm)"
        HELP="
Specify the ruled-surface height scale in Westerbork Units /cm."
!
!  Get area
!       Ref:    NPLDAT
!
KEYWORD=AREA
        DATA_TYP=J
        IO=I
        NVALUES=4
        SWITCHES=LOOP,VECTOR,NULL_VALUES,WILD_CARD
        SEARCH=L,P
        PROMPT="Area centre (l,m) and width (dl,dm) in grid units"
        HELP="
Specify an area of a map:
.
        l,m     grid coordinates of area centre: 0,0 is the map centre,
                 increasing to the upper right (i.e. with DEcreasing
                 right ascension and INcreasing declination)
.
        dl,dm   area width and height "
!
!  Get plot size
!       Ref:    NPLDAT
!
KEYWORD=SIZE
        DATA_TYPE=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR
        CHECKS=MAXIMUM,MINIMUM
        MAXIMUM=5.,5.
        MINIMUM=.001,.001
        DEFAULT=1.,1. /ASK
        SEARCH=L,P
        PROMPT="Plot scaling factors (horizontal, vertical)"
        HELP="
At this point, the plot has been dimensioned to fit on a single plotter page or
terminal screen, but will not necessarily fill it. You may blow it up in either
or both dimensions with the factors you specify here.
.
If necessary, the blown-up plot will be distributed over more tham one page.
!!              Explain default size
!!              ''(i.e. a QMS plot of a power of 2 length will be 12.8 cm).''
"
!
!  Get full drawn contours
!       Ref:    NPLDAT
!
KEYWORD=FULL_CONT
        DATA_TYPE=R
        IO=I
        NVALUES=32
        SWITCH=LOOP,NULL_VALUE
        SEARCH=L,P
        PROMPT="Full-contour levels"
        HELP="
Specify up to 32 values of the contours to be drawn as full lines. "
!
!  Get dotted contours
!       Ref:    NPLDAT
!
KEYWORD=DOT_CONT
        DATA_TYPE=R
        IO=I
        NVALUES=32
        SWITCH=LOOP,NULL_VALUE
        SEARCH=L,P
        PROMPT="Dotted-contour levels"
        HELP="
Specify up to 32 values of the contours to be drawn as dotted lines. "
!
!  Specify halftone
!       Ref:    NPLDAT
!
KEYWORD=HALFTONE
        DATA_TYP=C
        IO=I
        LENGTH=24
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=NONE,CONTINUE; STEP,PATTERN
        DEFAULT=NONE /ASK
        PROMPT="Halftone transfer function"
        HELP="
At this point, data values have been normalised to lie within the interval
[0,1]. Halftones are represented by the same interval: 0=white, 1=black. You
are now to define the transfer function F for mapping data values onto
halftones:
.
        halftone = F (normalised data value)
.
   Continuous functions:
.
        CONTINUE  a quadratic function (you will be prompted for the
                   coefficients)
        NONE      direct mapping: halftone level = normalised data value
.
   Discontinuous functions:
.
        STEP      F is a staircase function; halftone shades are generated by a
                   stochastic algorithm
        PATTERN   as STEP, but halftone shades are represented by a set of
                   fixed patterns
.
In selecting a method, bear in mind that the human eye is quite sensitive to
density variations in light shades while very poorly perceiving the same
variations in the dark shades; in other words, its response to density
variations is quasi-logarithmic.
.
To compensate for this, a quasi-exponential transfer function is suitable. The
best approximation to this available here is a steeply quadratic function (i.e.
specify CONTINUE here and consult the on-line help for the TRANSFORM parameter).
.
You may judge the quality of your transfer function from the grey-scale wedge
that will appear side by side with your plot. "
!
!  Get halftone range
!       Ref:    NPLDAT
!
KEYWORD=RANGE
        DATA_TYPE=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUE,WILD_CARD
        CHECKS=ASCENDING
        SEARCH=L,P
        PROMPT="Halftone saturation limits"
        HELP="
Specify the range of values to be covered by the full range of halftone shades.
.
The first value is the minimum to be represented by 'white', the second value
the to be represented by 'black'.
.
NOTES:
        Values outside this range will always be white. (If you think this is a
bad idea, please submit a Bug Report.)
        It is not possible to invert the scale by specifying a maximum<minimum."
!
!  Get halftone conversion
!       Ref:    NPLDAT
!
KEYWORD=TRANSFORM
        DATA_TYPE=R
        IO=I
        NVALUES=5
        MIN_NVALUE=3
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        PROMPT="Grey-scale transfer coefficients:|-
up to 5 groups of 2 limits plus 3 coefficients of quadratic|"
        HELP="
Specify the range coefficients for the CONTINUOUS quadratic transfer function
that you selected.
!               {\em parameter \textref{HALFTONE}{.halftone} }
.
Remember that the data at this point have been normalised to the range [0,1].
You may specify 5 values, of which the first three are REQUIRED:
.
        m,M     Range of normalised data values to be represented by the full
                 halftone range. Values outside this range will be truncated.
        a,b,c   The 0-th through 2nd-order coefficients in the transfer
                 quadratic.
.
The result will be ('ndv' = normalised data value):
.
        ndv < m:         OUT = 0                         (white)
        m < ndv < M:     OUT = a + b*IN + c*IN*IN        (grey scale)
        ndv > M:         OUT = 1                         (black)
.
You may break the IN range up into partial ranges by specifying multiple sets
of m,M,a,b,c separated by semicolons, or specifying the sets one by one as the
prompt is repeated. Input will be considered complete when you give no new
reply.
.
Examples:
        standard linear, halftone=ndv:  0,1, 0,1
        ndv distance from .5:           0,.5, 1,-2; .5,1, -1,2
        four grey levels:               0,.25,0; .25,.5,.25; .5,.75,.5; .75,1,1
        an approximation to an exp
         that seeks to match the
         quasi-logarithic response
         of the human eye:              0,1, 0,.1,.9 "
!
!  Get polarisation range
!       Ref:    NPLDAT
!
KEYWORD=POL_RANGE
        DATA_TYPE=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUE,WILD_CARD
        CHECKS=ASCENDING
        SEARCH=L,P
        PROMPT="Polarised-flux limits (W.U.)"
!!                      units?
        HELP="
No polarisation pseudo-vector will be drown if the intensity of linear
polarisation is below the lower limit (and therefore mainly noise); above the
upper limit it will be truncated to that limit.
.
Please specify the limits in Westerbork Units. "
!
!  Get polarisation type
!       Ref:    NPLDAT
!
KEYWORD=POL_TYPE
        DATA_TYP=C
        LENGTH=16
        SWITCH=LOOP,NULL_VALUES,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=POL,MAG
        PROMPT="Polarisation representation"
        HELP="
Specify if polarisation (POL) or magnetic field (MAG) should be plotted"
!
!  Get ruled range
!       Ref:    NPLDAT
!
KEYWORD=RULE_RANGE
        DATA_TYPE=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUE,WILD_CARD
        CHECKS=ASCENDING
        SEARCH=L,P
        PROMPT="Ruled-surface intensity range (W.U.)"
        HELP="
Specify the intensity limits in Westerbork Units for the ruled surface plot.
Values outside the limits will be truncated."
!
!  Specify annotation type
!       Ref:    NPLDAT
!
KEYWORD=COORD
        DATA_TYP=C
        IO=I
        LENGTH=24
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=|-
 NONE;  LM,  DEGREE, RADEC;  DLM,  DDEGREE, DRADEC;|-
ONONE; OLM, ODEGREE,ORADEC; ODLM, ODDEGREE,ODRADEC
        DEFAULT=NONE /ASK
        PROMPT="Axis annotation style"
        HELP="
Select ONE style of axis annotations:
.
        NONE     no annotation (only pixel coordinates)
.
   Relative quasi-Cartesian coordinates:
.
        LM       l, m in arcsec with respect to map centre
                  (or annotation for UV-plane plots)
        DLM      l, m in arcsec with respect to centre of plot
                  (or annotation for UV-plane plots)
.
   Equatorial coordinates:
.
        DEGREE   right ascension and declination in decimal degrees
        RADEC    right ascension (hhmmss) and declination (ddmmss)
        DDEGREE  relative right ascension and declination in decimal degrees
                  w.r.t.  centre of plot
        DRADEC   relative right ascension (hhmmss) and declination (ddmmss)
                  w.r.t. centre of plot
.
These annotations will be printed along the left and bottom sides of the plot.
.
By default, (l,m) pixel-coordinates are shown along the top and right axes side
irrespective of what you select. You may suppress these, by prefixing any of
the above options with an O for 'Only'.
.
Example:
        ONONE will suppress all annotations. "
!
!  Specify annotation type
!       Ref:    NPLDAT
!
KEYWORD=COORD_TYPE
        DATA_TYP=C
        IO=I
        LENGTH=24
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=TICK,DOTTED,FULL
        DEFAULT=TICK /ASK
        PROMPT="Coordinate grid style"
        HELP="
Select the style for plotting coordinate grid lines:
.
   TICK          give along plot edges only
   DOTTED        dotted grid
   FULL          full-drawn grid "
!
!  Specify source plotting
!       Ref:    NPLDAT
!
KEYWORD=PLOT_POSITIONS
        DATA_TYP=C
        IO=I
        LENGTH=24
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=NO,YES,NAMES; EDIT
        DEFAULT=NO /ASK
        PROMPT="Mark source positions"
        HELP="
Specify if you want model-source positions marked in your plot. The answers may
be:
.
        NO
        YES     position markers only
        MAMES   position markers annotated with their IDs from the model list
.
The marker symbol used is determined per source by its Type (which is the
suffix number in its ID).
.
        EDIT    invoke model-handling code to modify model-components' Types,
                 (FEDIT option), then return to this prompt. This path also
                 allows you to define additional annotations.
!\whichref{NMODEL HANDLE/EDIT}{nmodel_public_keys.}
!\whichref{parameter SOURCES}{}
!\whichref{parameter TEXT}{}
"
!
!  Specify source plotting
!       Ref:    NPLDAT
!
KEYWORD=SOURCES
        DATA_TYP=C
        IO=I
        LENGTH=10
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        DEFAULT=* /ASK
        PROMPT="Source pair for annotation"
        HELP="
Give the names of two sources that you have selected for plotting. A connecting
line will be drawn between them. You will be prompted for an annotation, which
defaults to the separation in degrees."
!
!  Specify source plotting
!       Ref:    NPLDAT
!
KEYWORD=TEXT
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        DEFAULT=* /ASK
        PROMPT="Annotation for source pair"
        HELP="
The annotation (max 80 characters) for the source pair just selected"
!
!  Specify heading plotting
!       Ref:    NPLDAT
!
KEYWORD=PLOT_HEADING
        DATA_TYP=C
        IO=I
        LENGTH=24
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        OPTIONS=YES,NO
        DEFAULT=YES /NOASK
        PROMPT="Plot heading (Yes/No) ?"
        HELP="
Specify if you want to have a MAP plot with or without the heading."
!
!  Specify precision for coordinate steps
!	Ref:	NPLDAT
!
KEYWORD=COORD_PREC
	DATA_TYP=J
	IO=I
	SWITCH=LOOP,NULL_VALUE,WILD_CARD
	CHECKS=MAXIMUM,MINIMUM
	MAXIMUM=32768
	MINIMUM=8
	DEFAULT=256 /NOASK
	SEARCH=L,P
	PROMPT="Number of steps for coordinate contouring near pole"
	HELP="
Specify the number of steps across the map to use in defining the
coordinate grid for contouring of coordinates near the pole."
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF
!-
INCLUDE=WMPNODE_PEF     !
INCLUDE=WMPSETS_PEF
!-
INCLUDE=MDLNODE_PEF     !
INCLUDE=NMODEL_PEF
!-
