!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!.Ident:        NGIDS.PSC
!.Keywords:     GIDS Image Display
!.Author:       Ger van Diepen (NFRA, Dwingeloo)
!.Language:     DWARF-Fortran
!.Environment:  UNIX
!.Comments:
!.Version:      GvD 920610      Created
!.              WNB 921211      Add INCLUDE
!.              HjV 930215      Add OPTION
!.              WNB 930325      Change some text; make PSC
!.              WNB 930330      Add DISPLAY
!.              WNB 930427      Change host into xhost
!.              WNB 930510      Delete DISPLAY, WINDOW, add options
!.              WNB 930514      Use MB2
!.              HjV 930518      Change some keywords
!.              WNB 930621      Add CLEAR, SAVE ,FLAG option; remove AREA option
!.              WNB 930622      Change some option names
!.              JEN 930723      Improve on-line HELP texts
!.              CMV 930914      Add keywords for DATA option
!.              CMV 931220      Improved some help texts
!.              CMV 940103      Added ALL_POL keyword, changed text for
!.                              SAVE and UNLOAD.
!.              CMV 940218      Added BLANK_FLAGS keyword
!.              CMV 940817      Add CLIPFLAG option
!               JPH 941205      Help texts, prompt formatting
!		JPH 950215	Typo
!-------------------------------------------------------------------------
!+
!!                      Terminology: Image already in use for maps. Picture?
!!                              Plane?
!  Get overall action
!       Ref:    NGIDAT
!
KEYWORD=OPTION
	DATA_TYP=C
	IO=I
	LENGTH=24
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="action"
	OPTIONS=-
MAP,DATA; DOFLAG,CLIPFLAG, FLAG; UNLOAD,WRITE, CLEAR;| -
GCLEAR,NOFLAG,QUIT; POINT
	HELP="
!!                      POINT??
Specify the action to perform.
.
   Loading/unloading visibility data or images for half-tone/colour display. The
   visibility 'planes' or images that you select are stored in display memory
   for rapid access. The number of pictures that you can store is limited by the
   availablity of this memory.
!!                      Correct?
.
	MAP      Load images(s) from map (.WMP) files.
	DATA     Load (corrected) data from visibility (.SCN) files
.
   Select mode for flagging of visibility data on the display. The flags you set
   will not yet be written back to the .SCN file, so you can freely experiment.
.
	DOFLAG   Switch to Flagging mode for .SCN-file visibilities
	CLIPFLAG Switch to Flagging mode using cliplevels
	FLAG     Flagging in .WMP maps using the PGPLOT cursor in GIDS
.
!! In this mode, no maps will be stored in GIDS (this allows you to flag in a
!! lot of channels etc in a single command, without overloading the GIDS
!! memory). In normal display mode, consequtive images will be stored in GIDS
!! for playback (this sequence is reset with the GCLEAR command).
!!                      Is this relevant?
.
   Saving the flags you set on your display:
.
	UNLOAD   Save flags list in a .FLF (binary file), from which you can
		  then transfer to the .SCN file through NFLAG LOAD.
	WRITE    Save flags list in an ASCII file, which you may manually
		  edit before transferringt it to the .SCN file through NFLAG
		  READ.
	CLEAR    Clear flags list.
!                       {\em see
!                       \textref{NFLAG OPS_FLIST}{nflag_private_keys.ops.flist}
!                       parameter }\
.
   Terminate current action sequence:
.
	GCLEAR   Remove a sequence of loaded pictures.
	NOFLAG   Switch back to normal display mode.
	QUIT     Leave NGIDS (the GIDS window will remain on the screen)."
!
!  Get compression factor
!       Ref:    NGIDAT
!
KEYWORD=MAP_COMPRESS
	DATA_TYPE=J
	SWITCHES=LOOP,NULL_VALUES,WILD_CARD
	CHECKS=MINIMUM
	MINIMUM=1
	DEFAULTS=1
	SEARCH=L,P
	PROMPT="display-size compression factor"
	HELP="
For a value N, the points in a N*N box are averaged into a single display
point. . Example:
	A 1024*1024 map loaded with N=2 will result in a 512*512 image."
!
!  Get output display range
!       Ref:    NGIDAT
!
KEYWORD=MAP_RANGE
	DATA_TYPE=R
	NVALUES=2
	SWITCHES=VECTOR
	CHECKS=ASCENDING
	SEARCH=L,P
	PROMPT="Data-value range for diaplay"
	HELP="
Data values outside the limuts will be truncated to the coresponding limit.
.
By choosing suitable limits you can concentrate the display on a particular
range of intensities in the map. The defaults shown are the extremes in the
image to be displayed.
"
!
!  Get type of DATA plot
!       Ref:    NGIDAT
!
KEYWORD=PLOT_TYPE
	DATA_TYP=C
	IO=I
	LENGTH=16
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	OPTIONS=IFRS,BASE,CHAN
!       PROMPT="(action)"
	HELP="
In displaying visibilities, you can choose between three two-dimensional cross
sections:
.
   IFRS   Hour angle (horizontal) versus interferometer (vertical), one picture
	   per frequency channel. Interferometers in the order
		....
.
   BASE   As IFRS, but interferometers in order of increasing baseline, - and of
	   increasing East telecope number within sets of redundant baselines.
.
   CHAN   Hour angle (horizontal) versus frequency channel (vertical), one
	   picture per interferometer.
.
NOTE:
	You select the channels to be displayed through the SCN_SETS parameter,
in the form <grp>.<obs>.<fld>.<CHN>. You have the liberty to select SCN_SETS
with channels for more than one <grp>.<obs>.<fld>. You may think of some good
use of this option (e.g. displaying the same channel for all fields in a
mosaic).
"
!
!  Get data types to plot
!       Ref:    NGIDAT
!
KEYWORD=DATA_TYPE
	DATA_TYP=C
	LENGTH=16
	NVALUES=1
	SWITCH=LOOP,NULL_VALUES,WILD_CARD
	CHECKS=ABBREV_OPTIONS
	SEARCH=L,P
	OPTIONS=AMPLITUDE,PHASE, COSINE,SINE
	DEFAULT=AMPLITUDE
	PROMPT="Select visibility component to display"
	HELP="
Specify how to display complex visibility values"
!
!  Get delete map range
!       Ref:    NGIDAT
!
KEYWORD=MAP_SEQUENCES
	DATA_TYPE=J
	NVALUES=2
	SWITCHES=VECTOR,WILD_CARD,NULL_VALUES,LOOP
	CHECKS=NON_DESCENDING,MINIMUM
	MINIMUM=1,1
	SEARCH=L,P
	PROMPT="Display-plane range to delete"
	HELP="
The display planes are numbered sequentially in the order in which they were
loaded. You may specify here the first and last value for a range of planes
that will be removed from the GIDS display memory.
"
!
!  Ask wether flagged data should be blanked
!       Ref:    NGIDAT
!
KEYWORD=BLANK_FLAGS
	DATA_TYP=L
	IO=I
	NVALUES=1
	SWITCHES=NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="Blank flagged data-points (Yes/No)?"
	HELP="
If you answer YES to this prompt, data points that are flagged will be set to
blank in the GIDS window.
.
A single red overlay plane is used to display flagged data points. This plane
will contain the flags for the last plane loaded. The BLANK_FLAGS option allows
you to discern flagged data in other pictures.
"
!
!  Get output file
!       Ref:    NFLFL5
!
KEYWORD=OUTPUT_FILE
	DATA_TYP=C
	IO=I
	LENGTH=80
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="(output filename)"
	HELP=" Specify the full name for the output disk-file."
!
!  Ask wether channels should be wildcarded
!       Ref:    NGIDAT
!
KEYWORD=ALL_CHAN
	DATA_TYP=L
	IO=I
	NVALUES=1
	SWITCHES=NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="Propagate flag changes to all channels (Yes/No)?"
	HELP="
If you answer Yes to this prompt, each flag will be set for all channels. That
is: If you set a flag in channel <i> it will be copied (in the flag list) to
the corresponding points in all other frequency channels."
!
!  Ask wether polarisations should be wildcarded
!       Ref:    NGIDAT
!
KEYWORD=ALL_POLS
	DATA_TYP=L
	IO=I
	NVALUES=1
	SWITCHES=NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	PROMPT="Propagate flag changes to all polarisations (Yes/No)?"
	HELP="
If you answer Yes to this prompt, each flag will be set for all polarisations.
That is: if you set a flag on an XX visibility it will be copied (in the flag
list) to the corresponding XY, YX and YY visibilities.
"
!!              What if you Clear a flag?
!
!  Get user specified flag to use
!       Ref:    NGIDAT
!
KEYWORD=USER_FLAG
	DATA_TYP=C
	IO=I
	LENGTH=16
	NVALUES=16
	CHECKS=ABBREV_OPTIONS
	SWITCHES=NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	OPTIONS=NONE,ALL; MAN,OLD,CLIP,NOISE,ADD,SHAD, U1,U2,U3
	PROMPT="Flags to use"
	HELP="
Each of the flagging modes (parameter OPTION, options FLAG, DOFLAG, CLIPFLAG)
uses a specific flag type by default. You may define here one ore more flags to
use instead. The use foreseen for this option is to experiment using one of
'user' flags without getting the experimental settings tangled up with settings
already in existence.
!               {\em see parameter \textref{OPTION}{.option} }
.
   NONE         revert to default types per flagging mode
   ALL or *     use all flag types (not a very sensible idea)
.
   MAN          use the flag type for the MANUAL class of operations
   CLIP         use the flag type for the CLIP class of operations
   NOISE        use the flag type for the NOISE class of operations
   SHAD         use the flag type for the SHADOW class of operations
   ADD          use the flag type for the ADDITIVE class of operations
.
   U1           use a separate flag for some user-defined operations
   U2           use a separate flag for some user-defined operations
   U3           use a separate flag for some user-defined operations
"
!
!  Get area
!       Ref:    NGIDAT
!
KEYWORD=AREA
	DATA_TYP=J
	IO=I
	NVALUES=4
	SWITCHES=LOOP,VECTOR,NULL_VALUES,WILD_CARD
	SEARCH=L,P
	PROMPT=" Display area (l,m, dl,dm)"
	HELP="
Specify the map area to be displayed. The coordinates are in grid units. (0,0)
is the map centre increase is toward the upper right (decreasing RA, incrasing
DEC). The area is defined by four values:
.
	l,m     area centre
	dl,dm   area size
"
!
!  Get output file
!       Ref:    NFLFL5
!
KEYWORD=NEXT
	DATA_TYP=C
	IO=I
	LENGTH=5
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	OPTIONS=YES,NO, ALL
	DEFAULT=ALL
	PROMPT="Load next picture"
	HELP="
This prompt appears after the loading of a picture. NGIDS will wait for your
reply while you may manipulate the GIDS display. Your reply options are:
.
	YES     Load next picture, then return with this prompt
	NO      Quit loading, return to OPTION prompt
	ALL     Load all remaining pictures without intervening consultations
"
!
!  Get cliplevel
!       Ref:    NGIDAT
!
KEYWORD=CLIP_LEVEL
	DATA_TYP=R
	IO=I
	NVALUES=1
	SWITCHES=LOOP,NULL_VALUES,WILD_CARD
	SEARCH=L,P
	PROMPT="Clip limit for flagging"
	HELP="
All visibilities that exceed the limit in the quantity displayed (cf. parameter
DATA_TYPE) will be flagged. Select a suitable level by consulting the display.
The colour bar to the left is annotated with values and you the pixel value
pointed at by the cursor is shown in the upper left corner.
!               {\em see parameter \textref{DATA_TYPE}{.data.type} }
"
!-
!
! NMODEL_PEF is included for model subtraction
!
INCLUDE=NMODEL_PEF
!
INCLUDE=NGEN_PEF
!-
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF
!-
INCLUDE=WMPNODE_PEF     !
INCLUDE=WMPSETS_PEF
!-
