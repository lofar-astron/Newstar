#!/bin/csh -f
#+scissor.csh
#
#  Some general commands that have knowledge of Scissor
#
#  CMV 941108	Created
#  CMV 941111	Added checkin command
#  CMV 941129   Added wsrtlog command
#  CMV 941212   Added chop command
#  CMV 950102   Added resize command
#  CMV 950104   Added oldtab command, code by HjV
#  CMV 950109   Add confirm command
#  CMV 950123	Modify checkin command
#  CMV 950124	Include "manual" for old tables
#  CMV 950131	Add overwrite command, add interface to Mosaic
#  CMV 950222   Typo in overwrite, improve interface to Mosaic
#  CMV 950404	Add calendar command
#  CMV 950410	Add f0f2 and ionos commands
#  CMV 950525	Add FILE argument to confirm
#  HjV 950720	Small changes on request Foley
#  CMV 950725	Option to specify station for ionos calculation
#  CMV 950728	Include option to import baseline packages (filpo)
#  CMV 960404   Include option to acknowledge observations.
#  CMV 960417   Changed mount command (mountcd -> mountopt)
#  CMV 960423   Correct missing quote
#  CMV 961105   Modify telescope calendar for use in MFFE commissioning
#  HjV 970521	Use == iso = to get only one Owner in scissor-commands
#
#  Syntax:  scissor help  will show all options
#set echo
#
#
#  Define some specific users
#
set Head    = foley,lems  # Head of reduction group
set TelAstr = foley,spoelstr,devoscm    # Telescope Astronomer

set Tmpfile=/tmp/scissor.$$
onintr abort_exit          # to get rid of tmpfile
#
#  If we want to get something more than help, we need a password
#
if (! $?SCIPWD) then
   if ("$1" == "") then
     set Tmp=$USER; setenv USER anonymous
     setenv SCIPWD $Tmp
     unset Tmp
     echo "**** anonymous access to Scissor ****"
   else if ("$1" !~ [Hh]*) then
     echo -n "Enter password for Scissor user ${USER}: "
     stty -echo; set Tmp=($<); stty echo; echo "*********"
     setenv SCIPWD $Tmp
     unset Tmp
   endif
endif

#
# Without arguments: standalone client
#
if ("$1" == "") then
   $n_exe/scissor.exe

#
# Help: show options
#
else if ("$1" =~ [Hh]*) then
   more <<_EOD_

Valid options (if arguments are missing, you will be prompted for them):

   scissor                                   - Start interactive client
   scissor help                              - Generate this list

   scissor mail    names...                  - Return list of eMail addresses

Standard commands for archiving:

   scissor checkin in-unit                   - Checkin a WSRT INF Tape or CD-ROM
   scissor archive in-unit out-unit [labels] - Archive a WSRT INF Tape or CD-ROM

Tools for correcting archiving errors:

   scissor overwrite unit [label]            - Remove labels from archive
   scissor resize    volume                  - Recalculate free space
   scissor delvolume volume                  - Remove volume and labels from archive

General tape/disk handling:

   scissor init    unit name description     - Mount, initialise new volumes
   scissor reinit  unit name description     - Mount and force initialise
   scissor ship    volume recipient          - Change volume owner and notify

Commands for handling of ionospheric data:

   scissor f0f2   [filename]                 - Enter new f0f2 values
   scissor ionos   date RA Dec Freq Station  - Calculate faraday rotation

 
Other commands for use by the reduction group:

   scissor wsrtlog yymmm user password       - Get WSRT logbook information

   scissor red     "String" seqno            - Confirm observation in logbook
   scissor ack     "String" seqno ...        - Acknowledge observations
      
   scissor filpo   file index                - Transfer baseline package

   scissor oldtab  volume label tableno      - Retrieve old calibration file
   scissor oldtab  calcode tableno           - Retrieve old calibration file

Other commands for use by the telescope astronomer:

   scissor calendar                          - Add calendar events


The Newstar start-up procedure may define aliases for some of the above.
_EOD_


#
# Mail returns a list of eMail addresses
#
else if ("$1" == "mail" || "$1" == "email") then
   set Name="$argv[2-]"
   if ("$Name" == "") then
      echo -n "Enter name(s): "
      set Name=($<)
   endif

   $n_exe/scissor.exe EMAIL=personal NAME=`echo $Name | tr ' ' ',' `

#
#
# Delvolume removes a full volume from the database
#
else if ("$1" == "delvolume") then

   setenv QEDDEBUG ""         # We want to get all output

   set Name="$2"
   set Ok=0
   while (! $Ok)
     if ("$Name" == "") then
        echo -n "Enter name of volume to calculate free space for: "
        set Name=($<)
     endif
   end

   $n_exe/scissor.exe delete=mediad volume=$Name label=1
   echo "Labels removed"

   $n_exe/scissor.exe delete=volumes volume=$Name
   echo "Volume removed"


#
# Resize recalculates the free space on the volume
#
else if ("$1" == "resize") then

   setenv QEDDEBUG ""         # We want to get all output

   set Name="$2"
   set Ok=0
   while (! $Ok)
     if ("$Name" == "") then
        echo -n "Enter name of volume to calculate free space for: "
        set Name=($<)
     endif

     echo ""
     echo "Checking $Name in Scissor..."
     if (-e $Tmpfile) rm -f $Tmpfile
     $n_exe/scissor.exe select=volumes volume=$Name >& $Tmpfile
     grep ^2 $Tmpfile 
     echo ""
     if (`grep -c ^207 $Tmpfile` != 1) then
        echo "No unique volume specified, try again"
        set Name=""
     else
        set Ok=1
        set Oldfree= (`grep ^207 $Tmpfile | awk '{ for (ii=1; ii<NF && $ii!="FREE="; ii++); printf("%d\n",$(ii+1)); }' `)
        set Size   = (`grep ^207 $Tmpfile | awk '{ for (ii=1; ii<NF && $ii!="SIZE="; ii++); printf("%d\n",$(ii+1)); }' `)
     endif
   end

   
   if (-e $Tmpfile) rm -f $Tmpfile
   $n_exe/scissor.exe select=mediad volume=$Name |& grep ^207 | awk '{ for (ii=1; ii<NF && $ii!="SIZE="; ii++); print $(ii+1); size+=$(ii+1) } END {print size}'
   set Used=(`$n_exe/scissor.exe select=mediad volume=$Name |& grep ^207 | awk '{ for (ii=1; ii<NF && $ii!="SIZE="; ii++); size+=$(ii+1) } END {printf("%d\n",size+0.5)}' `)

   @ Free = $Size - $Used

   unsetenv QEDDEBUG          # No need for output
   echo "According to Scissor `$n_exe/scissor.exe check=volumes volume=$Name`"
   if ($Free == $Oldfree) then
     echo "Free space correct, no need to change"
   else
     echo "Calculated: $Used used, $Size total, $Free free"
     $n_exe/scissor.exe put=volumes   volume=$Name free=$Free
     echo "Now: `$n_exe/scissor.exe check=volumes volume=$Name`"
   endif

#
#
# Ship changes file ownership, checks existence etc.
#
else if ("$1" == "ship") then

   setenv QEDDEBUG ""         # We want to get all output

   set Name="$2"
   set Ok=0
   while (! $Ok)
     if ("$Name" == "") then
        echo -n "Enter name of volume to ship: "
        set Name=($<)
     endif

     echo ""
     echo "Checking $Name in Scissor..."
     if (-e $Tmpfile) rm -f $Tmpfile
     $n_exe/scissor.exe select=volumes volume=$Name >& $Tmpfile
     grep ^2 $Tmpfile 
     echo ""
     if (`grep -c ^207 $Tmpfile` != 1) then
        echo "No unique volume specified, try again"
        set Name=""
     else
        set Ok=1
     endif
   end

   set Owner="$3"
   set Ok=0
   while (! $Ok)
     if ("$Owner" == "") then
        echo -n "Enter id of new owner: "
        set Owner=($<)
     endif

     echo ""
     echo "Checking $Owner in Scissor..."
     if (-e $Tmpfile) rm -f $Tmpfile
     $n_exe/scissor.exe select=personal name==$Owner >& $Tmpfile
     grep ^2 $Tmpfile 
     if (`grep -c ^207 $Tmpfile` != 1) then
        echo ""
        echo "No unique person specified, try again"
        set Owner=""
     else
        set Ok=1
     endif
   end

   if (-e $Tmpfile) rm -f $Tmpfile

   echo ""
   echo "Changing owner and sending mail"
   unsetenv QEDDEBUG          # No need for output
   $n_exe/scissor.exe put=volumes volume=$Name owner==$Owner ShipOut=Yes

#
#
# Checkin scans a tape and checks for parity errors
#
else if ("$1" == "checkin") then

   echo "------------------------------------------------------------"
   echo "Checkin/archiving procedure for WSRT tapes in Dwingeloo     "
   echo "------------------------------------------------------------"
   echo ""
   echo "Phase 1: List the tape, check for parity errors             "
   echo ""
   echo "After this phase, the tape-volume is known to Scissor       "
   echo "During this phase, a range of correct labels is determined  "
   echo "This range is kept for use in the next phase (archiving)    "
   echo "------------------------------------------------------------"
   echo ""
   
   $0 init-$$ "$2" I... Received

   set Mounted=( `cat $Tmpfile` )
   'rm' -f $Tmpfile
   set Unit  = $Mounted[1]
   set Input = $Mounted[2]

   if ("$Input" != "Unlabeled") then
      echo "Checking unit $Unit with volume $Input"
      source $n_src/sys/newstar_$n_site.csh
      dws nscan.checkin/nomenu <<_EOD_
OPTION=ARC
TYPE_TAPE=WSRT
ARC_OPTION=CHECK
INPUT_UNIT=$Unit
INPUT_LABELS=*
POINTING_SETS=1
#
_EOD_
      if ($Input =~ C* ) then
        dwe nscancd.checkin
        cat NSCAN*.LOG >$Input.checkin
      else
        dwe nscan.checkin
        cat NSCAN.LOG >$Input.checkin
      endif
 
      egrep -i '(Invalid)|(Cannot read)' NSCAN.LOG 
      
      echo ""
      echo "Now specify the range of labels to archive"
      echo "You can change this choice later at the archiving stage"
      echo "Specify as you would do in NSCAN DUMP (Start TO End or *)"
      echo "Enter labels to archive later: "
      set Labels=($<)
      set noglob
      $n_exe/scissor.exe PUT=volumes volume=$Input description=Use $Labels
      echo "------------ Selected labels for copy: $Labels --------------" \
                   >> $Input.checkin
      unset noglob
   endif

   echo ""
   echo "The next phase is to archive the data and inform Scissor"
   echo -n "Do you want to do this now (y,n) [y]? "
   set Tmp=($<)
   if ("$Tmp" == "" || "$Tmp" =~ [Yy]*) then
      set noglob
      $0 archive "$Unit" "" $Labels
      unset noglob
   else
      echo "***** Use command  scissor archive  later ******"      
   endif

   echo "Overview of INF tapes that need to be archived: "
   setenv QEDDEBUG ""
   $n_exe/scissor.exe select=volumes type=inf description=Use | grep ^20
#
#
# Archive scans a tape and checks for parity errors
#
else if ("$1" == "archive") then

   echo "------------------------------------------------------------"
   echo "Checkin/archiving procedure for WSRT tapes in Dwingeloo     "
   echo "------------------------------------------------------------"
   echo ""
   echo "Phase 2: Copy the tape and inform Scissor                   "
   echo ""
   echo "After this phase, the observations on the tape are known to "
   echo "Scissor, the data is copied to the archive medium and the   "
   echo "contents of both the tape and the archive medium are known  "
   echo "to Scissor. You can specify the range of labels to copy.    "
   echo "------------------------------------------------------------"
   echo ""
   
   echo "Checking the INPUT tape-unit"
   $0 init-$$ "$2" "" invalid
   set Mounted=( `cat $Tmpfile` )
   'rm' -f $Tmpfile
   set Unit  = $Mounted[1]
   set Input = $Mounted[2]

   if ("$Input" != "Unlabeled") then

      echo "Archiving from volume $Input on unit $Unit"
      echo "Checking the OUTPUT tape-unit"
      $0 init-$$ "$3" "" invalid
      set Mounted=( `cat $Tmpfile` )
      'rm' -f $Tmpfile
      set OutUnit  = $Mounted[1]
      set Output   = $Mounted[2]
   else 
      set Output="Unlabeled"
   endif

   if ("$Output" != "Unlabeled") then
      echo "Archiving to volume $Output on unit $OutUnit"
      echo ""
      echo "You have to specify the labels to archive" 

      set noglob
      setenv QEDDEBUG ""
      $n_exe/scissor.exe select=volumes volume=$Input | tr '=' '\012' |& \
         awk '/Use/ {for (i=2; i<NF; i++) printf("%s ",$i); }' >$Tmpfile
      unsetenv QEDDEBUG
      echo "Labels suggested from checking the tape: `cat $Tmpfile`"

      if ("$4" != "") then
         set Labels=($argv[4-])
      else
         set Labels=(`cat $Tmpfile`)
      endif
      'rm' -f $Tmpfile

      echo -n "Enter range of labels to copy [$Labels]: "
      set Tmp=($<)
      if ("$Tmp" != "") set Labels=($Tmp)
      
      echo "Starting to pass observations on volume $Input to Scissor"
      source $n_src/sys/newstar_$n_site.csh
      dws nscan.checkin/nomenu <<_EOD_
OPTION=ARC
TYPE_TAPE=WSRT
ARC_OPTION=ARCHIVE
INPUT_UNIT=$Unit
INPUT_LABELS=$Labels
POINTING_SETS=1
#
_EOD_
      if ("$Input" =~ C*) then
         dwe nscancd.checkin
         cat NSCAN*.LOG >$Input.admin
      else 
        dwe nscan.checkin
        cat NSCAN.LOG >$Input.admin
      endif

      echo "Starting to copy from $Input to $Output"
      dws nscan.copy/nomenu <<_EOD_
OPTION=DUMP
INPUT_UNIT=$Unit
INPUT_LABELS=$Labels
OUTPUT_UNIT=$OutUnit
OUTPUT_LABEL=0
#
_EOD_

      if ("$Input" =~ C*) then
         dwe nscancd.copy
         cat NSCAN*.LOG >$Input.copy
      else 
#        setenv tmp $n_exe
#        setenv n_exe /newstar/master/exe/hp
        dwe nscan.copy
        cat NSCAN.LOG >$Input.copy
#        setenv n_exe $tmp
      endif

      $n_exe/scissor.exe PUT=volumes volume=$Input description=Done $Labels

   endif
#
#
# Overwrite deletes labels from an archive medium
#
else if ("$1" == "overwrite") then

   echo "------------------------------------------------------------"
   echo "Overwrite labels on WSRT archive medium                     "
   echo "------------------------------------------------------------"
   echo ""
   
   echo "Checking the archive unit"
   $0 init-$$ "$2" "" invalid
   set Mounted=( `cat $Tmpfile` )
   'rm' -f $Tmpfile
   set Unit  = $Mounted[1]
   set Input = $Mounted[2]
   set Type = $Mounted[3]
   set Device = $Mounted[4]

   if ("$Type" != "disk") then
      echo "Can only remove files from Unix optical disk"

   else if ("$Input" != "Unlabeled") then

      echo "Removing from volume $Input on unit $Unit"

      echo "You have to specify the labels to overwrite" 
      if ("$3" != "") then
         set Label=($argv[3])
      else
         set Label=(abort)
      endif

      echo -n "Enter the first label to overwrite [$Label]: "
      set Tmp=($<)
      if ("$Tmp" != "") set Label=($Tmp)
      
      if ("$Label" != "" && "$Label" !~ [Aa][Bb][Oo][Rr][Tt]) then
         echo "Removing labels $Label and higher from Scissor"
         $n_exe/scissor.exe DELETE=mediad volume=$Input label=$Label

         echo "Removing labels $Label and higher from $Device"
         touch $Tmpfile
         set Home=$cwd
         cd $Device
         ls file*.mt | awk '{print substr($1,5,6),$1}' |\
          awk '{if ($1>='$Label') printf("chmod a+w %s; rm -f %s\n",$2,$2);}'\
          >$Tmpfile
         set echo
         source $Tmpfile
         unset echo
         'rm' -f $Tmpfile
      endif

   endif
#
#
# Init checks for volume labels and initialises if necessary
# Re-init forces initialize
# For internal use:  init-side        initialise other side
#                    init-<digit(s)>  write unit and name to scissor.<...>
#
else if ("$1" =~ *[Ii][Nn][Ii][Tt]* ) then

   set Unit="$2"
   while ("$Unit" == "") 
      echo -n "Enter number of tapeunit [list of units]: "
      set Unit=($<)
      if ("$Unit" == "") printenv | grep MAG | sort
   end

   if (`printenv MAG$Unit` == "") then
      echo "No such unit $Unit known (MAG$Unit undefined)"
      set Device="Unknown"
      set Type="Unknown"

   else

      #
      #   Split device type and true name
      #
      set Device=(`printenv MAG$Unit | tr ':' ' ' `)
      if ($#Device == 1) then 
         if ("$Device" =~ //*) then
            set Type="rmtd"
         else
            set Type="tape"
         endif
      else 
         set Type=$Device[1]
         set Device=$Device[2]
      endif

      if ("$Type" == "rmtd") then
         echo "Cannot initialise an rmtd device"
         set Device="Unknown"

      else if ("$Type" == "tape") then
  
         if (! -w $Device || -f $Device) then
            echo "Cannot mount MAG$Unit on $Device
            set Device="Unknown"   
         else 
            set Name=(`$n_exe/genaid.exe label $Device`)
            set Defnam="DDS"
            set Side=""
         endif
  
      else if ("$Type" == "disk") then
 
         if ($Device =~ *cd*) then
            cdload
         else
           if (! -d $Device) mountopt
           if ( "`df $Device | grep -v File`" !~ /opt* ) mountopt   # Temporary...
         endif
         if (! -d $Device) then
            echo "Cannot mount MAG$Unit on $Device
            set Device="Unknown"   
         else 
           if (-e $Device/volume.mt) then
              set Name=(`cat $Device/volume.mt | tr -d '[:cntrl:]'`)
           else if (-e $Device/VOLUME.MT) then
              set Name=(`cat $Device/VOLUME.MT | tr -d '[:cntrl:]'`)
           else
              set Name="Unlabeled"
           endif
           set Defnam="DO"

           set Side=""
           if (-e $Device/SIDE_A) set Side="A"
           if (-e $Device/SIDE_B) set Side="B"
         endif
      endif

   endif

   set Did_init=0       # Not initialised anything yet

   if ("$1" == "re-init") then  # Force init
       set Name="Unlabeled"
       echo "Ignore existing label, force initialization"
   endif

   if ("$Device" == "Unknown") then
      set Name="Unlabeled"
   else
      if ("$Name" == "Unlabeled") then
         set Name="$3"
         while ("$Name" == "" || "$Name" == "Unlabeled")
           echo -n "Enter a label for the volume in MAG${Unit} [new $Defnam]: "
           set Name=($<)
           if ("$Name" == "") set Name=${Defnam}...${Side}
           if ("$Name" =~ *.*) then
              if (-e $Tmpfile) rm -f $Tmpfile
              $n_exe/scissor.exe check=volumes volume=$Name | tee $Tmpfile
              if ($status == 1) then
                 echo "Sorry, Scissor could not figure out the next label"
                 echo "You will have to think of it yourself..."
              else
                 set Name=(`grep next $Tmpfile`)
                 set Name=$Name[1]
                 echo "New label: $Name"
              endif
              if (-e $Tmpfile) rm -f $Tmpfile
           endif
         end

         set Tmp="$Name"
         if ("$Type" == "tape") then
            $n_exe/genaid.exe init $Device $Name
            set Name=(`$n_exe/genaid.exe label $Device`)
         else if ("$Type" == "disk") then
            if (-e $Device/volume.mt) then
               chmod a+w   $Device/volume.mt
            endif
            echo $Name >$Device/volume.mt
            chmod a-w   $Device/volume.mt
            set Name=(`cat $Device/volume.mt`)
         endif

         if ("$Tmp" != "$Name") then
            echo "Could not initialise volume $Tmp on MAG$Unit ($Name)"
            set Name="Unlabeled"
         else
            echo "Volume $Name has been initialised, now informing Scissor..."
            set Did_init=1
         endif
      endif

      if ("$Name" != "Unlabeled") then
         echo ""
         echo "Volume on MAG3 is $Name" 
         $n_exe/scissor.exe check=volumes volume=$Name
         if ($status == 1) then
            if ("$4" == "") then
               echo -n "Enter a description for $Name [none]: "
               set Descr=($<)
            else
               set Descr="$4"
            endif

            if ("$Descr" != "invalid") then
               
               if ("$Type" == "disk")                   set Medium=OPT
               if ("$Type" == "disk" && "$Name" =~ C*)  set Medium=CD
               if ("$Type" == "tape")                   set Medium=DAT
               if ("$Type" == "tape" && "$Name" =~ I*)  set Medium=INF

               $n_exe/scissor.exe put=volumes volume=$Name medium.type=$Medium owner=$USER Description=$Descr
               $n_exe/scissor.exe check=volumes volume=$Name
               if ($status == 1) then
                  echo "Obscure error while informing Scissor..."
                  set Name="Unlabeled"
               endif
             
             else
               echo "$Name not known to Scissor"
               set Name="Unlabeled"
             endif

         endif
      endif


      if ($Did_init && "$Side" != "" && "$1" != init-side) then
         umountopt
         echo "Please remove the disk and put the other side in..."
         echo "Press Enter when done."
         set Tmp=($<)
         mountopt
         if (($Side == B && -e $Device/SIDE_B) || \
             ($Side == A && -e $Device/SIDE_A)) then
            echo "This is not the other side of $Name"
            echo "Try to initialise it later..."
         else 
            set Name=(`echo $Name | tr 'AB' 'BA'`)
            $0 init-side "$Unit" $Name 
         endif
      endif

   endif

   #
   # Save unit and name in tempfile of calling process
   #
   if ("$1" =~ init-[0-9]*) then
      set Tmp=(`echo $1 | tr '-' ' ' `) 
      set Tmp=/tmp/scissor.$Tmp[2]
      if (-e $Tmp) then
        'rm' -f $Tmp
      endif
      echo $Unit $Name $Type $Device >$Tmp
   endif

#
# 
#  wsrtlog gets logbook information from the WSRT
#
else if ("$1" == "wsrtlog") then

   set Names=(JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC)

   set Year  = `date +%y`
   set Month = `date +%m`
   set This=$Names[${Month}]19${Year}OBS.TXT

   @ Month = $Month - 1
   if ("$Month" == 0) then
      set Month = 12
      @ Year = $Year - 1
   endif
   set Last=$Names[${Month}]19${Year}OBS.TXT

   if ("$2" == "") then
      echo -n "Enter year and month to receive (yymmm) [current month]: "
      set ym=($<)
   else 
      set ym=($2)
   endif
   set ym=(`echo $ym | tr '[a-z]' '[A-Z]'`)
   if ("$ym" == "") then
      echo "Will import $This and $Last"
   else 
      set This=`echo $ym | awk '{ printf("%s19%sOBS.TXT",substr($1,3,3),substr($1,1,2)); }' `
      set Last=""
      echo "Will import $This"
   endif

   if ("$3" == "") then
      echo -n "Enter username on wsrt00: "
      set wsrt_user=($<)
   else
      set wsrt_user=$3
   endif

   if ("$4" == "") then
      echo -n "Enter password for ${wsrt_user}@wsrt00: "
      stty -echo; set wsrt_pass=($<); stty echo; echo "*********"
   else
      set wsrt_pass=$4
   endif

   set Names=(JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC)

   #
   # If we do not have a file, get one
   #
   echo "Retrieving logbook from wsrt00..."
   if ("$Last" == "") then
      set getLast=""
   else
      set getLast="get $Last"
   endif
   ftp -n wsrt00 <<_EOD_
user $wsrt_user $wsrt_pass
cd /disk3/obs/log/obslogbook
get $This
$getLast
quit
_EOD_

   set Files=($Last $This)

#   else
#     set Files=($argv[4-])
#   endif

   #
   # For each file: convert file to series of scissor commands and send them
   #
   setenv QEDDEBUG 1
   foreach File ($Files)
     if (-e $File) then

        echo "Inserting file $File"
   
        perl $n_src/sys/obslog.pls $File:r         
        cat wsrtlog.$File:r | sed -e 's%<%\&lt;%g' -e 's%>%\&gt;%g' | $n_exe/scissor.exe >& $File:r.err
     else
        echo "Could not find file $File"
     endif
   end     

   #
   # Remove files retrieved from wsrt00
   #
   if ("$4" == "") then
      'rm' -f $Files
   endif

#
#
# f0f2 will add f0f2 values to the database
#
else if ("$1" == "f0f2") then
   $n_exe/ionos.exe $2 | $n_exe/scissor.exe

#
# ionos will calculate faraday rotations for use with NCALIB
#
else if ("$1" == "ionos") then
   
   set Date="$2"
   echo -n "Enter date (dd/mm/yyyy) [$Date]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Date="$Tmp"

   set Ra="$3"
   echo -n "Enter right ascension (deg) [$Ra]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Ra="$Tmp"

   set Dec="$4"
   echo -n "Enter declination (deg) [$Dec]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Dec="$Tmp"

   set Freq="$5"
   if ("$Freq" == "") set Freq="1000"
   echo -n "Enter frequency (MHz) [$Freq]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Freq="$Tmp"

   set Station="$6"
   if ("$Station" == "") set Station="all"
   echo -n "Enter station(s) to use [$Station]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Station="$Tmp"
   if ("$Station" =~ [Aa][Ll][Ll]*) set Station=""    # Do not select on it

   set DMY=(`echo $Date | awk -F/ '{printf("%2.2d %2.2d %4.4d %2.2d",$1,$2,$3,$3-1900);}' `)

   echo "Retrieving f0f2 information from Scissor into f0f2-file..."
   $n_exe/scissor.exe <<_EOD_ | $n_exe/ionos.exe -w $DMY[3] $DMY[2]
SELECT=F0F2 DATE=01/$DMY[2]/$DMY[3] - 31/$DMY[2]/$DMY[3] STATION=$Station
SELECT=YPF2 DATE=01/$DMY[2]/$DMY[3] - 31/$DMY[2]/$DMY[3] STATION=$Station
bye
_EOD_
 
   echo ""
   echo "Calculation Faraday rotation/Ionospheric refraction values"
   source $n_src/sys/newstar_$n_site.csh
   setenv n_f0f2 $cwd                      # f0f2 file made in current dir

   dws ionost.calc/nomenu <<_EOD_
DAY_NUMBER=$DMY[4]$DMY[2]$DMY[1];0
FREQUENCY=$Freq
SOURCE_NAME=scissor
RIGHT_ASCENSION=$Ra
DECLINATION=$Dec
#
_EOD_
   
   if (-e ionos.rf) then
     'rm' -f ionos.rf
   endif

   dwe ionost.calc

   'rm' -f ION*.LOG

   if (! -e ionos.rf) then
      echo "Error in IONOST: no file ionos.rf produced"
   else
      echo "Output of IONOST is in file ionos.rf"

#
# Fix to handle blank fields.
#
      cat ionos.rf | awk '{ \
if ($1 ~ /U.T./ && $NF ~ /11.00/ )  hour=0    \
if ($1 ~ /U.T./ && $NF ~ /23.00/ )  hour=12   \
if ($1 ~ /hour/ && $2 ~/angle/){              \
    for (i=0; i < 12 ; i++) {                 \
      ha[hour+i] = substr($0,(26+8*i),8)}     \
}                                             \
if ($1 ~ /Faraday/ && $2 ~/rotation/){        \
    for (i=0; i < 12 ; i++) {                 \
      fr[hour+i] = substr($0,(26+8*i),8)      \
			 }                    \
}                                             \
if ($1 ~ /delta/ && $2 ~/phase/){             \
    for (i=0; i < 12 ; i++) {                 \
      iref[hour+i] = substr($0,(26+8*i),8)    \
			 }                    \
}                                             \
}                                             \
END { for (u=0; u < 24; u++)  printf("%.2f %.2f %.2f \n",ha[u],fr[u],iref[u]);}' \
      | sed '/0.00 ........./d' | sort -n |\
      awk '{ print $1,$2 >"far"; print $1,$3>"ref"}'

      set out = NCA$DMY[3]$DMY[2]$DMY[1]_${Ra}_${Dec}
      mv far $out.FAR
      mv ref $out.REF

 if (0) then
      set ha = ( `grep "hour angle"  ionos.rf | awk -F: '{ print $2}'` )
      set fr = ( `grep "Faraday rot" ionos.rf | awk -F: '{ print $2}'` )
      set ir = ( `grep "delta phase" ionos.rf | awk -F: '{ print $2}'` )
      if ($#ha == 0 || $#ha != $#fr || $#ha != $#ir) then
         echo "Error in information in ionos.rf..."
      else
          if (-e $out.FAR) then
            'rm' -f $out.FAR
         endif
         if (-e $out.IREF) then
            'rm' -f $out.IREF
         endif
 
         @ ii = 0
         while ( $ii < $#ha )
           echo $ha[$ii] $fr[$ii] >>$out.FAR
           echo $ha[$ii] $ir[$ii] >>$out.IREF
           @ ii = $ii + 1
         end
      endif
 endif

         echo "Input for NCALIB is in $out.FAR and $out.IREF"

         echo ""
         echo "Running NCALIB to store the corrections (FAR, IREF)."
         echo ""
         echo "Give input SCN-file, sectors and hour-angle range when asked"
         echo "If you do not want ionospheric refraction, enter # the "
         echo "second time that NCALIB asks you for a SCN-file."
         dws ncalib.putfar /nomenu <<_EOD_
OPTION=SET;SET
SET_OPTION=FAR;IREF
FARADAY_FILE=$out.FAR
IREFRACT_FILE=$out.IREF
#
_EOD_

         dwe ncalib.putfar
           
         'rm' -i ionos.rf $out.* f2*.01
      #endif

   endif
#
#
# red/ack will send a reply to the WSRT logbook
#
else if ("$1" == "red" || "$1" == "ack") then
   if ("$2" == "") then
      echo -n "Enter quality mark or comment: "      
      set Comment=($<)
   else
      set Comment=($2)
   endif
   set Comment=`echo $Comment | tr ',' ';' `
   set File=""

   if ("$Head" !~ *${USER}*) then
      echo "Only user ${Head} can confirm observations"
      set Cat=CMT
   else if ("$1" == "red") then
      set Cat=RED
   else 
      set Cat=ACK
   endif

   if ("$Cat" == "RED") then # || "$Cat" == "CMT") then
      if ("$3" == "") then
        echo -n "Enter a single sequence number to confirm or comment: "
        set Seq=($<)
      else
        set Seq=($3)
      endif
      echo -n "Enter name of associated file [none]: "
      set File=($<)

   else if ("$Cat" == "ACK") then
      if ("$3" == "") then
        echo "You may enter a comma-separated list or a range (first - last)"
        echo -n "Enter sequence number(s) to confirm: "
        set Seq=($<)
      else
        set Seq=($argv[3-])
      endif
      set Seq=(`echo $Seq | tr ',' ' '`)
      if ("$Seq" =~ *-*) then
         set tmp=`echo $Seq | tr '-' ' '`
         set Seq=""
         set i = $tmp[1]
         while ($i <= $tmp[2])
           set Seq=($Seq $i)
           @ i = $i + 1
         end 
      endif
   endif

   if ("$Cat" == "RED" || "$Cat" == "ACK") then
    if ("$File" == "") then
      foreach i ( $Seq )
        $n_exe/scissor.exe SELECT=OBSERVATION SEQNUMBER=$i >& /dev/null
        if ($status == 0) then
          $n_exe/scissor.exe PUT=OBSLOG SEQNUMBER=$i NAME=$USER CATEGORY=$Cat DESCRIPTION="$Comment"
        else 
          echo "$i does not exist"
        endif
      end
    else if (-e "$File") then
      set Note=${Seq}_`date +%y%m%d_%H%M%S`.$File:e
      ftp -n ftp.astron.nl <<_EOD_
user anonymous $USER
cd /pub/incoming
put $File $Note
quit
_EOD_
        $n_exe/scissor.exe SELECT=OBSERVATION SEQNUMBER=$Seq >& /dev/null
        if ($status == 0) then
          $n_exe/scissor.exe PUT=OBSLOG SEQNUMBER=$Seq NAME=$USER CATEGORY=$Cat DESCRIPTION="$Comment" URL=$Note
        endif
    else
      echo "File $File does not exist, no action taken"
    endif
   endif
#
#
# filpo adds a baseline package to Scissor
#
else if ("$1" == "filpo") then

   set Filpo="$2"
   echo -n "Enter location of filpo file [$Filpo]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Filpo="$Tmp"

   set Index=(`grep Baseline $Filpo | tail -1 | awk '{print $7}' `)
   if ("$3" != "") set Index="$3"
   echo -n "Enter index of package to store [$Index]: "
   set Tmp=($<)
   if ("$Tmp" != "") set Index="$Tmp"
  
   if ("$n_arch" == "hp") then
     awk -v Req=$Index -f $n_src/sys/filpo.kwa $Filpo | $n_exe/scissor.exe
   else
     awk -f $n_src/sys/filpo.kwa Req=$Index    $Filpo | $n_exe/scissor.exe
   endif

#
#
# calendar adds events to the calendar
#
else if ("$1" == "calendar") then
  if ("$TelAstr" !~ *${USER}*) then
     echo "Only users ${TelAstr} can add events to the calendar"
  else
     echo "----------------------------------------------"
     echo "---- Add events to the telescope calendar ----"
     echo "----------------------------------------------"
     echo ""
     echo "Enter dates as dd/mm/yyyy, year or year and month may be left out."
     echo "Stop date may be given after start date: dd/mm/yyyy,dd/mm/yyyy"
     echo "(year or year and month may be left out again)."
     echo "Enter Stop as start date to quit entering events."
     echo ""

     set Type="reduce"
     set Stop="stop"
     set Day = `date +%d`
     set Mon = `date +%m`
     set Yr  = 19`date +%y`

     cal_loop:
       echo -n "Enter start date (dd/mm/yyyy) [${Stop}]: "
       set Start=($<)
       if ("$Start" == "" ) set Start=$Stop
       if ("$Start" =~ [Ss]*) goto cal_done

       if ("$Start" =~ *,*) then
          set Stop =(`echo $Start | awk -F, '{print $2}' `)
          set Start=(`echo $Start | awk -F, '{print $1}' `)
       else
          set Stop=""
       endif

       set Tmp=(`echo $Start | tr '/' ' ' | tr '-' ' ' ` "" "")
       if ($Tmp[1] != "") set Day=$Tmp[1]
       if ($Tmp[2] != "") set Mon=$Tmp[2]
       if ($Tmp[3] != "") set Yr=$Tmp[3]
       if ($Yr < 100)     @ Yr = 1900 + $Yr
       set Start=(`echo $Day $Mon $Yr | awk '{printf("%2.2d/%2.2d/%4.4d",$1,$2,$3)}' `)

       if ("$Stop" == "") then
          echo -n "Enter stop  date (dd/mm/yyyy) [${Start}]: "
          set Stop=($<)
          if ("$Stop" == "") set Stop=$Start
       endif

       set Tmp=(`echo $Stop | tr '-' ' ' | tr '/' ' ' ` "" "" )
       if ($Tmp[1] != "") set Day=$Tmp[1]
       if ($Tmp[2] != "") set Mon=$Tmp[2]
       if ($Tmp[3] != "") set Yr=$Tmp[3]
       if ($Yr < 100)     @ Yr = 1900 + $Yr
       set Stop=(`echo $Day $Mon $Yr | awk '{printf("%2.2d/%2.2d/%4.4d",$1,$2,$3)}' `)

       set Done=0
       while (! $Done) 
         echo -n "Enter person to inform (Scissor name) [${Type}]: "
         set Tmp=($<)
         if ("$Tmp" != "") set Type=$Tmp
         echo -n "Email: `$n_exe/scissor.exe EMAIL=personal NAME=$Type` OK? [y] "
         set Tmp=($<)
         if ("$Tmp" == "" || "$Tmp" =~ [Yy]*) set Done=1
       end

       echo -n "Enter comment [stop]: "
       set Comment=($<)
       if ("$Comment" == "") goto cal_done
       echo -n "Telescopes involved [0..D]: "
       set Tmp=($<)
       set Comment=($Comment RT=$Tmp)

       echo "$Start - $Stop : $Comment ($Type)"
       $n_exe/scissor.exe put=telcal type=$Type start=$Start stop=$Stop comment=$Comment
     goto cal_loop

     cal_done:
     echo -n "Issue a new calendar (y,n) [n]? "
     set Tmp=($<)
     if ("$Tmp" =~ [Yy]*) then
        echo -n "Enter start date (dd/mm/yyyy): "
        set Start=($<)
        echo -n "Enter stop date  (dd/mm/yyyy): "
        set Stop=($<)
        echo -n "Enter identification (e.g. 1995.1): "
        set Note=($<)
        echo -n "Enter name of file with notes: "
        set File=($<)
        ftp -n ftp.astron.nl <<_EOD_
user anonymous $USER
cd /pub/incoming
put $File $Note
quit
_EOD_
        $n_exe/scissor.exe put=telcalh start=$Start stop=$Stop note=$Note
     endif

  endif

#
#
# oldtab will retrieve old tables from backup tape
#
else if ("$1" == "oldtab") then

cat <<_EOD_
Procedure for use of old reduction-tables with Newstar
------------------------------------------------------

All old reduction tables and an index are stored on a single DAT-tape.
To use these data, proceed as follows.

1e. Make sure Newstar is initialised
2e. Put the DAT-tape in the unit refered to by MAG8
3e. Enter "scissor oldtab <Calcode> <tableno>" to retrieve the 
    appropriate calibration file from the tape. The program wsrttab 
    will be retrieved as well. To list a particular table in the file,
    enter "wsrttab.x$n_arch <Calcode> <tableno>"

4e. Enter "scissor oldtab <volume> <label> <tableno>" to get the 
    calibration file for that particular observation. This only works 
    for calibration-groups before and including A267/B267, which have 
    been written to tape with corrections applied to them. The program
    wsrttab will be retrieved as well. To list a particular table in the
    file, enter "wsrttab.x$n_arch <Calcode> <tableno>" Use the Newstar 
    program NCALIB, option SET to remove the corrections from a SCN file.

The program wsrttab is not a regular Newstar application. It is maintained
under the Newstar Export Master (on ftp.astron.nl) and compiled versions for
Sun and HP workstations are stored on the backup tape. 

The backup tape contains:

   1e. Source code and executables for wsrttab
   2e. Index-file to search the calibration file for a particular label
   3e. All old reduction tables (bigfil.tab and <calcode>.tab)


The following calibration files give problems:

A205G  -  Cannot be read 
A234D  -  Cannot be read
A210H  -  600000 tables cannot be read
A211V  -  900000 tables cannot be read
A229F0 -  900000 tables cannot be read


Henk Vosmeijer & Marco de Vos, 950104.
_EOD_

#>>>>>> Start inclusion of /home/rzmws0/hjv/c/Findtab.csh >>>>>>
#! /bin/csh -f
#
# Script to extract a .tab-file from a backup DAT-tape and
# to print the contents of a table.
#
# Revision:
#	950204 HjV	Created
#
#
# First test the arguments:
# 2 arguments: calcode table-type
# 3 arguments: volume label table-type
#
if ($#argv < 2 || $#argv > 3) then
  echo "You should give:"
  echo "   2 arguments:   cal-code table-type"
  echo "   3 arguments:   volume   label   table-type"
  echo "	table-type should be a value between 0 and 9"
  exit
endif
if ($#argv == 3) then
  if ("$argv[1]" !~ [Dd][Aa]*) then
    echo "You specified 3 arguments: Volumes should start with da or DA"
    exit
  endif
  set volume=$argv[1]
  set label=$argv[2]
  @ ttype=$argv[3]
else	# 2 arguments
#	  test if not mentioned three, but forgot one
  if ("$argv[1]" =~ [Dd][Aa]*) then
    echo "You specified a volume (which start with da or DA), but"
    echo "you did not specify the third argument"
    exit
  endif
  set calgrp=$argv[1]
  @ ttype=$argv[2]
endif
#
# Test if Findtab.lis already exist in current directory
if (! -e Findtab.lis ) then
   echo "Extracting Findtab.lis from MAG8 ($MAG8) ... "
   echo "	This will take some time (about 25 minutes), so"
   echo "	you better gone do something more usefull yet"
   tar xvf $MAG8 Findtab.lis
endif
#
# Search for volume/label combination
if ($#argv == 3) then
  set Match=(`grep -i ${volume} Findtab.lis | grep -i "\, ${label}" | tr ',' ' ' `)
@ nrMatch = $#Match / 12
  if ($nrMatch == 0) then
     echo "Volume/label combination not found"
     exit
  endif
#
  if ($nrMatch == 1) then
     @ jj = 3
  endif
#
  if ($nrMatch > 1) then
    echo "Volume/label combination more than once found:"
    echo -n "Reduction codes found:"
    @ ii = 0
    while ( $ii < $nrMatch )
	@ jj = $ii * 12 + 3
	echo -n "     $Match[$jj]"
	@ ii = $ii + 1
    end
    echo " "
    @ ii = 0
    while ( $ii < $nrMatch )
	@ jj = $ii * 12 + 3
	echo -n "Use $Match[$jj]? (y/n) [Y} "
	set Flag=($<)
	if ($Flag =~ [Nn]* ) then
	  @ ii = $ii + 1
	else
	  @ ii = $nrMatch
	endif
    end
  endif
  set calgrp=$Match[$jj]
endif
#
#Test if tab-file and wsrttab-executable already exist in current directory
if (! -e $calgrp.tab || ! -e wsrttab.x$n_arch ) then
   echo "Extracting $calgrp.tab and wsrttab.x$n_arch from MAG8 ($MAG8) ... "
   echo "	This will take some time (about 25 minutes), so"
   echo "	you better gone do something more usefull yet"
   tar xvf $MAG8 $calgrp.tab wsrttab.x$n_arch
endif
@ xx = $ttype + $jj
wsrttab.x$n_arch $calgrp $Match[$xx]
#<<<<<< End inclusion of /home/rzmws0/hjv/c/Findtab.csh   <<<<<<

else
   echo "Invalid command $1 - for help, enter: scissor help"
endif  # Commands

#
# If ctrl_c was pressed, we get here to remove any remaining temp-files
#
abort_exit:

echo ""   
if (-e $Tmpfile) rm -f $Tmpfile
