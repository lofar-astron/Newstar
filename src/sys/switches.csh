#+
# Switches.csh
#   CMV 930526
#
#   CMV 930609	Changed to set= only, buffer switches in SW_key/SW_val
#   CMV 931020	Decode options from Command
#   CMV 931102	Switch -[N]Check added for dependencies
#   CMV 931201  Patch for GNU tr on Decstation at ruu
#   CMV 940216  Add switch -Confirm for clear
#   CMV 941006	Strange problem with tr on ruu, split in two calls
#   CMV 941102	Add -Import (retrieve) and -Merge (build) switches
#   CMV 950116  Add -Keep option for keeping old executables and ppd-files
#
# Decode the options listed in $Command or $Options into the various
# environment variables. Command is checked only if Options is empty.
#
# Switches appearing in groupfiles override the default ones. The
# defaults are temporarily saved in Save_switch. They can be restored
# by giving: set $Save_switch (and optionally: unset Save_switch).
#
# This script is sourced by the maintenance routines.
#
#-

#+
#..Default settings......Switch....Default...Action if set true...........
#
if (! $?_Alternate) set _Alternate =  0    # Link with alternate logging
#       _Binary                            # Flags binary files for ftp
if (! $?_Debug)     set _Debug     =  1    # Compile with debugging
if (! $?_Confirm)   set _Confirm   =  1    # Confirm deletions
if (! $?_Check)     set _Check     =  1    # Check dependencies
if (! $?_Echo)      set _Echo      =  0    # Set echo on 
if (! $?_Errors)    set _Errors    =  1    # Display error logs if any
#       _Help                              # Give list of valid options
if (! $?_Ignore)    set _Ignore    =  1    # Ignore softlinks 
if (! $?_Import)    set _Import    =  0    # Retrieve files from remote import
if (! $?_Keep)      set _Keep      =  1    # Default keep old exe-files
if (! $?_List)      set _List      =  0    # Make listfiles for compiles
if (! $?_Merge)     set _Merge     =  1    # Merge built files in source tree
if (! $?_Objectlib) set _Objectlib = ""    # Library for object code
if (! $?_Optimise)  set _Optimise  =  1    # Compile with optimisation
if (! $?_Print)     set _Print     =  0    # Print any listfile made
if (! $?_Select)    set _Select    =  1    # Select file for compilation
#       _Retrieve                          # Select file for ftp retrieval
if (! $?_Softlink)  set _Softlink  =  0    # Process softlinks
if (! $?_Textlib)   set _Textlib   =  ""   # Optional library for text files
if (! $?_Types)     set _Types     = "."   # List of file-types to select
if (! $?_Update)    set _Update    =  0    # Update files in n_exe
if (! $?_Xref)      set _Xref      =  0    # Include cross-ref in listings
#-

if (! $?Valid_options) then
   set Valid_options=( _Debug _Optimise _Alternate _List _Print _Errors \
                       _Xref  _Ignore _Import _Keep _Check _Echo        \
                       _Softlink  _Select  _Types _Update _Merge  \
                       _Textlib _Objectlib )
endif

set Save_switch=""

#
# If options is empty, scan Command and remove any options
#
if ($#Command == 0) set Command=""
if ("$Options[1]" == "" && "$Command[1]" != "") then
  set noglob
  @ ii = 1
  while ( $ii <= $#Command )
    if ("$Command[$ii]" =~ -* || "$Command[$ii]" =~ +* ) then
      set Options=( $Options $Command[$ii] ); 
      set Command[$ii]=""
    endif
    @ ii = $ii + 1
  end         
  set Command=( $Command )
  unset ii 
  unset noglob
endif

#
# Still no options, forget about it all
#
if ("$Options[1]" == "") exit

#
# Local options: temporarily save the original ones
#
if ( "$Options[1]" == "local" ) then
   if ( $#Options > 1 ) then
      shift Options
   else
      exit
   endif
   alias switch 'set Save_switch=($Save_switch \!:1 = $\!:1); set \!:1 = \!:2'
else
   alias switch 'set \!:1 = \!:2'
endif

#
# Patch for GNU tr in ruu (GNU has no way to handle the dash)
#
if (-e /usr/bin/tr ) alias tr /usr/bin/tr

set Options=( `echo $Options | tr -s '\-' ' ' | tr -s '+, ' '   '` )

foreach option ( $Options )

   if ("$option" =~ [Hh]*) then
      if ($option == $Options[1]) then
         cat <<'_EOD_'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  The syntax for options is generally: -Alternat,NList-Select
  A list of options should start with + or -, options can be
  separated by plus (+), minus (-) comma (,) or space ( ).
  The option name can be abbreviated to a single letter.
  If an option is to be negated (No List), it should be prefixed 
  with an N. Options are case insensitive.

  Select is a special case for use in groupfiles:
    special_file  -S:hp     # Compiled on hp only
  
  Types is a special case to limit the files to be selected by their
  type (extension), extensions are separated by slashes, the dot is 
  a wildcard, $ sign marks end of string (used to make exact match in
  stead of minimal match), starting with ^ causes that type to be ignored.

     shadow compile  -T:inc/pef @*.grp  # Compile include files only
     shadow compile  -T:f       @*.grp  # Compile all sorts of fortran files
     shadow compile  -T:c..     @*.grp  # Compile *.c?? but not *.c
     shadow checkout -T:^.              # Checkout nothing at all
     shadow checkout -T:.               # Checkout any type (default)

  You may also give -T:none, which is the same as -T:^. and can be useful
  if you want to move files in the master without compiling.

  If -H is given as the first switch, this information will be generated.
  If -H is given somewhere else, only the current setting will be shown.
#----------------------------------------------------------------------------#
'_EOD_'
      endif

      log "Current switch settings are: "
      foreach opt ( $Valid_options )
        log ` set | grep "^$opt" `
      end
      unset opt
      if ("$Save_switch" != "") then
         log " "
         log "Saved global settings: $Save_switch"
      endif
      log " "

   else if ("$option" =~ [Nn][Tt][Ee][Xx]*) then
      switch _Textlib ""
   else if ("$option" =~ [Tt][Ee][Xx]*) then
      set opt=`echo $option | tr ':' ' '`
      if ($#opt < 2 ) set opt=( x . )
      switch _Textlib $opt[2]

   else if ("$option" =~ [Nn][Oo][Bb][Jj]*) then
      switch _Objlib ""
   else if ("$option" =~ [Oo][Bb][Jj]*) then
      set opt=`echo $option | tr ':' ' '`
      if ($#opt < 2 ) set opt=( x . )
      switch _Objectlib $opt[2]

   else if ("$option" =~ [Bb]* || "$option" =~ [Nn][Bb]*) then
      # Ignore, only used for ftp 
   else if ("$option" =~ [Rr]* || "$option" =~ [Nn][Rr]*) then
      # Ignore, only used for ftp 

   else if ("$option" =~ [Dd]*) then
      switch _Debug     1
   else if ("$option" =~ [Nn][Dd]*) then
      switch _Debug     0

   else if ("$option" =~ [Cc][Oo]*) then
      switch _Confirm   1
   else if ("$option" =~ [Nn][Cc][Oo]*) then
      switch _Confirm   0
   else if ("$option" =~ [Cc]*) then
      switch _Check     1
   else if ("$option" =~ [Nn][Cc]*) then
      switch _Check     0

   else if ("$option" =~ [Oo]*) then
      switch _Optimise  1
   else if ("$option" =~ [Nn][Oo]*) then
      switch _Optimise  0

   else if ("$option" =~ [Aa]*) then
      switch _Alternate 1
   else if ("$option" =~ [Nn][Aa]*) then
      switch _Alternate 0

   else if ("$option" =~ [Kk]*) then
      switch _Keep      1
   else if ("$option" =~ [Nn][Kk]*) then
      switch _Keep      0

   else if ("$option" =~ [Ll]*) then
      switch _List      1
   else if ("$option" =~ [Nn][Ll]*) then
      switch _List      0

   else if ("$option" =~ [Mm]*) then
      switch _Merge     1
   else if ("$option" =~ [Nn][Mm]*) then
      switch _Merge     0

   else if ("$option" =~ [Pp]*) then
      switch _Print     1
   else if ("$option" =~ [Nn][Pp]*) then
      switch _Print     0

   else if ("$option" =~ [Ee][Rr]*) then
      switch _Errors    1
   else if ("$option" =~ [Nn][Ee][Rr]*) then
      switch _Errors    0

   else if ("$option" =~ [Ee][Cc]*) then
      switch _Echo      1
   else if ("$option" =~ [Nn][Ee][Cc]*) then
      switch _Echo      0

   else if ("$option" =~ [Pp]*) then
      switch _Print     1
   else if ("$option" =~ [Nn][Pp]*) then
      switch _Print     0

   else if ("$option" =~ [Xx]*) then
      switch _Xref      1
   else if ("$option" =~ [Nn][Xx]*) then
      switch _Xref      0

   else if ("$option" =~ [Ii][Gg]*) then
      switch _Ignore    1
   else if ("$option" =~ [Nn][Ii][Gg]*) then
      switch _Ignore    0

   else if ("$option" =~ [Ii][Mm]*) then
      switch _Import    1
   else if ("$option" =~ [Nn][Ii][Mm]*) then
      switch _Import    0

   else if ("$option" =~ [Ss]*:??) then
      if ("$option" =~ [Ss]*:$n_arch || "$option" =~ [Ss]*:un) then
         switch _Select 1
      else
         switch _Select 0
      endif

   else if ("$option" =~ [Ss]*) then
       switch _Softlink   1
   else if ("$option" =~ [Nn][Ss]*) then
       switch _Softlink   0

   else if ("$option" =~ [Tt]*:*) then
      set opt=`echo $option | tr ':' ' '`    # Split off argument
      if ("$opt[2]" =~ [Nn][Oo][Nn][Ee])  set opt[2]="^."
      switch _Types "$opt[2]"
      unset opt
   else if ("$option" =~ [Tt]*) then
      switch _Types "."

   else if ("$option" =~ [Uu]*) then
       switch _Update   1
   else if ("$option" =~ [Nn][Uu]*) then
       switch _Update   0

   else
      echo "Invalid or ambiguous switch chosen: $option"
   endif
end

#
# Handle echo here
#
if ($_Echo) then
   set echo
else
   unset echo
endif

unset Options option
unalias switch
