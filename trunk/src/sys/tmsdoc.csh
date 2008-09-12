#!/bin/csh
#
#$Prog$
#
#  $Id$
#
#  $Purpose: Register an update of a TMS related document
#
#  $Usage:   tmsdoc.csh memo|new|update|get [File]
#
#  $Log$
#
#
#$/Prog$
#
#set echo

if ("$1" == "") then
   echo -n "Enter a command (memo, new, update, get): "
   set Command=($<)
else
   set Command=($1)
endif

if ("$Command" == memo) then

   if ("$2" =~ [0-9]*) then
      set Memo="Doc=$2"
      shift
   else
      set Memo=""
   endif

   if ("$2" == "") then
      echo -n "Enter name of file: "
      set File=($<)
   else
      set File=($1)
   endif
  
   echo -n "From: "
   set From=($<)
   echo -n "To:   "
   set To=($<)
   set To=(`echo $To | tr ',' ' '`)
   echo -n "Cc:   "
   set Cc=($<)
   set Cc=(`echo $Cc | tr ',' ' '`)

   echo -n "Subject: "
   set Subject=($<)
   echo -n "Action:  "
   set Action=($<)
   echo -n "Before:  "
   set Before=($<)

   if ("$File" != "") then
     ftp -n ftp.astron.nl <<_EOD_
user anonymous ${USER}@`domainname`
cd pub/incoming
bina
put $File $File:t
bye
_EOD_
      set Flag=(`$n_exe/scissor.exe put=tmsmemo $Memo subject=$Subject Action=$Action Before=$Before Name=$From Extension=$File:e File=$File:t`)
   else
      set Flag=(`$n_exe/scissor.exe put=tmsmemo $Memo subject=$Subject Action=$Action Before=$Before Name=$From`)
   endif

   echo $Flag
   set Memo=$Flag[2]
   foreach Name ($To) 
      $n_exe/scissor.exe put=tmsdocto doc=$Memo name=$Name Function=To
   end
   foreach Name ($Cc) 
      $n_exe/scissor.exe put=tmsdocto doc=$Memo name=$Name Function=Cc
   end

else if ("$Command" == new) then
   if ("$2" == "") then
      echo -n "Enter name of file(s): "
      set File=($<)
   else
      set File=($argv[2-])
   endif
  
   echo -n "Auhor: "
   set Author=($<)
   echo -n "Subject: "
   set Subject=($<)
   
   ftp -n ftp.astron.nl <<_EOD_
user anonymous ${USER}@`domainname`
cd pub/incoming
bina
prompt
mput $File
bye
_EOD_

   $n_exe/scissor.exe put=tmsdoc subject=$Subject Author=$Author File=$File
      
else if ("$Command" == update) then
   if ("$2" == "") then
      echo -n "Enter number of document: "
      set Doc=($<)
   else
      set Doc=($2)
   endif

   if ("$3" == "") then
      echo -n "Enter number of version: "
      set Version=($<)
   else
      set Version=($3)
   endif

   if ("$4" == "") then
      echo -n "Enter name of file(s): "
      set File=($<)
   else
      set File=($argv[4-])
   endif

   ftp -n ftp.astron.nl <<_EOD_
user anonymous ${USER}@`domainname`
cd pub/incoming
bina
prompt
mput $File
bye
_EOD_
   
   $n_exe/scissor.exe put=tmsdoc doc=$Doc File=$File

else if ("$Command" == get) then

   if ("$1" == "") then
      echo "Enter number of memo: "
      set Memo=($<)
   else
      set Memo=($1)
   endif
   if ("$Memo" < 100) set Memo="0$Memo"
   if ("$Memo" <  10) set Memo="0$Memo"

   setenv QEDDEBUG
   $n_exe/scissor.exe select=tmsmemo doc=$Memo 

   ftp -n ftp.astron.nl <<_EOD_
user anonymous ${USER}@`domainname`
cd tms/tmsdoc/memo
bina
prompt
mget tms$Memo.*
bye
_EOD_
   
endif

