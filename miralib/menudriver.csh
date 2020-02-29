#!/bin/csh -f
# menu driver - Copyright Research Software Ltd 1985, 2006
# this version modified to ignore execute permissions of files

# see below for explanation of these variables, which we
# set to defaults if not present in the user's environment

if(! $?MENUVIEWER) set MENUVIEWER = cat    #ALTERNATIVE: more
if(! $?VIEWER) then 
# choose one of these by removing the '#' in column one
     set VIEWER = 'less -EX'  RETURNTOMENU = NO 
#    set VIEWER = less  RETURNTOMENU = YES
#    set VIEWER = 'more -d'  RETURNTOMENU = NO
endif

set sansnl n invalid last oldlasts noglob
set histchars = '@^' #neutralize '!'

if("`echo -n`" == '') then #use flag -n to suppress newline, if working
set sansnl = '-n'
endif

if("$1" != '') cd "$1"
set top = "`pwd`"
while( -f contents )
   if("$n" == '') then
        clear
        if("$invalid" != '') echo invalid option "$invalid"
        set invalid = ''
        $MENUVIEWER contents;
        echo $sansnl ::please type selection number \(or q to quit\):
        set line = "$<"
        set n = `expr " $line" : ' *\([^ ]*\)'` 
   endif
   if("$n" == '.' && "$last" != '') then
        set n = $last;
   else if("$n" == '+' && "$last" != '' && "$last" != '.') then
        set n = `expr $last + 1`
   else if("$n" == '-' && "$last" != '' && "$last" != '.') then
        set n = `expr $last - 1`
   endif
   if("$n" != '') then
        if(-d "$n") then
             if(-f "$n/contents") then
                  cd "$n"; set oldlasts = "$n,$oldlasts"  last = "."
	     else set invalid = "$n"
             endif
	     set n = ''
	else if(-f "$n") then
             if("$n" == '99') then #special case, 99 is an executable
	          clear; "./$n"
	     else clear; 
                  $VIEWER "$n"
                  if("$RETURNTOMENU" == 'YES') then
		       set last = $n  n = ''
                       continue  #next iteration of while-loop
                  endif
             endif
             echo $sansnl ::next selection \(return to go back to menu, q to quit\):
             set last = $n  line = "$<"
             set n = `expr " $line" : ' *\([^ ]*\)'`
        else if("$n" == 'q' || "$n" == '/q') then
             exit
        else if("$n" == '???') then # ??? interrogates display settings
             echo "    MENUVIEWER='$MENUVIEWER'"
             echo "    VIEWER='$VIEWER', RETURNTOMENU='$RETURNTOMENU'"
             echo these can be changed by setting environment variables \
of the same names
             set n = ''
             echo $sansnl '[Hit return to continue]'
             set lose = "$<"
        else if( "$n" =~ !* ) then
             set line = `expr "$line" : ' *\(.*\)'`
             set line = `expr "$line" : '\(.*[^ ]\) *'`
             if( ".$line" == '.!!' || ".$line" == '.!' ) then
               if(! $?lastbang) then
                 set invalid = "$n"  n = ''; continue
                 else echo !"$lastbang"
               endif
             else set lastbang = `expr "$line" : '!\(.*\)'`
             endif
             $lastbang
             echo $sansnl '[Hit return to continue]'
             set n = ''
             set lose = "$<"
        else set invalid = "$n"  n = ''
        endif
   else if("$oldlasts" == '') exit  #we are at the root of the tree
        cd ..
        set last = `expr $oldlasts : '\([^,]*\)'`
	set oldlasts = `expr $oldlasts : '[^,]*,\(.*\)'`
   endif
end #of while-loop
exit
# Explanation of variables
# 
# MENUVIEWER is the program used to display contents pages
# 
# VIEWER is the program used to display individual sections
# 
# RETURNTOMENU=YES prevents another prompt being  given  after  displaying
# each  section, causing instead an immediate return to contents page.  It
# should be `YES' if VIEWER is a program that pauses for input at  end  of
# file, `NO' if VIEWER is a program that quits silently at end of file.
