#! /bin/sh
# menu driver - Copyright Research Software Ltd 1985, 2006
# this version modified to ignore execute permissions of files

# see below for explanation of these variables, which we
# set to defaults if not present in the user's environment

test "$MENUVIEWER" || MENUVIEWER=cat      #ALTERNATIVE: more
if test ! "$VIEWER" 
then 
# choose one of these by removing the '#' in column one
     VIEWER='less -EX'; RETURNTOMENU=NO 
#    VIEWER=less, RETURNTOMENU=YES
#    VIEWER='more -d'; RETURNTOMENU=NO
fi

if test -z "`echo -n`" #use flag -n to suppress newline, if working
then sansnl='-n'
fi

cd "$1"
top="`pwd`"
while test -f contents
do
   if test '' = "$n"
   then echo [H[J #clear
        test ."$invalid" = . || echo invalid option "$invalid"
        invalid=""
        $MENUVIEWER contents;
        echo $sansnl ::please type selection number \(or q to quit\):
        read n
   fi
   if test '.' = "$n"  -a "$last"
   then n=$last
   elif test '+' = "$n" -a "$last" -a "$last" != .
   then n=`expr $last + 1`
   elif test '-' = "$n" -a "$last" -a "$last" != .
   then n=`expr $last - 1`
   fi
   if test '' != "$n"
   then if test -d "$n"
        then if test -f "$n/contents"
             then cd "$n"; oldlasts=$n:$oldlasts; last=".";
	     else invalid="$n"; fi
	     n=""
	elif test -f "$n"
        then if test '99' = "$n"  #special case, 99 is an executable
	     then echo [H[J; "./$n"
	     else echo [H[J #clear
                  $VIEWER "$n"
                  if test "$RETURNTOMENU" = YES
		  then last=$n; n=""; continue #next iteration of while-loop
                  fi
             fi
             echo $sansnl ::next selection \(return to go back to menu, q to quit\):
             last=$n; read n
        elif test ."$n" = .q -o ."$n" = ./q
        then exit
        elif test '???' = "$n"    # ??? interrogates display settings
        then echo "    MENUVIEWER='$MENUVIEWER'"
             echo "    VIEWER='$VIEWER', RETURNTOMENU='$RETURNTOMENU'"
             echo these can be changed by setting environment variables \
of the same names
             n=""
             echo $sansnl '[Hit return to continue]'
             read lose
        else case $n in
             !*) if test ".$n" = '.!!' -o ".$n" = '.!'
                 then if test "$lastbang" = ''
                      then invalid="$n"; n=""; continue
                      else echo !"$lastbang"
                      fi
                 else lastbang=`expr "$n" : '!\(.*\)'`
                 fi
                 $lastbang
                 echo $sansnl '[Hit return to continue]'
                 n=""
                 read lose ;;
             *) invalid="$n"; n="" ;;
             esac
        fi
   else test "$oldlasts" || exit  #we are at the root of the tree
        cd ..
        last=`expr $oldlasts : '\([^:]*\)'`
	oldlasts=`expr $oldlasts : '[^:]*:\(.*\)'`
   fi
done
exit
# Oct 2003 modified for Peter Bartke to overcome problem on UWIN
# no test -x, instead all files displayed except 99 which is executed
# May 2006 use echo $sansnl because echo -n not portable
  
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
