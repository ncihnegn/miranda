#! /home/dat/mira/states/src/miranda/mira -exec
|| Contributed by John Cupitt, University of Kent

||A while ago Steve Hill (I think) appealed for useful Miranda programs
||-- well, here is the greatest productivity aid for Miranda hackers
||since ball-point pens.  A 'vi' type filter to *rebox your comments*!!
||Amazing. It turns dull, unexciting notes like:

|| Given a node in the tree,
||examine the branches and chose the exit with the
|| highest score.

||into:

||----------------------------------------------------------------------||
|| Given a node in the tree, examine the branches and chose the exit 	||
|| with the highest score.						||
||----------------------------------------------------------------------||

||Any comments welcome -- my Miranda is not as hot as it could be ...

||John

||----------------------------------------------------------------------||
|| Box up Miranda comments. A filter from stdin to stdout
||	do_box :: [char] -> [char]					||
||		- Strip ||s, reformat, rebox.				||
||----------------------------------------------------------------------||

main = [Stdout (do_box $-)]

||----------------------------------------------------------------------||
|| Reboxing done in a pipeline of five stages.				||
||	- Split the input into lines					||
||	- Strip '||'s from input					||
||	- Lex the input, breaking into tokens				||
||	- Rejig tokens to produce fmted type output			||
||	- Output tokens as [char] with a box drawn around them		||
|| Formatting rules:							||
||	- Lines starting '||-' are deleted				||
||	- Leading & trailing '||' removed				||
||	- Lines starting with a tab are not reformatted			||
||	- Blank lines are 'new paragraph'				||
||----------------------------------------------------------------------||

||----------------------------------------------------------------------||
|| First a few types and useful little functions.			||
||----------------------------------------------------------------------||

|| Useful constants
outWid = 68			|| Width of the text in our boxes
boxWid = 72			|| Size of the box we draw

|| A token
tok ::=	Word [char] | Newpara | Line [char]

|| Useful character classifier
whitespace :: char -> bool
whitespace ch
      =	True, if ch = '\n' \/ ch = '\t' \/ ch = ' '
      =	False, otherwise

|| An edge of a box boxWid across
edge :: [char]
edge  =	"||" ++ (rep (boxWid-2) '-') ++ "||\n"

|| Find the length of a line containing tabs
len :: [char] -> num
len str
      = len' 0 str
	where
	len' n []
	      = n
	len' n (a:rest)
	      =	len' (n+tab_space) rest, if a = '\t'
	      =	len' (n+1) rest, otherwise
		where
		tab_space
		      =	8 - (n mod 8)

|| Useful when doing output --- only attach first param if its not [].
no_blank :: [char] -> [[char]] -> [[char]]
no_blank a b
      = a : b, if a ~= []
      = b, otherwise

||----------------------------------------------------------------------||
|| The main function. Call from a shell script in your /bin directory	||
|| looking something like:						||
||	#! /usr/bin/mira -exec						||
||	main = [Stdout (do_box $-)]					||
||	%include "../mira/box/box.m"					||
||----------------------------------------------------------------------||

do_box :: [char] -> [char]
do_box input
      =	edge ++ rejig input ++ edge
	where
	rejig =	re_box . format . lex_start . strip_start . split

||----------------------------------------------------------------------||
|| The first stage in processing. Split the input [char] into lines as	||
|| [[char]].								||
||----------------------------------------------------------------------||

|| Split the text into a list of lines
split :: [char] -> [[char]]
split input
      =	split' [] input
	where
	split' sofar (a:input)
	      =	sofar : split input, if a = '\n'
	      = split' (sofar ++ [a]) input, otherwise
	split' sofar []
	      = no_blank sofar []			|| No extra blank lines!

||----------------------------------------------------------------------||
|| The next stage ... strip old '||'s from the input. Remove:		||
||	- Lines starting '||-'						||
||	- Strip leading '||'s						||
||	- Strip trailing '||'s & trailing spaces			||
||----------------------------------------------------------------------||

|| At the start of a line:
strip_start :: [[char]] -> [[char]]
strip_start ([]:input)
      =	[] : strip_start input			|| Keep blank lines
strip_start (('|':'|':line):input)
      = strip_start' line
	where
	strip_start' ('-':rest)
	      = strip_start input		|| Strip '||---||' lines
	strip_start' rest
	      =	strip_rest rest input		|| Strip leading '||'
strip_start (line:input)
      = strip_rest line input			|| Pass rest through
strip_start []
      = []

|| Scan along the rest of the line looking for trailing '||'s to strip.
strip_rest :: [char] -> [[char]] -> [[char]]
strip_rest line input
      =	strip_rest' (rev line) input
	where
	strip_rest' ('|':'|':rest) input
	      =	strip_rest' rest input			|| Strip trailing ||
	strip_rest' (x:rest) input
	      = strip_rest' rest input, if whitespace x
	strip_rest' line input
	      =	(rev line) : strip_start input

|| Efficient(ish) reverse
rev list
      =	rev' [] list
	where
	rev' sofar (a:x)
	      =	rev' (a:sofar) x
	rev' sofar []
	      = sofar

||----------------------------------------------------------------------||
|| The next stage ... Break the input into Word, Newpara and Line	||
|| tokens. Newpara for blank lines and line starting with space; Line	||
|| for lines starting with a tab.					||
||----------------------------------------------------------------------||

|| At the start of a line.
lex_start :: [[char]] -> [tok]
lex_start ([]:input)
      = Newpara : lex_start input		|| Preserve blank lines
lex_start (('\t':rest):input)
      =	Line ('\t':rest) : lex_start input	|| Don't format tab lines
lex_start (line:input)
      = lex_rest (strip_ws line) input		|| Lex to eol
lex_start []
      =	[]

|| In the middle of a line. Try to take words off the front of what we
|| have so far. 
lex_rest :: [char] -> [[char]] -> [tok]
lex_rest [] input
      =	lex_start input
lex_rest sofar input
      =	Word wd : lex_rest (strip_ws rest) input
	where
	(wd, rest)
	      =	break_word sofar

|| Strip ws from the start of the line
strip_ws (a:input)
      =	(a:input), if ~whitespace a
      = strip_ws input, otherwise
strip_ws []
      =	[]

|| Break the word from the front of a line of text. Return the remains
|| of the line along with the word.
break_word :: [char] -> ([char], [char])
break_word (a:line)
      =	([a] ++ rest, tag), if ~whitespace a
      =	([], (a:line)), otherwise
	where
	(rest, tag)
	      =	break_word line
break_word []
      =	([],[])

||----------------------------------------------------------------------||
|| Almost the last stage ... Turn [tok] back into [[char]]. Format	||
|| onto outWid character lines.						||
||----------------------------------------------------------------------||

format :: [tok] -> [[char]]
format input
      =	format' [] input
	where
	format' sofar (Word wd:rest)
	      =	format' (sofar ++ " " ++ wd) rest, if #sofar + #wd < outWid
	      = sofar : format' (" " ++ wd) rest, otherwise
	format' sofar (Newpara:rest)
	      = no_blank sofar ([] : format rest)
	format' sofar (Line line:rest)
	      = no_blank sofar (line : format rest)
	format' sofar []
	      = no_blank sofar []

||----------------------------------------------------------------------||
|| The final stage. Box up a list of formatted lines. Try to be clever	||
|| about using tabs on the ends of lines.				||
||----------------------------------------------------------------------||

|| Draw a box boxWid across.
re_box :: [[char]] -> [char]
re_box (line:rest)
      =	"||" ++ line ++ padding ++ "||\n" ++ (re_box rest)
	where
	padding
	      =	rep n_tab '\t'
	n_tab
	      =	(boxWid - line_length + 7) div 8
	line_length
	      =	len ("||" ++ line)
re_box []
      =[]
