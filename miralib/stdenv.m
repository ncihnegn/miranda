> ||The Miranda Standard Environment  (C) Research Software Limited 1989

We give here, in alphabetical order, a  brief  explanation  of  all  the
identifiers  in  the  Miranda standard environment, each followed by its
definition  (except  in  a  few  cases  where  the   definition   cannot
conveniently  be  given  in  Miranda).   The  lines marked with a `>' in
column one are formal program text, the other  lines  in  the  file  are
comment.   Note  that  a  number of the functions given here are defined
internally (for speed) even though their  definitions  could  have  been
given  in  Miranda - in these cases the Miranda definition is given as a
comment.  This is the standard environment of Miranda release two.

Added October 2019 - showhex, showoct - see below.

`abs' takes the absolute value of a number - e.g. abs (-3) is 3, abs 3.5
is 3.5

> abs :: num->num
> abs x = -x, if x<0
>       =  x, otherwise

`and' applied to a list of truthvalues, takes their logical conjunction.

> and :: [bool]->bool
> and = foldr (&) True

`arctan' is the trigonometric function, inverse tangent.  It  returns  a
result in the range -pi/2 to pi/2.  See also `sin', `cos'.

> arctan :: num->num ||defined internally

`bool' is the type comprising the two truthvalues.

        bool ::= False | True ||primitive to Miranda

`char' is the type comprising the  Latin-1  character  set  (e.g.   'a',
'\n').

        char :: type  ||primitive to Miranda

`cjustify' applied to a number and a string, centre justifies the string
in  a  field  of  the specified width.  See also `ljustify', `rjustify',
`spaces'.

> cjustify :: num->[char]->[char]
> cjustify n s = spaces lmargin++s++spaces rmargin
>                where
>                margin = n - # s
>                lmargin = margin div 2
>                rmargin = margin - lmargin

`code' applied to a character returns its code number.  Example
        code 'a' = 97.
See also `decode'.

> code :: char->num  ||defined internally

`concat' applied to a list of lists, joins  them  all  together  into  a
single list with `++'.  E.g.
        concat [[1,2],[],[3,4]] = [1,2,3,4].

> concat :: [[*]]->[*]
> concat = foldr (++) []

`const' is a combinator for creating  constant-valued  functions.   E.g.
(const 3) is the function that always returns 3.

> const :: *->**->*
> const x y = x

`converse' is a combinator for inverting the order  of  arguments  of  a
two-argument function.

> converse :: (*->**->***)->**->*->***
> converse f a b = f b a

`cos' is the trigonometric cosine function, argument in radians.

> cos :: num->num   ||defined internally

`decode' applied to an integer returns the character with that code.

> decode :: num->char ||defined internally

`digit' is a predicate on characters.  True if the character is a digit.
See also `letter'.

> digit :: char->bool
> digit x = '0'<=x<='9'

`drop' applied to a number and a list returns the list  with  that  many
elements removed from the front.  If the list has less than the required
number of elements, `drop' returns [].  Example
        drop 2 [1,2,3,4] = [3,4]
See also `take'.

> drop :: num->[*]->[*]  ||defined internally, as below

  drop (n+1) (a:x) = drop n x
  drop n x = x,                                         if integer n
           = error "drop applied to fractional number", otherwise

`dropwhile' applied to a predicate and a list, removes elements from the
front of the list while the predicate is satisfied.  Example:
        dropwhile digit "123gone" = "gone"
See also `takewhile'.

> dropwhile :: (*->bool)->[*]->[*]
> dropwhile f [] = []
> dropwhile f (a:x) = dropwhile f x, if f a
>                   = a:x,           otherwise

`e' is a transcendental number, the base of natural logarithms.

> e :: num
> e = exp 1

`entier' when applied to a number returns its integer part, meaning  the
largest integer not exceeding it.  E.g.
        entier 1.0 = 1
        entier 3.5 = 3
        entier (-3.5) = -4.
Notice  that  for  Miranda  the  number  `1'  and  the  number `1.0' are
different values - for example they yield different  results  under  the
`integer'  test.   However  `1=1.0'  is  True,  because of the automatic
conversion from integer to float.

> entier :: num->num  ||defined internally

A useful fact about `entier', which relates it to the operators div  and
mod, is that the following law holds for any integers a, b with b~=0 and
a/b within the range for which integers can be  represented  exactly  as
fractional numbers
        a div b = entier (a/b)

`error' applied to a string creates an error value with  the  associated
message.   Error  values are all equivalent to the undefined value - any
attempt to access the value causes the program to  terminate  and  print
the string as a diagnostic.

> error :: [char]->*  ||defined internally

`exp' is the exponential function on real numbers.  See also `log'.

> exp :: num->num   ||defined internally

`filemode' applied to a string representing the pathname of a UNIX file,
returns  a  string  of  length four giving the access permissions of the
current process to the file.  The permissions are encoded  as  (in  this
order)  "drwx",  any  permission  not  granted  is  replaced  by  a  '-'
character.  If there is no file at pathname p, filemode  p  returns  the
empty string.  Example
        member (filemode f) 'w'
tests f for write permission.  See also `getenv', `read', `system'.

> filemode :: [char]->[char]  ||defined internally

`filestat'  applied  to  a  UNIX   pathname   returns   three   integers
((inode,device),mtime),  where  mtime  is  the time-last-modified of the
file, in seconds since 00.00h on 1 Jan 1970.   The  pair  (inode,device)
identifies a file uniquely, regardless of the pathname used to reach it.
A non-existent file has inode & device (0,-1) and mtime 0.

> filestat :: [char]->((num,num),num)  ||defined internally

`filter' applied to a predicate and a list, returns  a  list  containing
only those elements that satisfy the predicate.  Example
        filter (>5) [3,7,2,8,1,17] = [7,8,17]

> filter :: (*->bool)->[*]->[*]
> filter f x = [a | a<-x; f a]

`foldl' folds up a list, using a given binary operator and a given start
value, in a left associative way.  Example:
        foldl op r [a,b,c] = (((r $op a) $op b) $op c)
But  note  that  in order to run in constant space, foldl forces `op' to
evaluate  its  first  parameter.   See  the  definitions  of  `product',
`reverse', `sum' for examples of its use.  See also `foldr'.

> foldl :: (*->**->*)->*->[**]->*  ||defined internally, as below

  foldl op r [] = r
  foldl op r (a:x) = strict (foldl op) (op r a) x
                     where
		     strict f x = seq x (f x)

WARNING - this definition of foldl differs from that in  older  versions
of Miranda.  The one here is the same as that in Bird and Wadler (1988).
The old definition had the two args of `op' reversed.  That is:-
        old_foldl op r = new_foldl (converse op) r
the function `converse' has been added to the standard environment.

`foldl1' folds left over non-empty lists.  See the definitions of `max',
`min' for examples of its use.

> foldl1 :: (*->*->*)->[*]->*  ||defined internally, as below

  foldl1 op (a:x) = foldl op a x
  foldl1 op [] = error "foldl1 applied to []"

`foldr' folds up a list, using a given binary operator and a given start
value, in a right associative way.  Example:
        foldr op r [a,b,c] = a $op (b $op (c $op r))
See the definitions of `and', `concat', `or', for examples of its use.

> foldr :: (*->**->**)->**->[*]->** ||defined internally, as below

  foldr op r [] = r
  foldr op r (a:x) = op a (foldr op r x) 

`foldr1' folds right over non-empty lists.

> foldr1 :: (*->*->*)->[*]->*
> foldr1 op [a] = a
> foldr1 op (a:b:x) = op a (foldr1 op (b:x))
> foldr1 op [] = error "foldr1 applied to []"

`force' applied to any data structure, returns it, but  forces  a  check
that every part of the structure is defined.  Example
	hd(force x)
returns  the hd of x, but fully evaluates x first (so x must be finite).
See also `seq'.  Notice in particular the idiom `seq (force a) b'  which
returns `b' but only after fully evaluating `a'.

> force :: *->* ||defined internally

`fst' returns the first component of a pair.  See also `snd'.

> fst :: (*,**)->*
> fst (a,b) = a

`getenv' looks up a string in the user's UNIX environment.  Example
        getenv  "HOME"
returns the pathname of your home directory.  [If you want to  see  what
else is in your UNIX environment, say `printenv' as a UNIX command.]

> getenv :: [char]->[char]   ||defined internally

`hd' applied to a non empty list, returns its first element.  It  is  an
error to apply `hd' to the empty list, [].  See also `tl'.

> hd :: [*]->*
> hd (a:x) = a
> hd [] = error "hd []"

`hugenum' is the largest  fractional  number  that  can  exist  in  this
implementation (should be around 1e308 for IEEE standard 64 bit floating
point).  See also `tinynum'.

> hugenum :: num  ||defined internally

`id' is the identity function - applied to any object it returns it.

> id :: *->*
> id x = x

`index' applied to a (finite or infinite) list, returns a  list  of  its
legal  subscript values, in ascending order.  E.g.  index "hippopotamus"
is [0,1,2,3,4,5,6,7,8,9,10,11].

> index :: [*]->[num]
> index x = f 0 x
>           where
>           f n [] = []
>           f n (a:x) = n:f(n+1)x

`init' is dual to `tl', it returns a list without  its  last  component.
Example
        init [1,2,3,4] = [1,2,3].
See  also `last'.  [Note, by the `dual' of a list processing function we
mean the function which does the same job in a  world  where  all  lists
have been reversed.]

> init :: [*]->[*]
> init (a:x) = [],       if x=[]
>            = a:init x, otherwise
> init [] = error "init []"

`integer' is a predicate on numbers.  True if and only if the number  is
not fractional.

> integer :: num->bool    ||defined internally

`iterate' - iterate f x returns the infinite list [x, f x, f(f x), ... ]
Example:  iterate  (2*) 1 yields a list of the powers of 2.

> iterate :: (*->*)->*->[*]
> iterate f x = [y | y<-x, f y ..]

Note use of ", .." to generate an arbitrary sequence (see manual section
13/2).

`last' applied to a non empty  list  returns  its  last  element.   This
function is the dual of `hd'.  Note that for any non-empty list x
        (init x ++ [last x]) = x

> last :: [*]->*    ||defined internally, as below

  last x = x!(#x-1)

`lay' applied to a list of strings, joins them together after  appending
a newline character to each string.  Example
        lay ["hello","world"] = "hello\nworld\n"
Used to format output thus,
        lay(map show x)
as  a  top  level  expression,  causes  the elements of the list x to be
printed one per line.  See also `layn', `lines'.

> lay :: [[char]]->[char]
> lay [] = []
> lay (a:x) = a++"\n"++lay x

`layn' is similar to `lay', but produces output with numbered lines.

> layn :: [[char]]->[char]
> layn x =   f 1 x
>            where
>            f n [] = []
>            f n (a:x) = rjustify 4 (show n) ++") "++a++"\n"++f (n+1) x

'letter' is a predicate on characters.   True  if  the  character  is  a
letter.

> letter :: char->bool
> letter c = 'a'<=c<='z' \/ 'A'<=c<='Z'

`limit' applied to a list of values, returns the first  value  which  is
the  same  as  its  successor.   Useful in testing for convergence.  For
example the following Miranda expression computes the square root  of  2
by the Newton-Raphson method
        limit [x | x<-2, 0.5*(x + 2/x).. ]

> limit :: [*]->*
> limit (a:b:x) = a,           if a=b
>               = limit (b:x), otherwise
> limit other = error "incorrect use of limit"

`lines' applied to a list of characters containing newlines,  returns  a
list  of  lists,  by  breaking  the  original  into  lines.  The newline
characters are removed from the result.  Example, `lines' applied to
        "hello world\nit's me,\neric\n"
returns ["hello world","it's  me","eric"].   Note  that  `lines'  treats
newline  as  a  terminator, not a separator (although it will tolerate a
missing '\n' on the last line).

> lines :: [char]->[[char]]
> lines [] = []
> lines (a:x) = []:lines x,   if a='\n'
>             = (a:x1):xrest, otherwise
>               where 
>               (x1:xrest) = lines x, if x~=[]
>                          = []:[],   otherwise
>                            ||this handles missing '\n' on last line

Note that the inverse of `lines' is the function `lay', in that applying
`lay'  to the output of `lines' will restore the original string (except
that a final newline will be added, if missing in the original string).

`ljustify' applied to a number and a string, left justifies  the  string
in a field of the specified width.

> ljustify :: num->[char]->[char]
> ljustify n s = s++spaces(n - # s)

`log' applied to a number returns its natural logarithm (i.e.  logarithm
to the base `e').  It is the inverse of the exponential function, `exp'.
See also log10.  Note that the log functions use a  different  algorithm
when  applied to integer arguments (rather than just converting to float
first) so it is possible to take log, or log10, of very large integers.

> log :: num->num      ||defined internally

`log10' applied to a number returns its logarithm to the base 10.

> log10 :: num->num      ||defined internally

`map' applied to a function and a list returns a copy  of  the  list  in
which the given function has been applied to every element.

> map :: (*->**)->[*]->[**]
> map f x = [f a | a<-x]

`map2' is similar to `map', but takes a function of two  arguments,  and
maps  it  along two argument lists.  We could also define `map3', `map4'
etc., but they are much less often needed.

> map2 :: (*->**->***)->[*]->[**]->[***]
> map2 f x y = [f a b | (a,b)<-zip2 x y]

Note: the Bird and  Wadler  function  `zipwith'  is  just  an  uncurried
version of `map2', that is `zipwith f (x,y)' means `map2 f x y'.

`max' applied to a list returns the largest element under the  built  in
ordering of `>'.  Examples
        max [1,2,12,-6,5] = 12
        max "hippopotamus" = 'u'
See also `min', `sort'.

> max :: [*]->*
> max = foldl1 max2

`max2' applied to two values of the same type returns the  larger  under
the built in ordering of '>'.  See also `min2'.

> max2 :: *->*->*
> max2 a b = a, if a>=b
>          = b, otherwise

`member' applied to a list and a value returns  True  or  False  as  the
value is or not present in the list.

> member :: [*]->*->bool
> member x a = or (map (=a) x)

`merge' applied to two sorted lists merges  them  to  produce  a  single
sorted result.  Used to define `sort', see later.

> merge :: [*]->[*]->[*]  ||defined internally, as below

  merge [] y = y
  merge (a:x) [] = a:x
  merge (a:x) (b:y) = a:merge x (b:y), if a<=b
		    = b:merge (a:x) y, otherwise

`min' applied to a list returns its least member under `<'.

> min :: [*]->*
> min = foldl1 min2

`min2' applied to two values of the same type returns the smaller under
the built in ordering of '<'.

> min2 :: *->*->*
> min2 a b = b, if a>b
>          = a, otherwise

`mkset' applied to a list returns a copy of  the  list  from  which  any
duplicated  elements have been removed.  A list without duplications can
be used to represent a set, whence the name.   Works  even  on  infinite
list,  but  (beware)  takes  a  time quadratic in the number of elements
processed.

> mkset :: [*]->[*]
> mkset [] = []
> mkset (a:x) = a:filter (~=a) (mkset x)

`neg' is a function of one numeric argument, with the same action as the
unary `-' operator.

> neg :: num->num
> neg x = -x

`num' is the type comprising both integer and fractional  numbers  (such
as 42, -12.73e8).

        num :: type  ||primitive to Miranda

`numval' converts a numeric string to the corresponding number - accepts
optional leading "-" followed by integer or floating point number, using
same rules as the Miranda compiler.   Strings  containing  inappropriate
characters cause an error (exception - leading white space is harmless).

> numval :: [char]->num ||defined internally

`or' applied to a list of truthvalues, takes their logical disjunction.

> or :: [bool]->bool
> or = foldr (\/) False

`pi' is the well known real number (the ratio of the circumference of  a
circle to its diameter).

> pi :: num
> pi = 4*arctan 1

`postfix' takes an element and a list and adds the element to the end of
the list.  This is the dual of the prefix operator, `:'.

> postfix :: *->[*]->[*]
> postfix a x = x ++ [a]

`product' applied to list of numbers returns their  product.   See  also
`sum'.

> product :: [num]->num
> product = foldl (*) 1

`read' returns the contents of file with a given pathname.  Provides  an
interface  to  the  UNIX  filing  system.   If  the file is empty `read'
returns [], but if the file does not exist, or  lacks  read  permission,
`read' causes an error.  See also `filemode', `getenv'.

> read :: [char]->[char]  ||defined internally

`readb' reads a file as bytes - useful in a UTF-8 locale,  where  binary
data  may  contain  illegal characters if read as text.  (In a non UTF-8
locale the results of read and readb do not differ.) See manual  section
31/9 for more information.

> readb :: [char]->[char]  ||defined internally

_r_e_a_d_v_a_l_s is a family of functions for reading a list of  values  from  a
file.  See manual section 31/3.

`rep' applied to a number and a value, returns  a  list  containing  the
specified  number  of  instances  of  the value.  (The name is short for
`replicate'.)  Example
        rep 6 'o' = "oooooo"
See also `repeat'.

> rep :: num->*->[*]
> rep n x = take n (repeat x)

`repeat' applied to a value returns  an  infinite  list,  all  of  whose
elements are the given value.

> repeat :: *->[*]
> repeat x = xs
>            where xs = x:xs

`reverse' applied to any finite list returns a list of the same elements
in reverse order.

> reverse :: [*]->[*]
> reverse = foldl (converse(:)) []

`rjustify' applied to a number and a string, right justifies the  string
in a field of the specified width.

> rjustify :: num->[char]->[char]
> rjustify n s = spaces(n - # s)++s

`scan op r' applies `foldl op r' to every initial  segment  of  a  list.
For example `scan (+) 0 x' computes running sums.

> scan :: (*->**->*)->*->[**]->[*]
> scan op = g 
>           where
>           g r = (r:). rest
>                 where
>                 rest [] = []
>                 rest (a:x) = g (op r a) x

There is another way to explain `scan', which makes it clearer why it is
useful.    Let   s0   be   the   initial  state  of  an  automaton,  and
f::state->input->state, its state transition function - then `scan f s0'
is  a function that takes a list of inputs for the automaton and returns
the resulting list of states, starting with s0.

`seq' applied to two values, returns the  second  but  checks  that  the
first  value  is  not  completely  undefined.  Sometimes needed, e.g. to
ensure correct synchronisation in interactive programs.

> seq :: *->**->** ||defined internally

_s_h_o_w is a keyword denoting a family of functions for  converting  values
of  different  types to their print representations.  See manual section
23 for more details.

`shownum' applied to a number returns  as  a  string  a  standard  print
representation  for it.  A special case of the operator `show'.  Applied
to fractional numbers `shownum' gives 16 significant figures  (less  any
trailing  zeros), using a format appropriate to the size of number.  For
more detailed control over number format see `showfloat', `showscaled'.

> shownum :: num->[char] ||defined internally,

`showhex', `showoct' applied to an integer  return  its  hexadecimal  or
octal  representation  as a string.  Note that showhex will also convert
floating point numbers to hexdecimal format as per the C 2011  standard.
Example
	showhex pi => 0x1.921fb54442d18p+1
the scale factor in p is a power of 2 (oddly, this part is in decimal).

> showhex, showoct :: num->[char] ||defined internally

`showfloat p x' returns as a string the number  x  printed  in  floating
point  format,  that  is  in the form "digits.digits", where the integer
p (>=0) gives the number of digits after the decimal point.

> showfloat :: num->num->[char] ||defined internally,

`showscaled p x' returns as a string the number x printed in  scientific
format,  that  is  in  the  form "n.nnnnnne[+/-]nn", where the integer p
(>=0) gives the number of digits required after the decimal point.

> showscaled :: num->num->[char] ||defined internally,

`sin' is the trigonometric sine function, argument in radians.

> sin :: num->num  ||defined internally

`snd' returns the second component of a pair.

> snd :: (*,**)->**
> snd (a,b) = b

`sort' applied to any finite list sorts the elements of  the  list  into
ascending order on the built in '<' relation.  Note that you cannot sort
a list of functions.  Example
        sort "hippopotamus" = "ahimoopppstu"
The following definition uses merge-sort, which has n log  n  worst-case
behaviour.

> sort :: [*]->[*]
> sort x = x,                                         if n<=1
>	 = merge (sort(take n2 x)) (sort(drop n2 x)), otherwise
>	   where
>	   n = # x
>	   n2 = n div 2

`spaces' applied to a number returns a list of that many spaces.

> spaces :: num->[char]
> spaces n = rep n ' '

`sqrt' is the square root function on (integer or  fractional)  numbers.
The result is always fractional.

> sqrt :: num->num   ||defined internally

`subtract' is a name for (converse) infix  minus.   Needed  because  you
cannot  form  postsections  in  `-'.  (See manual page 9 on `sections'.)
Example
        subtract 3
is the function that subtracts 3.

> subtract :: num->num->num
> subtract x y = y - x 

`sum' applied to list of numbers returns their sum.

> sum :: [num]->num
> sum = foldl (+) 0

`sys_message' is an algebraic type containing a family  of  constructors
used to control output to UNIX files.  See manual section 31/2 on Output
to UNIX files.  The binary versions Stdoutb etc are used to write binary
data in a UTF-8 locale, see section 31/9 for more information.

> sys_message ::= Stdout [char] | Stderr [char] | Tofile [char] [char] |
>                 Closefile [char] | Appendfile [char] | System [char] |
>                 Exit num |  Stdoutb [char] |  Tofileb [char] [char]  |
>                 Appendfileb [char]

`system' applied to a string causes the string to be executed as a  UNIX
shell  command  (by `sh').  The result returned is a 3-tuple, comprising
the  standard_output,  error_output,   and   exit_status   respectively,
resulting  from  the  execution of the UNIX command.  See manual section
31/1 on Input from UNIX files etc for more details.

> system :: [char]->([char],[char],num)  ||defined internally

`take' applied to a number and a list returns the  specified  number  of
elements  from  the  front  of  the list.  If the list has less than the
required number of elements, `take' returns  as  many  as  it  can  get.
Examples
        take 2 [1,2,3,4] = [1,2]
        take 7 "girls" = "girls"

> take :: num->[*]->[*]  ||defined internally, as below

  take (n+1) (a:x) = a:take n x
  take n x = [],                                        if integer n
           = error "take applied to fractional number", otherwise

`takewhile' applied to a predicate and a list, takes elements  from  the
front of the list while the predicate is satisfied.  Example:
        takewhile digit "123gone" = "123"

> takewhile :: (*->bool)->[*]->[*]
> takewhile f [] = []
> takewhile f (a:x) = a:takewhile f x, if f a
>                   = [],              otherwise

`tinynum' is  the  smallest  positive  fractional  number  that  can  be
distinguished  from zero in this implementation (should be around 1e-324
for IEEE standard 64 bit floating point).

> tinynum :: num  ||defined internally

`tl' applied to a non empty list returns  the  list  without  its  first
element.  Example, tl "snow" is "now".

> tl :: [*]->[*]
> tl (a:x) = x
> tl [] = error "tl []"

`transpose' applied to a list of lists, returns their transpose (in  the
sense of matrix transpose - rows and columns are interchanged).  Example
        transpose [[1,2,3],[4,5,6]] = [[1,4],[2,5],[3,6]]
The  following definition is slightly more subtle than is at first sight
necessary, in order to deal correctly with `upper triangular'  matrices.
Example
        transpose [[1,2,3],[4,5],[6]] = [[1,4,6],[2,5],[3]]

> transpose :: [[*]]->[[*]]
> transpose x = [],                             if x'=[]
>             = map hd x':transpose(map tl x'), otherwise
>               where
>               x' = takewhile (~=[]) x

It might be thought that this function belongs in a specialised  library
of  matrix handling functions, but it has been found useful as a general
purpose list processing function, whence its inclusion in  the  standard
environment.

`undef' is a name for  the  completely  undefined  value.   Any  attempt
access  it  results  in  an error message.  Note that `undef' belongs to
every type.

> undef :: *
> undef = error "undefined"

`until' applied to a predicate, a function  and  a  value,  returns  the
result  of  applying  the  function  to the value the smallest number of
times necessary to satisfy the predicate.  Example
        until (>1000) (2*) 1 = 1024

> until :: (*->bool)->(*->*)->*->*
> until f g x = x,               if f x
>             = until f g (g x), otherwise

`zip2' applied to two lists returns a list of pairs, formed  by  tupling
together corresponding elements of the given lists.  Example
        zip2 [0..3] "type" = [(0,'t'),(1,'y'),(2,'p'),(3,'e')]
This  function is often useful in list comprehensions, where it provides
an idiom  for  traversing  two  lists  in  parallel.   For  example  the
following expression returns the scalar product of x and y (x,y::[num])
        sum [ a*b | (a,b) <- zip2 x y ]

> zip2 :: [*]->[**]->[(*,**)]  ||defined internally, as below

  zip2 (a:x) (b:y) = (a,b):zip2 x y
  zip2 x y = []

Note that if the lists being zipped are of different lengths, the length
of  the result is that of the shortest list (this holds for zip2 and all
the following zip functions).

The function `zip3' is analogous but takes three  lists  and  returns  a
list  of 3-tuples.  Similarly for `zip4', `zip5', `zip6' - zip functions
above zip6 are not provided in the standard environment.

> zip3 (a:x) (b:y) (c:z) = (a,b,c):zip3 x y z
> zip3 x y z = []
> zip4 (a:w) (b:x) (c:y) (d:z) = (a,b,c,d):zip4 w x y z
> zip4 w x y z = []
> zip5 (a:v) (b:w) (c:x) (d:y) (e:z) = (a,b,c,d,e):zip5 v w x y z
> zip5 v w x y z = []
> zip6 (a:u)(b:v)(c:w)(d:x)(e:y)(f:z) = (a,b,c,d,e,f):zip6 u v w x y z
> zip6 u v w x y z = []

The following is included for compatibility with Bird and Wadler (1988).
The normal Miranda style is to use the curried form `zip2'.

> zip :: ([*],[**])->[(*,**)]
> zip (x,y) = zip2 x y

End of definitions of the standard environment

