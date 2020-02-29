> || _L_i_t_e_r_a_t_e_ _s_c_r_i_p_t_s_ _(_a_n_ _a_l_t_e_r_n_a_t_i_v_e_ _c_o_m_m_e_n_t_ _c_o_n_v_e_n_t_i_o_n_)

The standard comment convention for Miranda  scripts  is  that  anything
rightwards from a pair of vertical bars to the end of a line is taken to
be comment and ignored by the compiler, thus
	||This is a comment

Everything else in the script is taken to be formal  program  text.   An
inverted  style  of  commenting is also available in Miranda, permitting
the construction  of  a  "literate  script"  (the  name  is  taken  from
Professor Donald Knuth's idea of "literate programming").  In a literate
script EVERYTHING is assumed to be comment, except for lines marked with
the formalising symbol '>' in column 1.  For example the following lines

>  fac 0 = 1
>  fac (n+1) = (n+1)*fac n

would  be  taken  as  formal program text - and could be preceded and/or
followed by some narrative explaining what the factorial function is and
why we define it in this way.

To minimise the danger that you will accidentally omit the '>" from  one
line of your formal text without the compiler noticing that something is
wrong, the following additional rule applies to Miranda literate scripts
-  whenever  a  group  of  lines  of  formal program text is preceded or
followed by some lines of "narrative", the two types  of  text  must  be
separated  by  at least one blank line.  You will see that this has been
done for the definition of  factorial  given  above.   (Definition  -  a
"blank line" is one containing only white space.)

Within the formal sections of a literate  script  the  standard  comment
convention still works.  For example

>  result = sum [fac n | n <- [1..50]]  ||NB this is a large number!

The  compiler  takes  a  decision on which comment convention applies by
looking at the first line of a script.  If this has a '>' in  column  1,
then  it  is  a  literate script, otherwise the compiler assumes it is a
conventional script.  Typically the first line of a literate script will
just be a comment, eg

> ||This is a literate script

In fact this manual section is a legal Miranda  script,  defining  `fac'
and  `result'  (see  first line). 

An alternative convention is based on the name of the  file  -  if  this
ends   in  `.lit.m'  then  it  is  assumed  to  be  a  literate  script,
independently of the form of the first line.  This makes it possible  to
have literate scripts which begin in `narrative' mode.

As an aid to maintaining good layout in literate scripts, a simple  text
formatting  program, called `just' (short for justify), is supplied with
the Miranda system.  This leaves untouched the formal  sections  of  the
script  and formats the narrative parts to specified width (default 72).

There is a UNIX manual page  for  `just'  which  gives  details  of  its
behaviour.   Note  that `just' is a general purpose text formatting tool
and is not in any way Miranda-specific.

As an additional aid  to  the  use  of  document  preparation  tools  in
conjunction  with  Miranda  scripts, the Miranda compiler will recognise
underlined keywords.  This applies both to reserved words, such as `_d_i_v'
and  `_m_o_d'  and  to  directives  such  as  `_%_e_x_p_o_r_t' (underlining of the
initial  `%'  is  optional).   The  style  of  underlining  accepted  is
`backspace-underline-character'  as  generated  by nroff/troff.  It will
also recognise the underlined symbols _> and _< as being equivalent to >=,
<=  respectively.  This works in both literate scripts and scripts using
the standard comment convention.

_U_s_i_n_g_ _L_a_T_e_X_ _w_i_t_h_ _M_i_r_a_n_d_a_ _l_i_t_e_r_a_t_e_ _s_c_r_i_p_t_s
 Because of the `.lit.m' convention it is possible for a file to be both
a  Miranda  script and a LaTeX source file.  In such a case the sections
of formal Miranda text (recognised by the Miranda compiler by the `>' in
column 1) will be surrounded by the LaTeX commands
 \begin{verbatim}

 \end{verbatim}
 A similar arrangement can be made for troff.

[The 1989 distribution included a program, mtotex, for using  mira  with
LaTeX, but this no longer works and has been removed - DT]

_A_c_k_n_o_w_l_e_d_g_e_m_e_n_t_s
 The '>' inverse-comment convention (and the "blank line" rule) are  due
to  Richard  Bird  and  Philip  Wadler  of Oxford University Programming
Research Group, and were first used in  their  language  "Orwell".

