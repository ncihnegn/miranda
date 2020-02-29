||program for printing isomers of the alkanes -- D.A.Turner
||say `output' to run
mol ::= H | C[mol]  ||new type -- alkane molecules and radicals
c p = C (sort p)    ||place subcomponents in a standard order
molecules n = mkset [ mk_molecule x | x <- radicals n ]
mk_molecule(C p) = canonical_orientation (C(H:p))
radicals 0 = [H]
radicals n = (map rads [1..])!(n-1)  || make rads a memo function
rads n = mkset [ c[x,y,z] | i <- [0..(n-1)div 3]; j <- [i..(n-1-i)div 2];
                 x <- radicals i; y <- radicals j; z <- radicals(n-1-i-j) ]
canonical_orientation x = min (closure reorientations [x])
reorientations (C p) =  [ invert (p--[x]) x | x <- p; x ~= H ]
                        where
                        invert p (C q) = c (c p:q)
output = concat (map out [1..])
out n = title n ++ concat(map prettyprint (molecules n))
title n = "isomers of " ++ prefix!(n-1) ++ "ane\n"
prefix = ["meth","eth","prop","but","pent","hex","hept","oct","non",
	  "dec"] ++ [show i++"-"|i<-[11..]]

||below this line is concerned with drawing pictures of the molecules
%include "graphics.m"
prettyprint x = printpic1 (hd (molpics x)) ++ "\n"
molpics (C p) = compositions ([centre]:map f [1..# p] )
                where
                f i = map (reflect (direction!(i-1))) (subpics i)
                subpics i = [q1|q<-molpics (p!(i-1));
                                q1<-shift(translate 1 0 q),shift q1..]
molpics H = [pic 0 0 ["H"]]
direction = [1,-1,0,2]
shift p = translate 1 0 (overlaypic[bond,p])
bond = pic 0 0 ["-"]
centre = pic 0 0 ["C"]
