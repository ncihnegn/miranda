||package for developing rectangular pictures composed of ascii characters
||DT Jan 84
||all pictures are conceived as lying in an infinite plane with origin at (0,0)
||and filled with blanks except where non-blank chars have been specified
query * ::= FAIL | SUCCEED *
||generic union type, often useful -- should probably be in library
picture ::= MKPIC (num,num)! [[char]] | EMPTYPIC
||MKPIC holds a figure with its north west corner at the given coordinates
frame :: picture->(num,num,num,num)
||returns (left,right,low,high) of smallest rectangle containing the figure
frame(MKPIC (x,y) a) = (x,x+#a!0-1,y-#a+1,y)
||it is an error to apply frame to the empty picture
printpic :: picture->[char]  ||prints pic with frame north west justified
printpic EMPTYPIC = []
printpic (MKPIC (x,y) a) = concat[p ++ "\n" | p <- a]
printpic1 :: picture->[char]  ||likewise, right shifted 8
printpic1 EMPTYPIC = []
printpic1 (MKPIC (x,y) a) = concat["        "++p ++ "\n" | p <- a]
alignpic :: num->num->picture->[char]
||prints picture as seen looking south east from the given cooordinates --
||only parts of the figure onside from this position appear of course
alignpic x y EMPTYPIC = []
alignpic x y (MKPIC (x1,y1) a)
    = newlines (y-y1) ++ concat a1, if y>y1
    = concat(drop (y1-y) a1), if y<y1
    = concat a1, otherwise
      where
      a1 = [drop (x-x1) p ++ "\n" | p <- a], if x>x1
         = [spaces (x1-x) ++ p ++ "\n" | p <- a], if x<x1
         = [p ++ "\n" | p <- a], otherwise
translate :: num->num->picture->picture ||move picture x right and y up
translate x y EMPTYPIC = EMPTYPIC
translate x y (MKPIC (x1,y1) a) = MKPIC (x+x1,y+y1) a
rotate :: num->picture->picture
||rotate the picture by n*pi/2 radians anticlockwise about (0,0)
rotate n EMPTYPIC = EMPTYPIC
rotate 0 = id
rotate 1 = reflect 3.reflect 2  ||result from group theory
rotate 2 (MKPIC (x,y) a) =
       MKPIC (-(x+#a!0-1),-(y-#a+1)) (reverse (map reverse a))
rotate 3 = reflect 2.reflect 3  ||group theory
rotate n = rotate (n mod 4)  ||other values of n
reflect :: num->picture->picture
||reflect about a line inclined at n*pi/4 to the x axis
reflect n EMPTYPIC = EMPTYPIC
reflect 0 (MKPIC (x,y) a) = MKPIC (x,-(y-#a+1)) (reverse a)
reflect 1 = reflect 3.rotate 2  ||group theory
reflect 2 (MKPIC (x,y) a) = MKPIC (-(x+#a!0-1),y) (map reverse a)
reflect 3 (MKPIC (x,y) a) = MKPIC (-y,-x) (transpose a')
                            where a' = map(map f)a
				  f '-' = '|'
				  f '|' = '-'
				  f etc = etc
reflect n = reflect (n mod 4)  ||other values of n
composepic :: [picture]->query picture
||tries to combine the given list of pictures to yield a composite picture
|| -- fails if any parts of the figures overlap
composepic = foldr apic (SUCCEED EMPTYPIC)
compositions :: [[picture]]->[picture]
||finds all possible ways of composing a picture (with no overlaps) with
||one component from each of the given picture lists
||this will probably be more useful in practice than composepic
compositions [] = [EMPTYPIC]
compositions (xx:etc) = f[apic1 x r // x <- xx; r <- compositions etc]
                        where
                        f [] = []
                        f (FAIL:x) = f x
                        f (SUCCEED a:x) = a: f x
overlaypic :: [picture]->picture
||similar to the above but allows pictures earlier in the given list to hide
||details of later ones, so the result is always a picture
overlaypic = foldr opic EMPTYPIC
apic :: picture->query picture->query picture  ||picture addition
p $apic SUCCEED EMPTYPIC = SUCCEED p
p $apic FAIL = FAIL
EMPTYPIC $apic q = q
MKPIC (x1,y1) a $apic SUCCEED (MKPIC (x2,y2) b)
        = FAIL, if xx=FAIL
        = SUCCEED (MKPIC (x,y) (f xx)), otherwise
          where
          x = min[x1,x2]
          y = max[y1,y2]
          xx = pointwiseadd a1 b1
          a1 = sidepad (x1-x) (-rjut) (toppad (y-y1) a)
          b1 = sidepad (x2-x) rjut (toppad (y-y2) b)
          rjut = x1+#a!0-x2-#b!0
          f(SUCCEED c) = c
apic1 :: picture->picture->query picture  ||picture addition mark2
p $apic1 EMPTYPIC = SUCCEED p
EMPTYPIC $apic1 q = SUCCEED q
MKPIC (x1,y1) a $apic1 MKPIC (x2,y2) b
        = FAIL, if xx=FAIL
        = SUCCEED (MKPIC (x,y) (f xx)), otherwise
          where
          x = min[x1,x2]
          y = max[y1,y2]
          xx = pointwiseadd a1 b1
          a1 = sidepad (x1-x) (-rjut) (toppad (y-y1) a)
          b1 = sidepad (x2-x) rjut (toppad (y-y2) b)
          rjut = x1+#a!0-x2-#b!0
          f(SUCCEED c) = c
opic :: picture->picture->picture  ||picture superposition
p $opic EMPTYPIC = p
EMPTYPIC $opic q = q
MKPIC (x1,y1) a $opic MKPIC (x2,y2) b
        = MKPIC (x,y) (pointwiseoverlay a1 b1)
          where
          x = min[x1,x2]
          y = max[y1,y2]
          a1 = sidepad (x1-x) (-rjut) (toppad (y-y1) a)
          b1 = sidepad (x2-x) rjut (toppad (y-y2) b)
          rjut = x1+#a!0-x2-#b!0
sidepad n r a = [spaces n ++ p ++ spaces r | p <- a]
toppad n a = f n
             where
             f n = a, if n<=0
                 = spaces (#a!0):f (n-1), otherwise
pointwiseoverlay :: [[char]]->[[char]]->[[char]]
pointwiseoverlay a b = f a b
                       where
                       f [] b = b
                       f a [] = a
                       f (p:a) (q:b) = g p q:f a b
                       g [] q = q
                       g p [] = p
                       g (c1:p) (c2:q) = c2:g p q, if c1=' '
                                       = c1:g p q, otherwise
pointwiseadd :: [[char]]->[[char]]->query [[char]]
pointwiseadd a b = SUCCEED c, if and [~member z clashchar | z<-c]
                 = FAIL, otherwise
                   where
                   c = f a b
                   f [] b = b
                   f a [] = a
                   f (p:a) (q:b) = g p q:f a b
                   g [] q = q
                   g p [] = p
                   g (c1:p) (c2:q) = c2:g p q, if c1=' '
                                   = c1:g p q, if c2=' '
                                   = clashchar:g p q, otherwise
clashchar = '\0'  ||assumed not to be present in any normal picture
pic :: num->num->[[char]]->picture
||takes a rectangular array of chars and turns it into a picture with its north
||west corner at the given x y position
pic x y a = EMPTYPIC, if and[p=[]|p<-a]
	    ||covers both a=[] and elements a all []
          = pic x (y-1) (tl a), if and[c=' ' | c<-hd a]
	    ||strip blank rows
          = pic (x+1) y (map tl a), if and[hd p=' ' | p <- a] 
	    ||strip blank cols
          = MKPIC (x,y) a, otherwise 
	  ||what about east and south trimming? -- fix later
||we have assumed given a rectangular and not containing control chars, we
||ought perhaps to check this when a picture is formed -- fix later

newlines n = rep n '\n'
closure :: (*->[*])->[*]->[*];
||takes the closure of a set under a pointwise function that returns
||increments to the set
closure f s = g s s
              where
              g r t = [], if t=[]
                    = t ++ g(r ++ t)(mkset[x|x<-concat(map f t);~member r x]),
		      otherwise
