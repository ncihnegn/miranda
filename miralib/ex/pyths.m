||Finds all pythagorean triangles (right triangles with  integer  sides)
||Note  the use of a diagonalising list comprehension, with `//' instead
||of `|'.  To see the triangles, say
||	output

output = lay (map show pyths)
pyths = [(a, b, intsqrt (a*a+b*b)) // a <- [3..]; b<-[a+1..]; is_sq (a*a+b*b)]
intsqrt x = entier (sqrt x)
is_sq y = (intsqrt y) ^ 2 = y
