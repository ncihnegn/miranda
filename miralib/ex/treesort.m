|| Here is another sorting algorithm, this time treesort.
|| to try it out, say: 	treesort testdata

tree * ::= NILT | NODE * (tree *) (tree *)

treesort = flatten.build

build::[*]->tree *
build = foldr insert NILT

insert b NILT = NODE b NILT NILT
insert b (NODE a x y) = NODE a (insert b x) y, if b<=a
		      = NODE a x (insert b y), if b>a

flatten::tree *->[*]
flatten NILT = []
flatten (NODE a x y) = flatten x ++ [a] ++ flatten y

testdata = [1..5]++[20,19..16]++[6..10]++[15,14..11]

