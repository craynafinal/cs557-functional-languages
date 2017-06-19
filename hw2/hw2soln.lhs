----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017                 Homework 2

Notes and Sample Solutions
----------------------------------------------------------------------

> import Data.List

----------------------------------------------------------------------
Exercises from the textbook:
----------------------------
Chapter 5 (P57-58): Questions 6 and 9
Chapter 6 (P72): Questions 7 and 8

Ch 5, Ex 6:
-----------
The factors function, defined on P.48 of the book, returns the list
of all factors of a number n, including 1 and n:

> factors  :: Int -> [Int]
> factors n = [ x | x <- [1..n], n `mod` x == 0 ]

A number is said to be "perfect" if it equals the sum of all its
factors, excluding the number itself.  In other words, a number i
is perfect if sum (init (factors i)) == i.  Thus the list of all
perfect numbers in the range 1..n can be calculated using:

> perfects  :: Int -> [Int]
> perfects n = [ i | i <- [1..n], sum (init (factors i)) == i ]

This definition replicates the behavior shown in the question:

  Main> perfects 500
  [6,28,496]
  Main>

The Wikipedia article on perfect numbers indicates that the next
perfect number is 8128, and our solution is also able to confirm
that (although the computation takes a lot longer in this case):

  Main> perfects 8200
  [6,28,496,8128]
  Main>


Ch 5, Ex 9:
-----------
We use the zip function make a list of pairs, each of which includes
one component from each of the two input lists.  We use a list
comprehension to compute the corresponding list of products, and
then add up all the results using sum:

> scalarproduct      :: [Int] -> [Int] -> Int
> scalarproduct xs ys = sum [ x*y | (x, y) <- zip xs ys ]

Once again, we can confirm that this gives us the same behavior
as shown in the question:

  Main> scalarproduct [1,2,3] [4,5,6]
  32
  Main>

It is a bit hard to come up with other interesting tests.  But the
following examples do show how we can pick out individual components
of one vector by taking scalar products with vectors that contain
a single 1 with zeros in all other positions:

  Main> scalarproduct [1,2,3] [1,0,0]
  1
  Main> scalarproduct [1,2,3] [0,1,0]
  2
  Main> scalarproduct [1,2,3] [0,0,1]
  3
  Main>

Incidentally, while the question was expecting you to use the zip
function as above, the pattern of using zip and then combining the
values in each pair is common enough that the prelude actually has
a special function for this called zipWith.  Using this function,
the definition above can be rewritten as:

> scalarproduct1      :: [Int] -> [Int] -> Int
> scalarproduct1 xs ys = sum (zipWith (*) xs ys)

Ch 6, Ex 7:
-----------
The best model that we have at this point in the book for defining
the merge function is the definition of zip at the top of Page 64.
Following that pattern:

> merge        :: Ord a => [a] -> [a] -> [a]
> merge []   ys = ys
> merge xs   [] = xs
> merge (x:xs) (y:ys)
>   | x<=y      = x : merge xs (y:ys)
>   | otherwise = y : merge (x:xs) ys

The first two equations here take care of situations where one of
the two inputs is empty, in which case the result of the merge is
just whatever list was passed in as the other argument.  The third
equation handles the case where we are merging two nonempty lists,
using guards to compare the initial values from each list to decide
which should appear first in the output list.

A quick test run demonstrates that this definition produces the
same behavior described in the question.

  Main> merge [2,5,6] [1,3,4]
  [1,2,3,4,5,6]
  Main>

We will test this function a little more thoroughly in the next
exercise as a component of our merge sort implementation.

Ch 6, Ex 8:
-----------
The question suggests that we define a functiont that splits an
input list in to two halves whose lengths differ by at most one.
If the input list is xs, then we can use take m xs and drop m xs
as the two output lists, where m = length xs `div` 2:

> halve   :: [a] -> ([a], [a])
> halve xs = (take m xs, drop m xs)
>            where m = length xs `div` 2

Some quick tests confirm that this function works correctly on
short lists, including some with even numbers of elements, and
some with odd (forcing, in the latter case, a situation where
one of the two "halves" has more elements than the other):

  Main> halve [1..9]
  ([1,2,3,4],[5,6,7,8,9])
  Main> halve [1..10]
  ([1,2,3,4,5],[6,7,8,9,10])
  Main> halve [1..11]
  ([1,2,3,4,5],[6,7,8,9,10,11])
  Main>

From this point, it is easy to build a complete implementation of
mergesort.  The first two equations in the definition below follow
the hints in the question that empty and singleton lists are already
sorted.  The final equation takes care of longer lists: use halve
to break the input in to two pieces; use recursive calls to sort
each of those pieces individually; and then use merge to recombine
them:

> msort    :: Ord a => [a] -> [a]
> msort []  = []     -- empty list already sorted
> msort [x] = [x]    -- singletons already sorted
> msort xs  = merge (msort ls) (msort rs)
>  where (ls, rs) = halve xs

We can test this on a few sample inputs: an already sorted list,
a reverse sorted list, and a "randomly chosen" unsorted list are
good candidates for this:

  Main> msort [1..10]
  [1,2,3,4,5,6,7,8,9,10]
  Main> msort [10,9..1]
  [1,2,3,4,5,6,7,8,9,10]
  Main> msort [5,8,2,4,3,10,1,9,7,6]
  [1,2,3,4,5,6,7,8,9,10]
  Main>

Beyond the scope of what was expected in student answers, we could
define a function to generate the list of all permutations of a given
list as follows:  (This will actually come up later in the book in
Section 9.4.)

> perms       :: [a] -> [[a]]
> perms []     = [[]]
> perms (x:xs) = concat (map (interleave x) (perms xs))

> interleave         :: a -> [a] -> [[a]]
> interleave x []     = [[x]]
> interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

For example:

  Main> perms [1,2,3]
  [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
  Main> length (perms [1..8])
  40320
  Main>

Now we can apply our sorting algorithm to all possible permutations
of [1,2,3] and confirm, by inspection, that we get the expected
result in each case:

  Main> map msort (perms [1,2,3])
  [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
  Main> 

Checking every element in a long list is a bit error prone; we might
easily skip over one element that is incorrect, for example.  One way
to handle this is to use the nub function from the Data.List library,
which removes all duplicate elements from an input list:  (In English,
"the nub" refers to "the essence or central point of a matter".)

  Main> nub (map msort (perms [1..6]))
  [[1,2,3,4,5,6]]
  Main> 

Now we can see that there is only one output list, and that it is
the correctly sorted list that we expect.  Or perhaps we should just
write a function to verify that the a given list is sorted (more good
practice for writing recursive definitions on lists):

> sorted         :: Ord a => [a] -> Bool
> sorted (x:y:ys) = (x<=y) && sorted (y:ys)
> sorted other    = True

And now we don't even have to look at any sorted outputs, just the
reassuring "True" in the output below, which confirms that our msort
implementation has correctly sorted all 40,320 of the test cases that
we just threw at it:

  Main> all sorted (map msort (perms [1..8]))
  True
  Main>

You can try larger numbers here if you want, but you will need to be
patient because the number of test cases grows very rapidly!  I'm
not sure anyone will have the patience to let the following example
finish (it requires 2,432,902,008,176,640,000 tests!):

  Main> all sorted (map msort (perms [1..20]))
  ^C{Interrupted!}

  Main>

On the other hand, the fact that it didn't print False before I hit
control C to abort the calculation does at least tell us that it had
yet to find any test cases for which msort failed, even though we do
not know how many tests had been tried at that point.

----------------------------------------------------------------------
Question 1:
-----------

We could just ask a Haskell interpreter for some help with this
question, but it's more informative to try figuring out some answers
on our own.  We might not always get the same answer as the
interpreter (which will always return the most general type that it
can find), but that's ok: the question only asks for a *possible*
type.

a) map odd

odd is a function of type Int -> Bool, so map odd will be a
corresponding function that maps lists of Int values to lists of
Bools.  In other words:  map odd :: [Int] -> [Bool]

b) takeWhile null

null is a function from lists to Bool; takeWhile p is a function
that will take an argument list and return the initial portion for
which p returns true.  So takeWhile null returns the initial
portion of a list of lists in which element is empty.  That is:
takeWhile null :: [[a]] -> [[a]]

c) (++[])

This is a section of the append, ++, operator that will add the
empty list on to the end of its argument.  As such, it can be
applied to any list and will return the same list as its result.
So:  (++[]) :: [a] -> [a]

d) (:[])

This is a section of the cons operator, :.  It will take a value x
as argument and return (x:[]) as a result, which is the same as [x].
So, if the argument is of type t, then the result will be a
(singleton) list of type [t].  Hence:  (:[]) :: t -> [t]

e) ([]:)

This is a section of the cons operator that adds an empty list on to
the front of its argument.  Because the empty list is (obviously) a
list, this implies that the argument must be a list of lists.  So:
([]:) :: [[a]] -> [[a]]

f) [ [], [[]], [[[]]], [[[[]]]], [[[[[    ]]]]] ]

If this example is valid at all, then it must be a list of some type
that includes each of the elements.  For the rightmost element,
[[[[[    ]]]]], there are five levels of nesting, and hence the
element type must be of the form [[[[[t]]]]] for some t.  All of the
other elements in the list are values of this type, and hence the
type of the full list must be:

  [[[[[[t]]]]]]   (six levels of nesting) for some type t

g) [ [], [[]], [[[]]], [[[[]]]], [[[[[True]]]]] ]

The same argument as in Part (f) applies here too except that the
last element this time is of type [[[[[Bool]]]]], so the whole list
is of type [[[[[[Bool]]]]]], again with six levels of nesting.

h) [ [True], [[]], [[[]]], [[[[]]]], [[[[[]]]]] ]

This example is ill-typed.  All elements of a list must have the
same type, but in the expression above, the first element has type
[Bool] while the second is [[a]] for some a.  Because [a] and Bool
can never be the same, regardless of what type we pick for a, this
cannot be a valid list in Haskell.

i) map map

map :: (a->b) -> ([a] -> [b]), so if we apply the map function to
the elements in a list of type [a -> b], then the result will be a
list of type [[a] -> [b]].  Hence:

  map map :: [a -> b] -> [[a] -> [b]]

j) map (map odd)

map odd :: [Integer] -> [Bool]       (see part a)
so
map (map odd) :: [[Integer]] -> [[Bool]]

k) map . map

if we apply this to a function f :: a -> b, then we will get
(map . map) f = map (map f), which, by a similar argument to
the one used in (j) above is of type [[a]] -> [[b]].  Thus:

map . map :: (a -> b) -> [[a]] -> [[b]]

l) (map , map)

Each occurrence of map in this pair can be used with a different
function type, so the most general possible type for this expression
is:

   (map, map)
     :: ((a->b) -> [a] -> [b], (c -> d) -> [c] -> [d])

(It's ok if you wrote down more specific types, or if you assumed
the same type for both components of the pair, but it's nice to know
that you can use different types for each of the different
occurrences if you want them ...)

m) [ map id, reverse, map not ]

  map id :: [a] -> [a]
  reverse :: [a] -> [a]
  map not :: [Bool] -> [Bool]

For these types to be the same (so that we can store all of them in
a single list), we need to set a = Bool, and then we have a list in
which all values are functions of type [Bool] -> [Bool].  Hence the
final answer is:

  [ map id, reverse, map not ] :: [[Bool] -> [Bool]]


Question 2:
-----------

a) odd . (1+)

returns True if and only if its argument is an *even* number.
(Because n+1 will be odd if, and only if n is even).  You won't get
credit for explaining what this function does if you just say that
it "returns True if (n+1) is odd."

b) odd . (2*)

returns False, regardless of the input number (because no multiple
of two is odd).  This function could also be written as \x -> False,
or as (const False) using the prelude function const.

c) ([1..]!!)

If the input argument is i, then this function returns the ith
element of [1..].  In other words, for i>=0, this function returns
the number (i+1).

d) (!!0)

Returns the first value in a non-empty list; it is equivalent to the
prelude function called "head".

e) reverse . reverse

Reversing a list once and then reversing it again will get us back
to the list that we started with.  In other words, this function is
the identity on (finite) lists.  That is, for any list input, it
returns the same list as a result.  This function doesn't actually
behave as an identity function for *infinite* lists, but we haven't
covered that in class, so I'm not expecting you to be aware of such
details here.

f) reverse . tail . reverse

This function returns the initial portion of a (finite) list (i.e.,
every element of the list except the very last one).  It is similar
to the prelude function called "init".

g) map reverse . reverse . map reverse
 
This is essentially equivalent to the reverse function, but only on
lists with type of the form [[a]].  The two uses of map reverse
ensure that we are working with lists of lists, but the effect of
the first map reverse is cancelled out by the effect of the second,
leaving only the reverse in the middle.  (This assumes, however,
that all of the lists in the input list are finite ...  but that's
another detail that you don't need to worry about yet.)


Question 3:
-----------
[NOTE: There are multiple ways to answer this question.  You may have
used different definitions for integers, pairs, and pos than the ones
described below.  For example, you may enumerate the elements in a
different order, or include some elements multiple times (the question
did not specify whether this was allowed, so we will not deduct points
for solutions that do include repeated elements).  I short, you are
not expected to have replicated the exact solutions shown here, just
to have provided reasonable definitions and to have demonstrated,
by appropriate testing, that they satisfy the properties specified
in the question.]

a) The problem with the original definition of integers is that it
does not include any negative numbers.  Even if you try to add the
negative numbers to the end of list (as in [0..] ++ [-1,-2..]), we will
never be able to get past the positive numbers to find the negatives.
Instead, we need to interleave the positive and negative numbers

> integers :: [Integer]
> integers  = 0 : [ n | i <- [1..], n <- [-i, i] ]

We can list the first few elements to check that everything looks
right:

  Main> take 10 integers
  [0,-1,1,-2,2,-3,3,-4,4,-5]
  Main>

And we can check to make sure the required property holds for some
specific positive and negative numbers, both small and (relatively)
large:

  Main> 12 `elem` integers
  True
  Main> 1000 `elem` integers
  True
  Main> (-1) `elem` integers
  True
  Main> (-10000) `elem` integers
  True
  Main>

Finally, we can test our solution for a full range of numbers using
a test like the following:

  Main> and [ i `elem` integers | i <- [-1000..1000] ]
  True
  Main>

All tests succeeding, it's time to move on to the next part of the
question!

b) The set of all possible pairs of nonnegative integers can be
pictured as follows:

    (0, 0)    (0, 1)    (0, 2)    (0, 3)    (0, 4)   ....
    (1, 0)    (1, 1)    (1, 2)    (1, 3)    (1, 4)   ....
    (2, 0)    (2, 1)    (2, 2)    (2, 3)    (2, 4)   ....
    (3, 0)    (3, 1)    (3, 2)    (3, 3)    (3, 4)   ....
    (4, 0)    (4, 1)    (4, 2)    (4, 3)    (4, 4)   ....
    ...       ...       ...       ...       ...

With the original list comprehension, we never get past any of the
numbers on the first row whose first coordinate is zero:

  Main> take 10 [ (n,m) | n <- [0..], m <- [0..] ]
  [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9)]
  Main

But we will run in to the same problem if we switch the order of the
arguments (for example, if we swap m and n, then we will only get
pairs in the first column above.

The trick is again to interleave the order in which the elements are
listed, not waiting until all of the pairs in any one row or column
have been output before we start on the pairs in another row or column.
There are several ways to do this, but for the approach shown here,
we'll use diagonal lines sloping through the grid of pairs shown above.
For example, consider the list of all pairs that add up to some given
number t.  If t=0, then the only option is [(0,0)].  If t=1, then
we get [(0,1),(1,0)].  If t=2, then we get [(0,2), (1,1), (2,0)].
In the general case, the numbers in the diagonal of pairs whose
components add up to t is just [ (n, t-n) | n <- [0..t] ].  And if
we repeat this for all possible values of t, then we get the following:

> pairs :: [(Integer, Integer)]
> pairs  = [ (n, t-n) | t <- [0..], n <- [0..t] ]

Again we can enumerate the first few elements of pairs to see that we
have the expected elements (in this case, the diagonals with sums 0,
1, 2, and 3):

  Main> take 10 pairs
  [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
  Main>

We can also test for specific pairs:

  Main> (1,1) `elem` pairs
  True
  Main> (10,14) `elem` pairs
  True
  Main> (20,17) `elem` pairs
  True
  Main>

Or run a test on a whole block of test cases, in this case a collection
of 121 different tests for pairs (m, n) where the two coordinates
are numbers in [0..10]:

  Main> all (`elem` pairs) [(n,m) | n<-[0..10], m<-[0..10]]
  True
  Main>

All of these tests produce a result of True, confirming that the
elements are included in the pairs list.  Do not be too alarmed
if you tested your solution with larger numbers and got a garbage
collection error.  To get to a pair whose components add up to 4000,
for example, you will have had to generate more than 8 million earlier
pairs, and storing all these might take more memory than Hugs has
allocated.  You will be able to go further if you are using ghci,
but the fundamental problem remains: evaluating all of the elements
in an infinite list takes a lot of memory!

c) Where does a particular pair (n, m) appear in the pairs list?
From the construction above, we can see that it is the nth entry in the
diagonal row for (n+m).  There are (n+m) smaller diagonals with pairs
before that, with 1, 2, 3, ..., (n+m) elements in each one, and hence
a total of sum [1..(n+m)] pairs with a smaller sum.  Adding a further
n to this should get us to the position of (n,m) in the overall list:

> pos      :: (Integer, Integer) -> Integer
> pos (n,m) = n + sum [1..(n+m)]

We can confirm that this gives the right results for some simple
examples:

  Main> pos (0,0)
  0                   --- correct result: see output from pairs above
  Main> pos (2,3)
  17
  Main> genericIndex pairs (pos (2,3))
  (2,3)               --- a special case of property in question
  Main>

The last example above shows that (2,3) is indeed the 17th element
in pairs.

We can also check that pos gives the correct result for some "randomly
chosen" larger index value:

  Main> pairs !! 346
  (21,4)               -- the 346th pair is (21, 4)
  Main> pos (21,4)
  346                  -- and the position of (21, 4) is 346!
  Main> 

Finally, we can run a test to check for correct behavior on a whole
grid of points (100 different tests in one line of code):

  Main> and [ genericIndex pairs (pos (n,m)) == (n,m) | n<-[0..9], m<-[0..9]]
  True
  Main> 

The output True here confirms that the required property holds for
all of the test cases.

If you happen to remember it, there is a closed form formula for
calculating sums of this form:

   sum [1..t] = t * (t+1) `div` 2

Using this, we could also define the pos function as follows:

> pos1      :: (Integer, Integer) -> Integer
> pos1 (n,m) = n + t * (t+1) `div` 2
>              where t = n + m

Of course, we can confirm that this really is the same as pos by
running a few more tests:

  Main> and [ pos p == pos1 p | let ns=[1..100], m<-ns, n<-ns, let p=(m,n) ]
  True
  Main> all (\p -> pos p == pos1 p) pairs
  ^C{Interrupted!}
  
  Main> 

The first of these expressions compares pos and pos1 on over 10,000
different inputs; that particular list comprehension may be most
interesting as an example of using let definitions inside a list
comprehension, which isn't something I've shown in class.  But if
10,000 tests is not enough, the second example will compare the
two functions on all pairs ... or at least, on all the pairs that
it can before we run out of patience and hit control C :-)


Question 4:
-----------
Suppose that n :: Int.  Using the rules given in class, show how the
list comprehension

  [ x - y | x <- [1..n], y <- [1..n] ]

can be rewritten in an equivalent form without comprehensions by
using the map and concat functions.

  [ x - y | x <- [1..n], y <- [1..n] ]

  = concat [ [ x - y | y <- [1..n] ] | x <- [1..n] ]
    { because [ e | gs1, gs2 ] = concat [ [ e | gs2 ] | gs1 ] }

  = concat [ map (\y -> x - y) [1..n] | x <- [1..n] ]
    { because [ e | v <- es ] = map (\v -> e) es }

  = concat (map (\x -> map (\y -> x - y) [1..n]) [1..n])
    { same as previous line }

To verify that this expression gives the same result as the list
comprehension, we'll define:

> comp   n = [ x - y | x <- [1..n], y <- [1..n] ]
> nocomp n = concat (map (\x -> map (\y -> x - y) [1..n]) [1..n])

And now we can run some tests for a couple of small values of n:

  Main> comp 4 == nocomp 4
  True
  Main> comp 100 == nocomp 100
  True
  Main>

Indeed, we can use another list comprehension to run a hundred test
cases:

  Main> and [ comp n == nocomp n | n <- [1..100] ]
  True
  Main> 

Now consider the following expression:

  sum [ x - y | x <- [1..n], y <- [1..n] ]

The result will be zero.  The expression calculates the sum of n*n
numbers.  In this calculation, however, every number that appears as
a positive contribution, x, to the overall sum, also appears equally
many times as a negative contribution to the sum, and hence the
positives cancel out with the negatives leaving zero as the result!


Optional extra: What result do you get from the expression:
  sum [ x | x <- [1..n], y <- [1..n] ]?
 
This will create a list containing n copies (one for each value of
y) of the numbers from 1 to n (cycling through distinct values of
x).  Thus the result of this expression will be:

   n * sum [1..n]

Using the closed form formula for the sum of an arithmetic
progression (mentioned previously in the solution for Q3(c)), we
can further reduce the expression above to:

   n * n * (n+1) `div` 2.

----------------------------------------------------------------------
