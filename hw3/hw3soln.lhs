----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017                 Homework 3

Notes and Sample Solutions
----------------------------------------------------------------------

> import Data.List(sort, nub)

----------------------------------------------------------------------
Question 1:
-----------

- init and tail:

  What happens when we take the init of the tail of a list?

    Main> init (tail [1..10])
    [2,3,4,5,6,7,8,9]
    Main> init (tail "hello, world")
    "ello, worl"
    Main>

  These examples suggest that the end result here is to remove
  both the first and last elements of the input list.  And what
  if we reverse the order of those two functions?

    Main> tail (init "hello, world")
    "ello, worl"
    Main> tail (init [1..10])
    [2,3,4,5,6,7,8,9]
    Main>

  The results are the same!  With this in mind, we can formulate
  the following simple law:

    init . tail = tail . init

  The examples above illustrate that this law is true for some
  specific lists.  In fact, we can even prove that this law holds
  for any list with at least two elements (i.e., any list of the
  form (x:y:ys)):

    (tail . init) (x:y:ys)
      = { by definition of composition }
        tail (init (x:y:ys))
      = { by definition of init }
        tail (x : init (y:ys))
      = { by definition of tail }
        init (y:ys)
      = { by definition of tail }
        init (tail (x:ys:ys))
      = { by definition of composition }
        (init . tail) (x:y:ys)

  On the other hand, for lists with less than two elements, there
  are not enough values in the list to be able to remove both the
  first and the last item, and so we will get an error if we try
  to apply either of the above functions to a singleton or an
  empty list:

    Main> init (tail [1])

    Program error: pattern match failure: init []

    Main> tail (init [1])

    Program error: pattern match failure: tail []

    Main> 

  As such, we might say that the law above only holds for inputs
  with at least two elements.  (However, if we ignore the difference
  in the error message texts shown above and treat all errors as
  being equivalent, then in fact we can say that the law holds for
  all inputs: that is, either init (tail xs) produces the same list
  as tail (init xs) or both produce an error.  This view of errors
  is not something we have talked about in the class yet, so it's
  ok if you have used it here, but it's also ok if you've just said
  that the law only holds for lists with at least two elements.)

- tail and map:

  In this case, we will jump straight to the following law:

    tail . map f  =  map f . tail

  Again, we can actually prove this property for any nonempty
  list input:

    tail (map f (x:xs))
      = { by definition of map }
        tail (f x : map f xs)
      = { by definition of tail }
        map f xs
      = { by definition of tail }
        map f (tail (x:xs))

  However, for the purposes of this question, all that we really
  expected was some simple tests to confirm that the law holds on
  specific examples:

    Main> map (1+) (tail [1..10])
    [3,4,5,6,7,8,9,10,11]
    Main> tail (map (1+) [1..10])
    [3,4,5,6,7,8,9,10,11]
    Main>

  Clearly, the two expressions here produce the same output from
  the same input, but they apply the map (1+) and tail functions
  in the opposite order from one another.

  Once again, there is a special case to consider, this time for
  the empty list because tail is only defined for non-empty lists.

    Main> map (1+) (tail [])

    Program error: pattern match failure: tail []

    Main> tail (map (1+) [])

    Program error: pattern match failure: tail []

    Main>
 
  In this case, we can see that the results are the same, even
  down to details of error messages.  So, technically, we can say
  that this law holds for all inputs, but it's also ok for the
  purposes of this assignment if you said that it only holds for
  nonempty inputs.

- init and reverse:

  Let's experiment again:

    Main> init (reverse [1..10])
    [10,9,8,7,6,5,4,3,2]
    Main> 

  Clearly, the result here is just the same as what we'd get if we
  removed the first element of the input list before reversing:

    Main> reverse (tail [1..10])
    [10,9,8,7,6,5,4,3,2]
    Main>

  With this in mind, we can formulate the following law:

    init . reverse  =  reverse . tail

  Proving that this law holds in general is a bit trickier than
  the examples we saw in the first two parts above.  (Maybe we'll
  come back to that later ... :-)   So for now we'll just content
  ourselves with doing a bit more testing.  To make the task a
  little simpler, we define the following function:

> test1 xs = (init . reverse) xs == (reverse . tail) xs

  The idea here is that a call test1 xs will check to see if the
  law holds for the specific input xs.  All we have to do now is
  to make sure that expressions of this form always return True:

    Main> test1 "hello" && test1 [1,4,7,8,9,3,5,2] && test1 [[],[[]],[[[]]]]
    True
    Main>

  That looks good :-)

  Once again, it's ok if you said that this law is only valid for
  nonempty lists ... but if we treat all errors as equivalent,
  then it is actually true for all lists.  In fact, the law even
  holds for infinite lists, but I wouldn't encourage you to try
  that in practice:  If xs is infinite, the init (reverse xs) and
  reverse (tail xs) will both need to reverse an infinite list
  before they can produce any output, and that will inevitably
  cause the program to run out of memory before it can produce
  a result.

  Incidentally, perhaps you wrote down a slightly different law
  with the functions in the reverse order ... something like:

    reverse . init  =  tail . reverse

  Is this a valid law?  Is this a fundamentally different law?

  In fact, it turns out that this second law is actually implied
  by the first, so long as we assume we're working with finite
  lists.  Here's a proof:

    reverse . init
    = { because, for any f,   f . id  =  f }
      reverse . init . id
    = { because, for finite inputs, reverse . reverse = id }
      reverse . init . reverse . reverse
    = { using first law, init . reverse = reverse . tail }
      reverse . reverse . tail . reverse
    = { because, for finite inputs, reverse . reverse = id }
      id . tail . reverse
    = { because, for any g,   id . g  =  g }
      tail . reverse

  Carefully inspecting this proof, we can see that, by assuming
  the first law, and using some standard properties of the reverse
  function on finite lists, an the identity function, we can
  derive the second law as a direct corollary.

  [Note: students are not expected/required to include reasoning
  or proofs like this.  They are included here for students who
  actually read sample solutions as preparation for material that
  we will study in upcoming lectures!]

----------------------------------------------------------------------
Question 2:
-----------

Preliminaries: As hinted in the question, we're likely to need the
splits definition from the AUG example for our answers here, so
we'll begin by including that definition:

> splits       :: [a] -> [([a],[a])]
> splits []     = []     -- <<<<< Added this line!!!!!
> splits ts     = zip (inits ts) (tails ts)

> inits        :: [a] -> [[a]]
> inits [x]     = []
> inits (x:xs)  = map (x:) ([]:inits xs)

> tails        :: [a] -> [[a]]
> tails [x]     = []
> tails (x:xs)  = xs : tails xs

Note that the definition of splits shown here is taken from the
slides and does NOT use the functions called inits and tails that
are defined in the standard Data.List library.

We'll also include the definitions of layout and appString that
were given in the question:

> layout :: [String] -> IO ()
> layout  = putStr
>         . unlines
>         . zipWith (\n l -> show n ++ ") " ++ l) [1..]

> appString    :: String -> String -> String
> appString l r = "(" ++ l ++ "++" ++ r ++ ")"

Down to business!  In this question, we want to produce a list
describing all of the ways that a given string of characters xs
can be constructed, either by writing out the list xs as a single
string, or else by using the ++ operator (fully parenthesized) to
join together two strings that correspond to a left and right
split of xs.  The first of these cases is easy to handle: we just
apply the show function to present the list xs as the
corresponding string.  This tells us that we can write a
definition for allWays that looks something like the following:

  allWays xs = show xs : ...

The "..." portion here will be responsible for generating all of
the outputs involving the "++" operator.  Using the given
appString function, we can further revise this definition to
something of the following form:

  allWays xs = show xs : [ appString l r | ... ]

Now the "..." portion represents some code that will find all
suitable choices for l and r.  Of course, we know already that we
can produce a list describing all possible ways of splitting xs
into two pieces by using (splits xs); this will generate a list of
pairs (ls, rs) such that ls ++ rs == xs.  And then we can call
allWays recursively to find all possible strings l for building up
ls, and again to find all possible strings r for building up rs.

So now the complete definition is as follows:

> allWays   :: String -> [String]
> allWays xs = show xs : [ appString l r | (ls,rs) <- splits xs,
>                                          l <- allWays ls,
>                                          r <- allWays rs ]

Of course, we can run some quick tests to confirm that we get the
behavior shown in the question with this definition:

  Main> layout (allWays "pdx")
  1) "pdx"
  2) ("p"++"dx")
  3) ("p"++("d"++"x"))
  4) ("pd"++"x")
  5) (("p"++"d")++"x")
  
  Main> layout (allWays "test")
  1) "test"
  2) ("t"++"est")
  3) ("t"++("e"++"st"))
  4) ("t"++("e"++("s"++"t")))
  5) ("t"++("es"++"t"))
  6) ("t"++(("e"++"s")++"t"))
  7) ("te"++"st")
  8) ("te"++("s"++"t"))
  9) (("t"++"e")++"st")
  10) (("t"++"e")++("s"++"t"))
  11) ("tes"++"t")
  12) (("t"++"es")++"t")
  13) (("t"++("e"++"s"))++"t")
  14) (("te"++"s")++"t")
  15) ((("t"++"e")++"s")++"t")
  
  Main> length (allWays "portland")
  2950
  Main> length (allWays "computation")
  223191
  Main>

The first of these outputs produces the same results as shown in
the original question.  For the second, using the input word
"test", we can perform a visual inspection to check that each of
the lines seems to be a valid and distinct expression that will
produce the result "test".  The third and fourth tests were not
actually required, but it is interesting (and scary!) to see how
quickly the number of possible strings grows with increases in the
length of the input.

Because the outputs of allWays are actually Haskell expressions,
we could potentially use the Haskell interpreter to check that
each of the outputs produced by allWays will evaluate to the
original string.

> makeAllWaysTests xs = foldr1 (\x y -> x ++ " && " ++ y)
>                     $ [ show xs ++ "==" ++ way | way <- allWays xs ]

For small cases, we can generate a test string at the interpreter
prompt and then copy and paste the result back in to see if the
resulting expression is True:

  Main> putStr (makeAllWaysTests "or")
  "or"=="or" && "or"==("o"++"r")
  Main> "or"=="or" && "or"==("o"++"r")
  True
  Main> 

If you're okay with longer lines, you can try this with strings of
length 3 or 4.  But much larger than that and you'll want to use
files to capture the output.  We haven't officially seen how to do
this yet, so the following is beyond what I'd expect you to be
able to do but it might still be a good example to look at:

  Main> writeFile "ats.hs" ("x = "++makeAllWaysTests "oregon" ++ "\n")

  Main> !wc ats.hs
         1     377    8247 ats.hs
  Main> :l ats.hs 
  Main> x
  True
  Main>

The first line of this test creates a new Haskell file that
contains the definition for a Boolean variable x.  As the output
of wc shows, the resulting Haskell file has 8247 characters, all
on one line.  Fortunately, Hugs seems to swallow that input just
fine when we load the file and then confirms that x is True: all
tests passed!

----------------------------------------------------------------------
Question 3:
-----------
For this question, we dispense with parentheses in the output
strings.  As before, there are two ways to produce a string for a
given input list xs:

- The first is to turn the whole list into a string by applying
  the show function, just as in Question 1.

- The second is to split xs into two pieces, ls ++ rs == xs, and
  then output the strings in which xs is constructed by appending
  some grouping r of the right portion rs, onto the end of the
  left string ls.  In more concrete terms, this will give us a
  list of strings of the form: show ls ++ "++" ++ r where r can be
  any element of noParens rs.

Putting this all together gives the following definition:

> noParens   :: String -> [String]
> noParens xs = show xs : [ show ls ++ "++" ++ r
>                         | (ls,rs) <- splits xs,
>                           r       <- noParens rs ]

Note, in particular, that there is no need to make a recursive
call to (noParens ls) in this case.  For example, if we split an
input string "test", then all of the outputs will be of the form:
"t"++... or "te"++... or "test"++..., and there is no need to make
recursive calls to split "tes" in the last of those cases, because
we have already accounted for the strings that could produce with
the "t"++... and "te"++... cases.

It never hurts to do a little testing; of course, we can reuse the
examples from Question 2:

  Main> layout (noParens "pdx")
  1) "pdx"
  2) "p"++"dx"
  3) "p"++"d"++"x"
  4) "pd"++"x"

  Main> layout (noParens "test")
  1) "test"
  2) "t"++"est"
  3) "t"++"e"++"st"
  4) "t"++"e"++"s"++"t"
  5) "t"++"es"++"t"
  6) "te"++"st"
  7) "te"++"s"++"t"
  8) "tes"++"t"

  Main> length (noParens "portland")
  128
  Main> length (noParens "computation")
  1024
  Main>

It's no coincidence, in fact, that the number of items in each of
these lists is a power of two.  If you think about it, another way
to generate the output of noParens for a string with (n+1)
characters in it would be:

1) generate the list of 2^n binary numbers
2) for each binary number, output the original string with a break
   after each position where the binary number has a 1 bit.

For example, here are the examples above annotated with the
corresponding bit patterns:

  Main> layout (noParens "pdx")
  1) "pdx"          -- 00
  2) "p"++"dx"      -- 10
  3) "p"++"d"++"x"  -- 11
  4) "pd"++"x"      -- 01

  Main> layout (noParens "test")
  1) "test"              -- 000
  2) "t"++"est"          -- 100
  3) "t"++"e"++"st"      -- 110
  4) "t"++"e"++"s"++"t"  -- 111
  5) "t"++"es"++"t"      -- 101
  6) "te"++"st"          -- 010
  7) "te"++"s"++"t"      -- 011
  8) "tes"++"t"          -- 001

  Main>

Trying to code up an implementation of noParens that works in this
way would be possible, but almost certainly wouldn't produce as
short and simple a solution as the one we've obtained above.
These intutions about noParens, however, do give us another
insight about we can test our definition, checking to see that it
does give the appropriate power of 2 outputs for a range of input
lists:

  Main> and [ length (noParens (replicate n 'a')) == 2 ^ (n-1)| n <- [1..12] ]
  True
  Main>

Incidentally, some students suggested defining noParens in terms
of allWays by writing some code to filter out the parentheses in
the list produced by allWays, and then performing a filtering
action with nub to remove all duplicates from this list.  The
following definition implements the algorithm this way:

> badNoParens = nub . map filterParens . allWays
>  where filterParens = filter ('('/=) . filter (')'/=)

The problem with this approach is that, in order to remove
duplicates, any implementation that uses this technique will need
to keep a list of all of the "previously seen" strings in memory.
For larger inputs, this will create unreasonable demands on
memory.  To see this in practice, we can try the following tests
in Hugs, using the :set +s command to turn on the display of
reduction and cell counts:

  Main> :set +s
  Main> length (noParens "computation")
  1024
  (27649 reductions, 92151 cells)
  Main> length (badNoParens "computation")
  
  (209285967 reductions, 404278840 cells, 2545 garbage collections)
  ERROR - Garbage collection fails to reclaim sufficient space
  Main> 

The first calculation, using noParens, produces a result almost
instantaneously.  The second, however, runs for a long time but
eventually crashes, before it gets to a final answer, having
allocated 404 million cells of memory (each of which is 8 bytes
long) and, after 2,545 separate garbage collections, has finally
run out of memory.  GHCi has a more flexible runtime system that,
unlike Hugs, can increase the size of the heap on demand, so this
example will at least run to completion there, but it will still
take much more memory than is needed, so it does not provide a
practically useful solution.

----------------------------------------------------------------------
Question 4:
-----------
I know that reading sample solutions may not be everyone's
favorite activity.  But those of you that do it anyway will have
realized that significant parts of the answer to this question
were included in the previous set of sample solutions.  Consider
that a reward for the time you've invested in reading the samples
(and as an attempt to encourage you to read future sets of sample
solutions too! :-)

In addition, we'll be needing the following function from the
required reading in the text book:

> perms       :: [a] -> [[a]]
> perms []     = [[]]
> perms (x:xs) = concat (map (interleave x) (perms xs))

> interleave         :: a -> [a] -> [[a]]
> interleave x []     = [[x]]
> interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

Another reward, this time for doing the required reading! :-)
But now let's get started on the actual questions:

- isSorted:

  This part of the question requires the definition of a function
  for testing to see if a given list is sorted.  The following
  definition was included in the HW2 sample solutions:

> sorted         :: Ord a => [a] -> Bool
> sorted (x:y:ys) = (x<=y) && sorted (y:ys)
> sorted other    = True

  We could easily rename this function to isSorted to satisfy the
  requirements in the question.  However, for a little variety, we
  can also define isSorted without explicit recursion by using the
  zipWith function in a cunning manner:

> isSorted   :: Ord a => [a] -> Bool
> isSorted xs = and (zipWith (<=) xs (tail xs))

  To check that sorted and isSorted are the same function, we can
  compare their behavior on a selection of inputs:

    Main> and [ isSorted p == sorted p | p <- perms [1..8] ]
    True
    Main> filter isSorted (perms [1..8])
    [[1,2,3,4,5,6,7,8]]
    Main>

  The first test here compares the two functions on 8! (40,320)
  different inputs, all permutations of [1..8].  Only one of those
  permutions is sorted, but sorted and isSorted give the same result
  in all cases.  The second test picks out the sorted permuations,
  which is just a singleton list containing [1..8] ... which, of
  course, is indeed the only sorted permuation.

- isRevSorted:

  We could define an isRevSorted function in a very similar way
  to isSorted above ... but now that we've done the hard work of
  building the latter, let's combine that with a hint taken from
  the name isRevSorted and test to see we get something that works:

> isRevSorted :: Ord a => [a] -> Bool
> isRevSorted  = isSorted . reverse

  Another 40,320 quick test cases:

    Main> filter isRevSorted (perms [1..8])
    [[8,7,6,5,4,3,2,1]]
    Main>

  Of course, this output confirms that, of all the permutations
  considered, only the one specific example shown in the list
  will pass the isRevSorted test.  And because that is indeed
  the only reverse sorted list among all the permuations, we
  can be confident that isRevSorted is working correctly.

- testSorter:

  As a first attempt: we construct the list of numbers [1..n];
  generate the associated list of all n! permuations by using
  the perms function; run the potential sorting algorithm on
  each one using map; and then check to see that all of the
  results are sorted:

> testSorter0      :: ([Int] -> [Int]) -> Int -> Bool
> testSorter0 alg n = all isSorted (map alg (perms ns))
>                     where ns = [1..n]

  At first glance, this seems to work great, accepting two
  sorting algorithms that we already know and trust, but
  rejecting some functions that are not valid sorting
  algorithms:

    Main> testSorter0 sort 8 && testSorter0 msort 8
    True
    Main> testSorter0 id 8   || testSorter0 tail 8
    False
    Main> 

  However, testSorter0 can be fooled by passing in a "sort"
  function that always produces a specific sorted list as
  it result, regardless of what input we provide:

    Main> testSorter0 (\ns -> [1..10]) 8
    True
    Main> 

  To fix this, we need to make sure that the output from
  each sort function call has the same elements as the input.
  In fact, if we know that the input is a permutation of [1..n],
  then we know that the sorted output must actually be [1..n].
  So instead of testing for sorted outputs, we can just compare
  the output results with [1..n]:

> testSorter      :: ([Int] -> [Int]) -> Int -> Bool
> testSorter alg n = all (ns==) (map alg (perms ns))
>                    where ns = [1..n]

  Repeating the previous tests for testSorter0, we get True
  as output for each valid sorting function, and False for
  invalid sort algorithms:

    Main> testSorter sort 8 && testSorter msort 8
    True
    Main> testSorter id 8   || testSorter tail 8
    False
    Main> testSorter (\ns -> [1..10]) 8
    False
    Main>

  As an aside, we might note that although this implementation
  of testSorter does exactly what is requested in the question,
  it could still be fooled into reporting True, even for a
  function that is not really a valid sort function.  As a
  silly example, we obviously don't get the right result if
  we limit ourselves to testing on singleton lists:

    Main> testSorter id 1
    True
    Main>

  To handle this, we would need to increase testing to use multiple
  length lists.  But even then, our tests could still be subverted,
  for example, by a function whose behavior varies depending on the
  length of the input:

    Main> testSorter (\ns -> if length ns <= 8 then sort ns else ns) 8
    True
    Main> (\ns -> if length ns <= 8 then sort ns else ns) [10,9..1]
    [10,9,8,7,6,5,4,3,2,1]
    Main> 

  The function used here clearly doesn't sort the second list correctly,
  but still persuades testSorter to report a True result.  Another way
  to trick testSorter is to look for inputs that don't show up when we
  only look for permutations of lists of the form [1..n].  In particular,
  this doesn't allow for functions that remove duplicates:

    Main> testSorter (sort . nub) 8
    True
    Main> (sort . nub) [1, 1, 1, 1, 1, 1, 1, 1]
    [1]
    Main> 

  The second evaluation here shows that (sort . nub) does not
  correctly sort a list with duplicates, in spite of the
  enthusiastic True from testSorter in the first evaluation.
  Test sorter can also be fooled by test cases involving negative
  numbers, which don't appear anywhere in perms [1..n]:

    Main> testSorter (sort . map abs) 8
    True
    Main> (sort . map abs) [-5..5]
    [0,1,1,2,2,3,3,4,4,5,5]
    Main>

  In this case, our bogus sort algorithm starts by calculating the
  absolute value of every input using map abs.  This has no impact
  on inputs like the ones that are used in testSorter where all of
  the numbers are positive, so testSorter still reports True.  But,
  as we can see from the preceeding output, (sort . map abs) is not
  a valid sorting algorithm for lists that include negative numbers.

Are there some morals to this story?  Perhaps one or two:

1) Effective testing is hard; it can rightfully increase our
   confidence, but it is possible to overlook important test cases,
   perhaps by focussing on specific test inputs that are not
   representative of real world data that might be used in
   practice.

2) Sample solutions may contain hints for future homework
   assignments!


----------------------------------------------------------------------
Appendix:
---------

Code from the HW2 merge sort implementation; used for testing in
Question 4 above.

> merge        :: Ord a => [a] -> [a] -> [a]
> merge []   ys = ys
> merge xs   [] = xs
> merge (x:xs) (y:ys)
>   | x<=y      = x : merge xs (y:ys)
>   | otherwise = y : merge (x:xs) ys

> halve   :: [a] -> ([a], [a])
> halve xs = (take m xs, drop m xs)
>            where m = length xs `div` 2

> msort    :: Ord a => [a] -> [a]
> msort []  = []     -- empty list already sorted
> msort [x] = [x]    -- singletons already sorted
> msort xs  = merge (msort ls) (msort rs)
>  where (ls, rs) = halve xs

----------------------------------------------------------------------

