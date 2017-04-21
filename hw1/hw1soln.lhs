----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017                 Homework 1

Notes and Sample Solutions
----------------------------------------------------------------------

----------------------------------------------------------------------
Question 1:
-----------
[See rubric for grading details]

----------------------------------------------------------------------
Question 2:
-----------

> dup    :: (a -> a -> a) -> a -> a
> dup f x = f x x

The dup function takes a binary operator (a function of type a -> a -> a)
and returns a unary operator (a function of type a -> a).  The unary
operator obtains its result by passing its argument as both the left and
right operands of the binary operator.

Using this, the original definitions:

> originaldouble  :: Integer -> Integer
> originaldouble n = 2 * n

> originalsquare  :: Integer -> Integer
> originalsquare n = n * n

can be rewritten as:

> double :: Integer -> Integer
> double  = dup (+)

> square :: Integer -> Integer
> square  = dup (*)

Let's do a few quick tests to check that these definitions really are
equivalent.  To make sure we have plenty of tests, we'll run the same test
on a range of different inputs.  And to keep it "quick", we'll use the
interpreter to generate and run the tests automatically:

  Main> and [originalsquare n == square n | n <- [-1000..1000]]
  True
  Main> and [originaldouble n == double n | n <- [-1000..1000]]
  True
  Main> 

This may be the first time you've seen the and function.  But as you might
guess, it is a function of type [Bool] -> Bool that returns True if, and
only if all of the values in the input list are True.  But there are other
ways to implement the same test, such as the following:

  Main> dropWhile (\n -> originaldouble n == double n) [-1000..1000]
  []
  Main> dropWhile (\n -> originalsquare n == square n) [-1000..1000]
  []
  Main>

In this case, the empty list results show that the original and dup versions
of each function return the same result for every element in the list
[-1000..1000] of test cases.

In fact, we can see a common pattern in the two tests here, so perhaps we
should even extract this pattern into a reusable test:

> isSameFunctionAs :: (Integer -> Integer) -> (Integer -> Integer) -> Bool
> f `isSameFunctionAs` g
>        = and [ f n == g n | n <- [-limit..limit] ]
>          where limit = 10000

(The name that we've used here is an exaggeration: isSameFunctionAs doesn't
really guarantee that the two functions are the same, just that they agree
on all integers between +/- the specified limit.  (Which, for good measure,
I've now increased to 10,000.)  Lots of tests, but not much time later:

  Main> double `isSameFunctionAs` originaldouble
  True
  Main> square `isSameFunctionAs` originalsquare
  True
  Main> 

----------------------------------------------------------------------
Question 3:
-----------

> powerOfTwo  :: Int -> Integer
> powerOfTwo n = iterate (2*) 1 !! n

The powers of two are obtained by repeated multiplication by 2.  The
expression iterate (2*) 1 generates a list of all these powers of two,
starting with 1 (which is 2^0, and at index 0 in the list).  This allows
us to find the nth power of two by selecting the nth element of the list.

Note that the definition given above will actually fail if the input is
negative (because !! doesn't allow a negative index value).  The question,
however, didn't say anything about a restriction on the argument to
positive integers only.  Then again, the powers of two for negative
arguments (1/2, 1/4, 1/8, 1/16, ...) are fractional values, not
expressible as Integers.  For these reasons, We won't consider the
behavior of your function on negative arguments.

Another way to answer this question is to write a definition of the
following form:

> powerOfTwo1 n = product (copy n 2)

This uses the copy function defined below.  (There are also several ways
to define copy, each of which gives a corresponding variation in the
definition of powerOfTwo.)  Definitions like these work by computing the
nth power of two as the product of a list of n 2's.  Note that, in the
special case where n=0, this gives 1, which is the correct answer (2^0 =
1), because product [] = 1.

-----

Next up, logTwo v should return the smallest integer n such that v <=
powerOfTwo n.  Let's code a brute force solution to this problem that
looks at every possible candidate value n, starting with 0, until it finds
an answer.  Of course, there will be many solutions to this problem, which
we can collect together in a list.  And if we want to find only the
smallest solution, then we just need to take the head of the list:

> logTwo  :: Integer -> Int
> logTwo v = head [ n | n <- [0..], v <= powerOfTwo n ]

We can calculate the values of logTwo for some small integers by using
a call to map:

  Main> map logTwo [0..17]
  [0,0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5]
  Main>

You might have wanted to try computing the values for this function
using Haskell's built-in operators for computing logarithms.  It's
possible to do that using the logBase function that takes both a base
and the value whose logarithm you want.  After a little experimentation,
I came up with the following two formulations, one in point-free and
another in more conventional form:

> logTwo1  :: Integer -> Int
> logTwo1   = ceiling . logBase 2  . fromIntegral

> logTwo2  :: Integer -> Int
> logTwo2 v = ceiling (logBase 2 (fromIntegral v))

We can test to see that logTwo and logTwo2 are indeed the same function
by applying them to some sample arguments.  But wouldn't it be nice if
we could test them on a much larger set of inputs?  Fortunately, that's
very easy!  Just map each of the two functions over the same list of
inputs and check that we get the same output in each case:

> testLogTwo  :: Integer -> Bool
> testLogTwo n = map logTwo [1..n] == map logTwo2 [1..n]

We start the list of test cases here with 1 because logTwo2 0 is not
defined.  For example, here's a (fairly) quick test to show that the
two functions give the same results, at least for integers in the
range 0 to 1000:

   Main> testLogTwo 1000
   True
   Main>

It's nice to be able to run so many tests that quickly and concisely!

Under the surface, of course, the logTwo2 function relies on floating
point arithmetic, which can be subject to rounding or other numerical
errors.  So I'd be careful about assuming that logTwo2 and logTwo will
always give the same result.  But I did make a brief experiment to see
if I could find a difference between logTwo2 and logTwo on some large
numbers:

  Main> dropWhile (\n -> logTwo n == logTwo2 n) (take 100 (iterate (10*) 1))
  []
  Main>

In this test, I'm creating a list of the first 100 powers of 10 by using
(take 100 (iterate (10*) 1)), and then scanning for items in that list
where logTwo and logTwo2 disagree.  The fact that we get an empty list
as the result tells us that, at least for this particular set of values,
the logTwo and logTwo2 functions behave the same.  Yay for automated
testing!

-----

Our next challenge is to define the following function:

> copy    :: Int -> a -> [a]
> copy n x = [ x | i <- [1..n] ]

This definition uses a list comprehension with individual index values i
pulled from the list [1..n], which we know has n values in it.  But the
value of i isn't used on the left of the | symbol, so every element of
the list that we produce is just a copy of x.

Given that there's a specific example in the question, it won't hurt to
test at least that.  For good measure, I'll also test powerOfTwo against
powerOfTwo1, which is also an indirect way to test our definition of
copy.

  Main> copy 3 True
  [True,True,True]

  Main> and (map (\n -> powerOfTwo n == powerOfTwo1 n) [1..1000])
  True

As is common, there are some other ways to obtain the same kind of
result.  For example, we could express the same basic approach using
map:

> copy1 n x = map (\i -> x) [1..n]

Another approach is to use the prelude function repeat to construct a
potentially infinite list of x's and then combine that with take to get
a list with exactly n elements:

> copy2 n x = take n (repeat x)

We hadn't seen repeat in class at the time when I gave out this
assignment, so don't feel bad if you didn't know about this option (but
if you'd happened to read ahead and find out about it already, that's ok
too).

In practice, Haskell programmers don't need to define a copy function
because there is already a suitable function in the prelude.  The only
difference is that the prelude function is called "replicate":

> copy3 n x = replicate n x

However, as indicated in the question, using replicate in your answer to
this question won't get you credit!

One last idea: could you define the copy function using an arithmetic
sequence like the following?

> copy4 n x = take n [x,x..]

Unfortunately, this only works if x is a value of a type whose elements
can be enumerated using the arithemetic sequence expression [a,b..];
this works for numbers and characters, but not strings, for example:

  Main> copy4 10 12
  [12,12,12,12,12,12,12,12,12,12]
  Main> copy4 10 "hi"
  ERROR - Cannot infer instance
  *** Instance   : Enum [Char]
  *** Expression : copy4 10 "hi"

  Main>

-----

The final definition in this question is for the multiApply function:

> multiApply      :: (a -> a) -> Int -> a -> a
> multiApply f n x = iterate f x !! n

The definition given here is just a generalization of the approach that
we used to define powerOfTwo above: we apply f repeatedly to the
argument x and then select the nth element in the resulting list, which
contains f^n x.

It doesn't take a lot of work to check at least the examples given in
the question:

  Main> multiApply (\x -> x*x) 2 2
  16

  Main> multiApply not 0 True
  True

We're also asked to consider the following function:

> q f n m x = multiApply (multiApply f n) m x

Clearly n and m must be of type Int in this definition because they are
both used as the second argument of multiApply, which requires an Int
value.  If we assume that f :: a -> a, for some type a, then we can
conclude that:

  multiApply :: (a -> a) -> Int -> a -> a,

so

  multiApply f :: Int -> a -> a

and hence:

  multiApply f n :: a -> a

But then, by a very similar argument, we can conclude that

  multiApply (multiApply f n) m x :: a

and that x must also have type a.  Thus the type of q is just:

  q :: (a -> a) -> Int -> Int -> a -> a

Of course, if we don't want to do all that work, we could just ask a
friendly Haskell interpreter for some help:

  Main> :t q
  q :: (a -> a) -> Int -> Int -> a -> a
  Main> 

Hooray, we got the same answer!  And if that's all you did in you answer
to this question, we'll still give you credit ...  but I hope you'll
have followed along with the approach I used above so that you'll know
how to answer questions like this in future without the aid of hugs or
ghci.

What exactly does the cryptically named q function do?  First let's
think about the (multiApply f n) subexpression: this is a function that
will take an argument, apply the function f to it n times, and then spit
out a result.  So when we pass that in as an argument of multiApply with
a count m, then we're going to end up, for each of m times, applying the
function f to an argument n times.  In total, this means that we'll end
up applying f a total of (n * m) times to the original argument.  In
other words:

  q f n m x  returns f^(m*n) x

or, to put it another way:

  q f n m x = multiApply f (n*m) x

Now we have two definitions for q, and if we put the right hand sides of
those definitions together, then we get the following law:

  multiApply (multiApply f n) m x = multiApply f (n*m) x

or, if we drop the argument x from both sides:

  multiApply (multiApply f n) m  =  multiApply f (n*m)

If you're a real fan of the point-free approach, you could take this one
step further still and write:

  multiApply (multiApply f n)  = multiApply f . (n*)

But from my perspective, that's not as intuitive or as reasable as
either of the previous forms, so I probably wouldn't use the latter in
practice.

Last, but not least, experience has taught me that it's often prudent to
check my answers just in case I've made a mistake.  So let's define a
testing function:

> multiApplyTest n m = (multiApply (multiApply (1+) n) m 0)
>                      ==
>                      (multiApply (1+) (n*m) 0)

(The idea here is to fix a function f = (1+) and use x = 0 as the
starting point; when I multiApply f n x with these values, I should
expect to get n back as the result.)  And now I can test on a range of
different values:

  Main> and [ multiApplyTest n m | n <-[0..99], m<-[0..99] ]
  True
  Main> 

It took a while for that computation to run---I don't know exactly how
long because I took a break to stretch my legs :-)  But when I got back
and saw the True result after 10,000 different tests, I felt pretty
comfortable that I had got the right law. :-)

----------------------------------------------------------------------
Question 4:
-----------
I'll show two ways to implement revindex.  One option is to use some
arithmetic based on the length of the list:

> revindex1     :: [a] -> Int -> a
> revindex1 xs n = xs !! (length xs - (n+1))

Another option is just to reverse the list before indexing:

> revindex2     :: [a] -> Int -> a
> revindex2 xs n = reverse xs !! n

There are two specific tests given in the question, and we should
confirm that our solutions (only one was actually required) both satisfy
at least these tests:

  Main> revindex1 [1..10] 0
  10
  Main> revindex1 [1..10] 7
  3
  Main> revindex2 [1..10] 0
  10
  Main> revindex2 [1..10] 7
  3
  Main>

Short and sweet, we'll leave it at that for this question!

----------------------------------------------------------------------
Question 5:
-----------
Here are the definitions from the question:

> strange xs = head (head (reverse (takeWhile notnull (iterate twirl xs))))
> notnull xs = not (null xs)
> twirl xs   = reverse (tail xs)

The twirl function discards the first element of the list and then
reverses what remains:

  Main> twirl [0..9]
  [9,8,7,6,5,4,3,2,1]
  Main> 

If we apply twirl a second time to the result of an initial twirl (i.e.,
in an expression of the form twirl (twirl xs)), then we will end up with
a copy of the list xs with its first and last elements removed:

  Main> twirl (twirl [0..9])
  [1,2,3,4,5,6,7,8]
  Main>

When we iterate twirl on a given list, we alternate between removing
elements from each end of the original list until we get down to an
empty list.  The next attempt to twirl after that will trigger a pattern
match failure:

  Main> iterate twirl [0..9]
  [[0,1,2,3,4,5,6,7,8,9],[9,8,7,6,5,4,3,2,1],[1,2,3,4,5,6,7,8],[8,7,
  6,5,4,3,2],[2,3,4,5,6,7],[7,6,5,4,3],[3,4,5,6],[6,5,4],[4,5],[5],[],
  Program error: pattern match failure: tail []

  Main> iterate twirl [0..10]
  [[0,1,2,3,4,5,6,7,8,9,10],[10,9,8,7,6,5,4,3,2,1],[1,2,3,4,5,6,7,8,
  9],[9,8,7,6,5,4,3,2],[2,3,4,5,6,7,8],[8,7,6,5,4,3],[3,4,5,6,7],[7,6,
  5,4],[4,5,6],[6,5],[5],[],
  Program error: pattern match failure: tail []

  Main> 

I've picked the two specific lists shown in this example so that we can
see what happens in the case of even length lists like [0..9] and odd
length lists like [0..10].  I've also chosen to use the integers from 0
counting upwards so that we can track the values in each list by their
index in the original list.  In both of the examples above, we see that
the last nonempty list in the result that is produced by iterate is [5],
which we note is just the length of the original list divided by 2
(ignoring remainder).  In Haskell terms, if the original list is [0..n],
then this is just (n `div` 2).

The takeWhile notnull call chops off the empty list, and everything that
follows it in each of the lists above.  As a result, when we reverse the
resulting lists, we get a list whose head, at least for each of the two
examples above, is [5].  And because the head of this list is 5, we can
easily predict the following results:

  Main> strange [0..9]
  5
  Main> strange [0..10]
  5
  Main>

Based on these observations, we might conjecture that strange always
returns the middle element of any list that is supplied as its argument.
(There is no other way that it could have produced 5 in the specific
cases above, for example, because there was only one 5 in each input
list and it is clear that strange does not do any calculations that
involve the list elements themselves.  More precisely, we might
formulate:

> lessStrange xs = xs !! (length xs `div` 2)

Does this really do what we expect?  Let's test it on a couple of quick
cases:

  Main> lessStrange [0..9]
  5
  Main> lessStrange [0..10]
  5
  Main> 

That clearly agrees with what we obtained previously using the original
strange function.  So let's try it on a lot more tests:

  Main> and [ strange [0..n] == lessStrange [0..n] | n <- [0..400] ]
  True
  Main>

To my mind, this is good evidence that strange and lessStrange are
probably the same function ... :-)

----------------------------------------------------------------------
