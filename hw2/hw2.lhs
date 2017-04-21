----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017                 Homework 2
----------------------------------------------------------------------

This is a literate Haskell script that can be loaded directly in a
Haskell interpreter like hugs or ghci.

> import Data.List

----------------------------------------------------------------------
Reading: Read Chapters 3-6 from the textbook (and feel free to peek
at Chapter 7 for a little reinforcement of what we've talked about
this week and a peek to some of the material we'll be covering next
week).  You may choose to skim (or even skip) the material about the
"Caesar cipher" in Section 5.5; it's entertaining, and does provide
more examples of Haskell coding that you might find useful, but I
will not expect you to be familiar with the specifics as we move
forward.  You are encouraged to use the exercises in these chapters
to test your understanding, but only the specifi questions listed
below will be required.

----------------------------------------------------------------------
For Credit Exercises:  Your solutions to the following questions are
due at the start of class (10am) on April 20 in person, or by the same
time if you submit via D2L (submission in person/on paper is strongly
preferred if possible).

Please follow the same general guidelines that were provided for
Homework 1 (and in the course syllabus).

----------------------------------------------------------------------
Exercises from the textbook:
----------------------------
Chapter 5 (P57-58): Questions 6 and 9
Chapter 6 (P72): Questions 7 and 8

----------------------------------------------------------------------
Additional exercises:
---------------------
Question 1:
-----------
Give possible types for each of the following expressions (or else
explain why you think the expression is ill-typed).
 a) map odd
 b) takeWhile null
 c) (++[])
 d) (:[])
 e) ([]:)
 f) [ [], [[]], [[[]]], [[[[]]]], [[[[[    ]]]]] ]
 g) [ [], [[]], [[[]]], [[[[]]]], [[[[[True]]]]] ]
 h) [ [True], [[]], [[[]]], [[[[]]]], [[[[[]]]]] ]
 i) map map
 j) map (map odd)
 k) map . map
 l) (map , map)
 m) [ map id, reverse, map not ]

You may use hugs or ghci to help you answer these questions, but you
are strongly encouraged to try and figure out an answer for yourself
first, without typing it in.  Note, in particular, that you are only
expected to give a *possible* type for each expression, not a *most
general* type (although the latter are still valid answers).  For
example, you could conclude that (\x -> x) has type Int -> Int, which
is a possible type for that lambda expression, even though the most
general possible type is (a -> a) (in which a is a type variable).

----------------------------------------------------------------------
Question 2:
-----------
Explain what each of the following functions does.  (Note that
your answers should reflect the behavior of each function at a
high-level and should not just be a restatement of the Haskell
syntax.  For example, you could say that (sum . concat) is a
function that "adds up all of the values in a list of list of
numbers", but you wouldn't get credit for saying that it is
"the composition of the sum and concat functions".)

 a) odd . (1+)
 b) odd . (2*)
 c) ([1..]!!)
 d) (!!0)
 e) reverse . reverse
 f) reverse . tail . reverse
 g) map reverse . reverse . map reverse

----------------------------------------------------------------------
Question 3:
-----------
For this question, we need to introduce two new functions:

The function elem, defined in the standard prelude, can be used to
test if a given value is a member of a particular list.   For
example:

-  elem 4 [1..10]  and  elem 5 [-10..10]  both return True
-  elem 0 [1..10]  and  elem 5 [2,4..100] both return False

The function genericIndex, defined in the Data.List library, is a
variant of the (!!) operator that allows Integer index values.  In
other words, genericIndex xs n will return the nth element of the
list xs (starting from index 0), even if n is an integer.  (By
comparison, xs !! n will only work if n is an Int.)

a) Modify the following definition so that the symbol integers
represents the list of all Integer values, both positive and
negative:

> integers :: [Integer]
> integers  = [0..]

Specifically, once you have modified the definition, you should be
able to guarantee that  elem n integers  will return True for an
integer value n.  [Hint: as a starting point, make sure you
understand why the definition of integers given above is not a
valid answer.]

b) Modify the following definition so that the symbol pairs
represents the list of all pairs (n, m) of non-negative Integer
values:

> pairs :: [(Integer, Integer)]
> pairs  = [ (n,m) | n <- [0..], m <- [0..] ]

Specifically, once you have modified the definition, you should be
able to guarantee that  elem (n,m) pairs  will return True for any
non-negative integers n and m.  [Hint: again, as a starting point,
make sure you understand why the definition of pairs above is not a
valid answer.]

c) Define a function  pos :: (Integer, Integer) -> Integer  such
that, for any non-negative values n and m:

          genericIndex pairs (pos (n, m)) = (n, m)

[We refer to this function as pos because pos (n, m) returns the
position of the pair (n, m) in the list pairs.]

----------------------------------------------------------------------
Question 4:
-----------
Suppose that n :: Int.  Using the rules given in class, show how
the list comprehension

  [ x - y | x <- [1..n], y <- [1..n] ]

can be rewritten in an equivalent form without comprehensions by
using the map and concat functions.  Use hugs or ghci to verify
that the two forms of the expression produce the same result for
some small values of n.

Describing how you arrive at your answer, explain what result you
will get by evaluating an expression of the following form:

  sum [ x - y | x <- [1..n], y <- [1..n] ]

(An informal, intuitive explanation is sufficient; you do not need
to give a formal proof, and you do not need to use a version of
this expression in terms of map and concat.)

(Optional extra: what result will you get from the expression:
  sum [ x | x <- [1..n], y <- [1..n] ]
?)

----------------------------------------------------------------------

