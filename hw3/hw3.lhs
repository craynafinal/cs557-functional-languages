----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017                 Homework 3
----------------------------------------------------------------------

This is a literate Haskell script that can be loaded directly in a
Haskell interpreter like hugs or ghci.

----------------------------------------------------------------------
Reading: Read Chapters 7-9 from the textbook. Some notes on particular
sections:

- Sections 7.6 "Binary string transmitter" and 7.7 "Voting algorithms"
  describe simple applications, introduce some new functions, and
  reuse some old friends.

- Sections 8.3 and 8.5 introduce newtype and class and instance
  declarations, respectively.  We have not discussed these in class
  yet.

- Sections 8.6 and 8.7 are neat applications; you can skip 8.7 for
  now, but 8.6 is well worth study if you're feeling that you would
  benefit from more examples with algebraic datatypes.

- Chapter 9 is all about solving a specific problem that was inspired
  by a class daytime game show from British TV called "Countdown".
  (Of course, you can find videos of clips from the show on YouTube
  if you want to see what the show actually looks like: search terms
  like "Countdown Numbers Game" should get you pretty close.)  The
  material in Sections 9.4 and 9.6 is most directly relevant to the
  exercises in this homework exercise.

You are strongly encouraged to use the exercises in these chapters
to test your understanding, but I am not assigning any of those
questions as required exercises this week.

Maybe the book is so well written that you don't need any help to
understand it?  But so far, as best I can remember, not one person
has asked me a question about the text in the book!  Please do
remember that it's perfectly ok to ask questions about any part
of the assigned reading that doesn't seem to make sense or isn't
clear to you in the D2L discussion section for this assignment.

----------------------------------------------------------------------
For Credit Exercises:

Your solutions to the following questions are due at 10am on April
27 IF you submit via the D2L dropbox, OR at the start of the class
(i.e., 10am) on May 2 if you submit on paper.  (Recall that there is
no class on April 27, so I can't collect assignments in person that
day ... hence the extended deadline for in person submissions.)

Please follow the same general guidelines that were provided for
Homework 1 (and in the course syllabus).  This assignment does not
require a large quantity of Haskell code.  However, the grading
scheme will include non-trivial components for clarity,
explanation/commentary, and testing, so be sure to address these as
part of your solution!

Question 1:
-----------
Formulate a law to describe the interaction between the tail and
init functions, both of type [a] -> [a], that are defined by the
Haskell prelude.  State as carefully as possible any conditions that
are needed to ensure that the law is valid, and include some
evidence of testing to show examples where the law holds.

Repeat this exercise two more times, once for the tail and map
functions, and once for the init and reverse functions.

Question 2:
-----------
There are many different ways to construct a non-empty string using
only non-empty string literals and the ++ operator.  For example,
the string "pdx" can be constructed by writing "pdx", ("p"++"dx"),
or (("p"++"d")++"x") (and these are not the only options).

Your task in this question is to define a function:

> allWays   :: String -> [String]
> allWays xs = error "replace this with your definition!"

that will produce a list of strings that show all of the possible
ways to build the given string in this way, provided that the input
is not empty.  To help you to display the output from this function
in a readable manner, you may use the following function:

> layout :: [String] -> IO ()
> layout  = putStr
>         . unlines
>         . zipWith (\n l -> show n ++ ") " ++ l) [1..]

Remember also that you can convert an arbitrary string into the text
for a corresponding string literal by applying the show function:

  Main> show "hello"
  "\"hello\""
  Main>

For example, here is what you might see when you use allWays and
layout together in a Hugs or GHCi session:

  Main> layout (allWays "pdx")
  1) "pdx"
  2) ("p"++"dx")
  3) ("p"++("d"++"x"))
  4) ("pd"++"x")
  5) (("p"++"d")++"x")

  Main> 

Note that it is not necessary for your solution to list the
generated strings in the same order as shown in this input.  If
you happen to come up with a different variation of the code that
lists them in a different order, that will be just fine.  However,
you should follow the convention used above in which parentheses
are placed around any use of the ++ operator.  You are strongly
encouraged to use the following function to help construct strings
that are in this format:

> appString    :: String -> String -> String
> appString l r = "(" ++ l ++ "++" ++ r ++ ")"

[Hint: The splits function that was introduced in the textbook for
the reading portion of this assignment is likely to be *very* useful
in this task.]

Question 3:
-----------
Of course, in practice, there is no need to include parentheses in
expressions that construct lists using the notation from Question 2.
For example, ("p"++("d"++"x")) and (("p"++"d")++"x") are equivalent
to "p"++"d"++"x" because the ++ operator is associative.  Your task
in this question, therefore, is to write a new function:

> noParens   :: String -> [String]
> noParens xs = error "replace this with your definition!"

that generates a list of strings showing all of the possible ways to
construct the given input list using only ++ and string literals,
without any repetition or parentheses.  For example, here is an
example showing how this function might be used in Hugs or GHCi:

  Main> layout (noParens "pdx")
  1) "pdx"
  2) "p"++"dx"
  3) "p"++"d"++"x"
  4) "pd"++"x"

  Main>

Note here that there are only 4 output lines in this particular
example, so you cannot produce the output for noParens simply by
removing the open and close parenthesis characters from the output
of allWays.  [Hint: Indeed, your definition of noParens will
probably have a different structure to your definition of allWays,
although it might still make good use of the splits function ...]

Question 4:
-----------
Using explicit recursion, or otherwises, define a function:

   isSorted :: Ord a => [a] -> Bool

such that isSorted xs is True precisely when the list of values
in xs is sorted in ascending order.

Using the isSorted function, but no explicit recursion, define
a function:

  isRevSorted :: Ord a => [a] -> Bool

such that isRevSorted xs is True precisely when the list of
values in xs is sorted in descending order.

Define a higher-order function:

  testSorter :: ([Int] -> [Int]) -> Int -> Bool

such that testSorter alg n will return True if, and only if the
function will correctly sort every permutation of the numbers
between 1 and n.  As test cases of valid sorting algorithms, you are
encouraged to use the sort function that is defined in the Data.List
library, and the msort function that you defined for Homework 2.  Be
sure to demonstrate that your testSorter implementation correctly
rejects some functions of type [Int] -> [Int] that are not valid
sorting functions.  [Hint: You may use functions defined above or
described in the assigned reading for this assignment to simplify
your task here.]

----------------------------------------------------------------------

