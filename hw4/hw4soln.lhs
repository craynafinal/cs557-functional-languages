----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017

Homework 4: Notes and sample solutions
----------------------------------------------------------------------

> module HW4Soln where

----------------------------------------------------------------------
QUESTION 1: What do we have to define to use VizTree with the
Treedot library?

First, we actually have to import the "Treedot" library:

> import Treedot

The VizTree type is defined as follows:

> data VizTree = VizNode String [VizTree]

The Treedot library demands that the tree to be graphically printed
is an instance of LabeledTree.  Recall the definition of toDot (I've
added a row of ^^^^ to bring your attention to the LabeledTree
constraint):

toDot         :: LabeledTree t => t -> String
                 ^^^^^^^^^^^^^^^^
toDot          = graph . nodeTree []

We can just use the first component of each VizNode as the label for
that node:

> instance LabeledTree VizTree
>   where label (VizNode s _) = s

But our work is not done!  The definition of LabeledTree says that
any instance of that class must also be a Tree:

class Tree t => LabeledTree t where
      ^^^^^^^^^
  label :: t -> String

The subtrees of a VizNode are just the list of trees in the second
field:

> instance Tree VizTree
>   where subtrees (VizNode _ ts) = ts

Now we are ready to use VizTree values with the Treedot library!

----------------------------------------------------------------------
We define a class of visualizable types:  (The class declaration and
two of the instances are commented out because they will be redefined
later on and we can only have one version of a class in scope at any
given time.)

  class Viz a
    where toVizTree :: a -> VizTree

> instance Viz Integer
>   where toVizTree n = VizNode (show n) []

  instance Viz Char
    where toVizTree n = VizNode (show n) []

  instance Viz a => Viz [a]
    where toVizTree []	   = VizNode "[]" []
          toVizTree (x:xs) = VizNode ":" [toVizTree x, toVizTree xs]

QUESTION 2: Why the last line is ok?

In the last line, toVizTree x is applying toVizTree to a value of
type a where a is an instance of Viz, while toVizTree xs is
applying toVizTree to a value of type [a] where a is an instance
of Viz.  Note that so far a can be Integer, Char, list of Integer,
list of Char, list of list of Integer, list of list of Char...

Although x and xs have different types in this expression, the two
expressions (toVizTree x) and (toVizTree xs) both have type
VizTree, so it is ok to include them together in a single list.

----------------------------------------------------------------------
QUESTION 3: Provide instances of the Viz class for Bool, Int, pairs,
triples, and Maybe:

The instance declarations for these types are fairly straightforward:

> instance Viz Bool
>   where toVizTree b = VizNode (show b) []

> instance Viz Int
>   where toVizTree n = VizNode (show n) []

> instance (Viz a, Viz b) => Viz (a, b)
>   where toVizTree (a, b) = VizNode "Pair" [toVizTree a, toVizTree b]

> instance (Viz a, Viz b, Viz c) => Viz (a, b, c)
>   where toVizTree (a, b, c)
>            = VizNode "Triple" [toVizTree a, toVizTree b, toVizTree c]

> instance Viz a => Viz (Maybe a)
>   where toVizTree Nothing  = VizNode "Nothing" []
>         toVizTree (Just a) = VizNode "Just" [toVizTree a]

----------------------------------------------------------------------
QUESTION 4: Provide a way to visualize a VizTree (i.e., provide an
instance declaration that makes VizTree an instance of the Viz class):

> instance Viz VizTree where
>   toVizTree (VizNode s ts)
>     = VizNode "VizNode" [toVizTree s, toVizTree ts]

Every VizNode is labeled as a "VizNode" and has two subtrees, one
for each of its two components.

----------------------------------------------------------------------
Here is the main visualization function from the question:

> viz :: Viz a => a -> IO ()
> viz = writeFile "tree.dot" . toDot . toVizTree

QUESTION 5: Why does Prof. Senoj need to provide an additional type
annotation, ::Integer, in examples like viz [1..4::Integer]:

The reason here is that viz [1..4] has an ambiguous type.  (See Week 5,
Slide 110!)  In particular:

  [1..4] :: (Num a, Enum a) => [a]   -- Works for any enumerable,
                                     -- numeric type

  viz :: Viz a => a -> IO ()         -- Works for any visualizable type

So:

  viz [1..4] :: (Num a, Enum a, Viz a) => IO ()

The type variable "a" appears on the left of the => symbol, but not on
the right, so the type is ambiguous: different choices for instantiating
the variable "a" could potentially lead to different behaviors.  More
concretely, if we added an instance for Viz Float, then viz [1..4] could
produce a diagram in which the numbers are displayed either as "1", "2",
"3", "4" (by choosing a = Int), or as "1.0", "2.0", "3.0", "4.0" (by
choosing a = Float).

When Prof. Senoj adds the type annotation, ::Integer, to the examples,
the choice of type for "a" is made explicit, so there is no ambiguity,
and the behavior of the program is uniquely determined.

Those of you using GHCi have discovered a way in which that system
extends the definition of Haskell by picking a "default" choice of
the type "a" automatically.  This would be convenient for Prof. Senoj
because he wouldn't have to include an explicit type annotation every
time, but it might also be confusing because it hides the fact that
a "default" is being chosen without the user knowing it.

----------------------------------------------------------------------
Here are the revised class definitions suggested by Apsus Tudent:

> class Viz a
>   where toVizTree :: a -> VizTree
>         toVizList :: [a] -> VizTree
>         toVizList []     = VizNode "[]" []
>         toVizList (x:xs) = VizNode ":" [toVizTree x, toVizTree xs]

> instance Viz Char
>   where toVizTree n = VizNode (show n) []
>         toVizList s = VizNode ("\\\""++s++"\\\"") []

> instance Viz a => Viz [a]
>   where toVizTree = toVizList

QUESTION 6: Explain how this works, and why it is not necessary to
change any other parts of the code.

The key change in this code is the addition of a new member,
toVizList, in the Viz class, with a *default definition* that uses
exactly the same code that we used for visualizing lists in the
original implementation.

Because we have provided a default implementation for this function,
there is no need for any other instance to be extended with a
definition of toVizList; the default definition will be used whenever
there is no explicit definition.  This is why we don't need to change
any other parts of the code when the changes shown here are made.

Now, when we try to visualize a list type using toVizTree, we'll
be using the definition  toVizTree = toVizList  in the third
instance declaration above.  If the value that we are trying to
visualize has type [Char] (i.e., if it is a String), then this
will use the version of toVizList that was defined in the Viz Char
instance, creating a single VizNode with the text of the string as
its label.  In any other case, we'll be using the definition of
toVizList for a different instance ... and because there are no
other explicit definitions, that means we'll be using the
default definition, which constructs a visualization of the list
as a tree made from (:) nodes with a [] at the far right, just
as before.  Of course, this is exactly the behavior that we were
looking for!

As another way to explain what is going on here, we can use
some pseudo Haskell expressions in which we write the relevant
type immediately after the name of an overloaded function; this
will allow us to track which implementation is being used at
each step:

  toVizTree [1,2]
   = { because [1,2] is a list of type [Int] }
     toVizTree[Int] [1,2]
   = { because toVizTree[Int] = toVizListInt }
     toVizListInt [1,2]
   = { toVizListInt uses the default definition for toVizList }
     VizNode ":" [VizNode "1" [],
                  VizNode ":" [VizNode "2" [],
                               VizNode "[]" []]]

However:

  toVizTree "hi"
   = { because "hi" is a list of type [Char] }
     toVizTree[Char] "hi"
   = { because toVizTree[Char] = toVizListChar }
     toVizListChar "hi"
   = { using definition of toVizList in Viz Char instance }
     VizNode ("\\\""++"hi"++"\\\"") []
   = { simplifying uses of ++ }
     VizNode "\\\"hi\\\"" []

As these calculations illustrate, we will use the toVizListChar
implementation to output strings (i.e., values of type [Char]),
but the default implementation (which uses the square brackets
and comma notation for lists) in all other cases.

----------------------------------------------------------------------
QUESTION 7: What are the most limiting aspects of this viz
implementation?

1) It will only work with types that have been declared as instances
   of the Viz class.  Asking programmers to define new instances of
   Viz for every type could be tedious or even impossible (if, for
   example, the type includes function values)

2) The viz function can't handle infinite or cyclic structures; an
   attempt to visualize a value like [1..] will attempt to produce
   an "infinite" picture, which will not terminate properly.  (In
   practice, it will likely terminate eventually with an out of
   stack, memory, or disk space error ... but none of these counts
   as "proper termination", and none will produce a graph that can
   be viewed using GraphViz.)  One possible way to mitigate this
   problem is to introduce a "pruning" function that trims any
   generated VizTree to cut off nodes below a certain depth before
   we attempt to construct the tree.

----------------------------------------------------------------------
QUESTION 8: The goal here is to have some fun building some
"impressive" examples to demonstrate (and test) the code that you
have developed in the previous questions.  In particular, this means
that we should provide graphs that, at least, test the implementation
of toVizTree for:

- the Bool, Int, pair, triple, and Maybe instances of Viz (Q3)
- the VizTree instance of Viz (Q4)
- the visualization of String values (Q6)

We could try each of these individually, but we can also build some
more complex examples that include instances of all of these in a
single structure.  The following definitions include values for
almost all of the above types:

> pairOfBools   = (True,False)
> tripleOfInts  = (-1::Int, 0::Int, 1::Int)
> listOfMaybe   = [Just "hello", Nothing]
> example1      = (pairOfBools, tripleOfInts, listOfMaybe)

Now we can generate a tree diagram for example1 using:

   viz example1

The resulting diagram (in the accompanying ex1.pdf file) has the
structure that we expect; each interior node is labeled with an
appropriate name for the corresponding constructor, and the values at
the leaves are either basic constants (like the integers -1, 0, and
1), strings (captured in a single node), or constructors with no
children (like True, False, and Nothing).  (Technically speaking, the
latter are really interior nodes, even though they appear as leaves
in the diagram.)

The visualization of example1 covers all of the types from Q3 and Q6,
but not the VizTree type from Q4.  One way to address that is to
define another example, like the following, specifically for VizTree:

> example2 = VizNode "a" [VizNode "b" [VizNode "f" []],
>                         VizNode "c" [VizNode "g" [], VizNode "h" []],
>                         VizNode "d" [],
>                         VizNode "e" [VizNode "i" []]]

The resulting diagram in ex2.pdf, generated by running viz example2,
and then using "dot -Tpdf -o ex2.pdf tree.dot" captures the expected
structure, using interior nodes labeled with VizNode (the sole
constructor for VizTree) and ":" and "[]" (the two constructors for
lists that are used to build lists of children in VizTrees).  The
remaining leaf nodes are labeled with Strings, matching the pattern in
the definition of example2.

However, for those who don't want to build a VizTree by hand, we could
use the toVizTree function to build some for us:

> example3 = toVizTree example1
> example4 = toVizTree example2

The resulting diagrams (in ex3.pdf and ex4.pdf, respectively) are
more complex than our previous examples, but still show the expected
structure.  But using toVizTree like this opens a "Pandora's box",
because it is clear that we can apply toVizTree multiple times,
generating new, more complex, and more intimidating/impressive
examples at each step:

> example5 = toVizTree example3
> example6 = reverse (take 5 (iterate toVizTree (toVizTree True)))

The resulting diagrams are in ex5.pdf and ex6.pdf, respectively, but
you will need to zoom out quite a long way to fully appreciate the
structure!

[The use of reverse, take, and iterate in example6 illustrates the
use of these now-familiar operators to build a surprisingly complex
tree structure, starting from the visualization of a very simple
value: True.  There is no deep significance to the particular
combination of operators used here; they were chosen by
experimenting and inspecting the resulting diagrams.]

----------------------------------------------------------------------
