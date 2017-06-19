----------------------------------------------------------------------
CS457/557 Functional Languages, Spring 2017

Homework 5: Solutions and grading guide
----------------------------------------------------------------------

This file is a literate Haskell script, which means that it can be
loaded directly into a Haskell interpreter like Hugs or GHCi, bringing
the functions that are defined (as indicated by a '>' character in the
first column) into scope.

I've kept the sections of code from the questions so that I can use
this file for testing, but have otherwise trimmed out as much of the
additional text and examples as I could.

--------------------------------------------------------------------

> module HW5 where
> import IOActions hiding (find)
> import System.IO

--------------------------------------------------------------------
QUESTION 1: Use online sources or inspect the files that are already
installed on your computer to give brief documentation, in your own
words, for the find function that is defined in Data.List.  Target
your comments at a reader who has basic familiarity with Haskell,
but no previous experience with Data.List.
--------------------------------------------------------------------

The purpose of this exercise is to test your confidence in browsing
online documentation or Haskell source files on your computer for
information.

A good answer to this question will provide (not necessarily in
this order):

- An indication of how you got the information you needed.
  For example, I opened up Hugs and did:

   :l Data.List       loads the Data.List library
   :f find            opens an editor at the source for "find"

- A type for the function:

   find :: (a -> Bool) -> [a] -> Maybe a

- A brief description in "your own" words.

  For example: "find p xs returns Just x if x is the first element
  in the list xs that satisfies p.  If there are no elements in
  the list xs that satisfy p, then the result is Nothing."

  Depending on which source you use, some students might find it
  difficult to come up with a wording that differs significantly
  from what is already included in the source code.  For example:
  "The 'find' function takes a predicate and a list and returns
  the first element in the list matching the predicate, or
  'Nothing' if there is no such element."  Despite the explicit
  "in your own words" comment in the question, we won't take
  points away if you've used something very similar to this; the
  function is simple enough, it might be hard to find a
  fundamentally different way to describe it.

- Some examples to illustrate uses of find.  These are very easy
  to generate using an interpreter:

   Data.List> find odd [1..10]     -- 1 is the first odd # in the list
   Just 1
   Data.List> find (>9) [1..10]    -- 10 is the (only) # > 9 in the list
   Just 10
   Data.List> find (<0) [1..100]   -- no negative #s in this list
   Nothing
   Data.List> find even []         -- no #s at all in this list!
   Nothing
   Data.List> 

--------------------------------------------------------------------

> find      :: FilePath -> IO [FilePath]
> find path  = doesDirectoryExist path >>= \isDir ->
>              case isDir of
>                True  -> getDirectoryContents path
>                         >>= inIO (filter notDotDot)
>                         >>= mapM (find . (path </>))
>                         >>= inIO ((path :) . concat)
>                False -> return [path]
>  where notDotDot name = name /= "." && name /= ".."

--------------------------------------------------------------------
QUESTION 2:  Explain briefly how the code for find works, and
comment on the need for the local definition of notDotDot.  [Hint:
if you're not sure why that is needed, you could always try
commenting out the relevant section of the code and then see what
happens.  Hint 2: Remember that you can stop a long-running
computation by hitting ^C (control C) ...]
--------------------------------------------------------------------

The definition of find given above checks to see if the given
folder is a directory; if not, it assumes that the path
corresponds to a file and returns that name as a singleton.  (In
that case, perhaps it should actually check to see if the file
exists, returning an empty list if not --- but I decided to keep
the definition simple.)  On the other hand, if the input path
names an existing directory, then we set up a pipeline that
builds a list of all the entries in the directory, recursively
searches those items, and concatenates the list of all found
paths together as a single list.

- The (path </>) section is used to convert the name of a file
  in the given directory into (path </> name), which provides
  a full path name for the directory item.

- The (path :) section ensures that we include the current
  path in the output of find, in addition to any files or
  folder that we find in the subdirectory.

- The notDotDot function is used to filter out the "." (current
  directory) and ".." (parent directory) items from the list of
  directory contents.  If we left those items in the list, then
  the implementation of find would not terminate: for example,
  searching a directory called foo would produce foo, foo/..,
  foo/../foo, foo/../foo/.., etc..., ad infinitum.

--------------------------------------------------------------------

> infixl >-

> (>-)   :: IO [FilePath] -> (FilePath -> IO Bool) -> IO [FilePath]
> g >- p  = g >>= filterIO p

> filterIO         :: (a -> IO Bool) -> [a] -> IO [a]
> filterIO p []     = return []
> filterIO p (x:xs) = do b <- p x
>                        if b then filterIO p xs >>= inIO (x:)
>                             else filterIO p xs

--------------------------------------------------------------------
QUESTION 3: The declaration "infixl >-" specifies that the ">-"
operator should be treated as a function that associates (or
groups) to the left.  Explain why this is necessary here.
[Hint: you might like to consider expressions of the form:
"find dir >- filt1 >- filt2".]
--------------------------------------------------------------------

When we say that ">-" associates to the left, we mean that an
expression of the form

   find dir >- filt1 >- filt2

is treated as if it had been written with parentheses:

   (find dir >- filt1) >- filt2

This makes sense from the perspective of the types that we have
for each expression: (find dir) has type IO [FilePath], and the
combination with filt1 produces another IO [FilePath], which is
what we need as the left hand argument for the second ">-", in
turn producing another IO [FilePath], and so on.  By the same
argument, this allows us to string together an arbitrary number
of filters without using parentheses, as in:

  find dir >- filt1 >- filt2 >- ... >- filtn

The alternative would be for >- to associate to the right, but
then the previous expression would be treated as if we'd written:

   find dir >- (filt1 >- filt2)

and that doesn't work from a typing perspective because filt1
is a filter expression, so it doesn't have the right type to be
used on the left of >-.

Technically, there is a third alternative: to insist that >- does
not associate to the left or the right.  This, however, would force
us to use parentheses whenever there is more than one filter, which
would just make using the library more awkward.

--------------------------------------------------------------------

> name       :: (FilePath -> Bool) -> FilePath -> IO Bool
> name p f    = return (p f)

> haskellFiles = name ("hs" `isSuffixOf`)

--------------------------------------------------------------------
QUESTION 4: Show how the expression above can be modified to verify
that all of the FilePaths in the final list do indeed, have an even
number of characters in their name.
--------------------------------------------------------------------

The expression in question is:

  find "." >- name (even . length) >>= mapM_ putStrLn

A simple way to modify this is to replace the mapM_ putStrLn call
with a function that will print True if, and only if, all of the
generated file paths have even length.  One reasonable candidate
for this is:

  print . all (even . length)

Or, if you're not familiar with (or didn't think of) all:

  print . and . map (even . length)

Here's a quick test:

  HW5> find "." >- name (even . length) >>= print . all (even . length)
  True

  HW5> find "." >- name (even . length) >>= print . length
  160

  HW5> 

Notice that I threw in the second test just to make sure that the
input list wasn't empty; it wouldn't be much of a test if there were
no names to consider.  But, far from empty, my particular result
shows that I have 160 different paths to consider, and the line
before that confirms that they all have even length ... which is
just what we wanted!

--------------------------------------------------------------------

> size    :: (Integer -> Bool) -> FilePath -> IO Bool
> size p f = do b <- doesFileExist f
>               if b then do h <- openFile f ReadMode
>                            l <- hFileSize h
>                            hClose h
>                            return (p l)
>                    else return False

--------------------------------------------------------------------
QUESTION 5: Define a function of the following type:

  contents    :: (String -> Bool) -> FilePath -> IO Bool
  contents p f = undefined --- replace with your own code

Such that contents p f is a filter that will only keep a given file
if its contents satisfies the predicate p.
--------------------------------------------------------------------

There are a few ways to go about this, but if you follow the hint
in the question about using readFile, and if you mimic the definition
of size (which is what I had in mind when I gave you that definition),
then you'll end up with something like the following:

> contents    :: (String -> Bool) -> FilePath -> IO Bool
> contents p f = do b <- doesFileExist f
>                   if b then readFile f >>= inIO p
>                        else return False

Note that I've used an IO action pipeline here in the case where
b is True; you could also have used a do expression if you wanted,
something like:

  do s <- readFile f
     return (p s)

We could also go to a more elaborate solution that looks closer
to the code in the definition of size, using a do expression of
the form:

  do h <- openFile f ReadMode
     s <- hGetContents h
     let r = p s
     seq r (hClose h)
     return r

This version, however, requires a subtle detail: the call to seq,
which forces the evaluation of r before the file is closed.  We
haven't discussed this in class, which is why I hinted instead at
the use of readFile in the original question!

--------------------------------------------------------------------

> display  :: FilePath -> IO Bool
> display f = do putStrLn f
>                return True

--------------------------------------------------------------------
QUESTION 6: Define a filter command of the following type that
allows for user interation in a find command:

  queryUser  :: String -> IO Bool
  queryUser s = undefined --- replace with your own code!
--------------------------------------------------------------------

The following definition will suffice:

> queryUser  :: String -> IO Bool
> queryUser s = do putStr s
>                  putStr " (y/n)? "
>                  l <- getLine
>                  return (not (null l) && head l == 'y')

The last line that I've used in this definition is a little
clunky: I need to check that the string is not null before I
use head to inspect its head.  One alternative way to write
that last line is as follows:

  return (head (l++"n") == 'y')

which is shorter but a bit of a hack.  A more elegant approach
would be to write:

  return ("y" `isPrefixOf` l)

which takes advantage of the fact that Data.List is exported from
the IOActions module at the top of the file.

As for testing ... You've already seen what this looks like on my
machine; I included a sample run in the question.  But here's
another run in which I gave different answers to the (y/n)
questions:

  HW5> find "." >- haskellFiles >- size (<400) >- queryUser >- display
  /Users/user/fun/defs.lhs (y/n)? n
  /Users/user/fun/mapexample.lhs (y/n)? y
  /Users/user/fun/pascal.lhs (y/n)? n
  /Users/user/fun/pathy.hs (y/n)? n
  /Users/user/fun/subst.lhs (y/n)? n
  /Users/user/fun/mapexample.lhs

  HW5>

I only said "y" to one file this time, and that's the only one that
shows up in the final list.

----------------------------------------------------------------------
