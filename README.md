# Advent of Code 2021 

## Overview

This repo contains solutions to [Advent of Code 2021](https://adventofcode.com/2021) done in Common Lisp. Each day is in its own file and defined in its own package. Doing it this way makes every day self contained and easy to work on. Load the system using quicklisp. You will also need to tell lisp where to find the inputs for each day. They are in the inputs directory. For example:

```Common Lisp
(ql:quickload "advent-of-code-2021")
(setf utils:*input-directory* "/my/development/directory/advent-of-code-2020/inputs/")
```

Change the path to match what's on your machine.

Each day is in its own package called `day-n`, replace n with the day number. Each day also has two functions which show the solutions for that day named `part-1` and `part-2`. So if you want to see the solutions for day 1:

```Common Lisp
(in-package :day-1)
(part-1)
(part-2)
```

Run all of the above commands inside your common lisp environment. And your common lisp environemnt SHOULD be a properly set up to use SLIME. The easiest way to do that is to load [quicklisp](https://www.quicklisp.org/beta/) and then let quicklisp set everything up for you.

## At least I finally did finish it (On March 31, 2023)

This one took a long time to finish. In 2021 I was following along pretty well. As usual, in the later days, I fell behind a little. I made it through day 22 and was working on day 23 when I started getting back pain (probably from sitting at the computer too much). I put advent of code to the side to rest my back. A day or two after Christmas I got COVID. Even worse, the COVID made the back pain come back with a vengeance. I'm guessing the general inflamation caused by the virus cause any tissues already somewhat inflamed to go into overdrive. After that followed a long recovery and I never got back into AOC 2021.

A couple of weeks back I started working, slowly, on day 23. It was a pretty hard day for me, though I was able to finally finish it without any code helps. Hopefully, I can solve any hard problem involving Dijkstra's algorithm for least cost paths going forward. At least I know to use a Fibonacci heap/queue from the start. Day 24 was killer, more below. Day 25 was easy, just like it was in 2020.

## [Day 1](src/day-01.lisp) Generic Sequence Functions

Pretty simple day 1 problem. For part one just use the CL sequence function `count-if` to count the number of increasing numbers. For part 2, re-use `count-if` strategy after forming a new list that sums the numbers in the sliding window. I award myself bonus points for not using the `LOOP` macro.

## [Day 2](src/day-02.lisp) Use Symbolic Computation

One goal this year was to make better use of the lisp reader to a) parse files more easily and b) have the parsed output be either directly executable or executable with some form of a case statement. Day 2 used the latter approach. The reader parses each line directly to be `symbol number` which is fed into the simple lambdas in parts 1 and 2. Pretty happy with how simple and straightfoward it made this day. More bonus points for no `LOOP` macro.

## [Day 3](src/day-03.lisp) Recursion and bit-twiddling

Like I said, I was really trying to avoid `LOOP` in 2021. I later gave up on it, but early on, it was fun. This day used recursion to avoid looping, thus the code probably reads more like scheme than common lisp. Day 1 uses recursion to gradually build up binary numbers using `logior`. Day 2 continues filtering out list entries until it finds a single remaining value and then returns.

One upside to doing it this way is that there is only one `setf` and so most of the functions are, well, functional. And the only setf is used to create the initial data that is then passed on to functions for analysis. This also made the code quite elegant. However, after over a year of not looking at the code, it wasn't obvious what it was doing. I'm not sure this is actually an argument for mutable state; I'm not sure a loop mutating variables would actuall _be_ any better.

## [Day 4](src/day-04.lisp) OK, I Guess I Decided to Use Loop

I guess day 4 was the day I decided to give up and start using loops. It was easiest to represent each bingo card as a multi-dimensional array, which meant looping across indices, which meant `loop` was more natural than other constructs. I think the way I used to know if a bingo card was winning was pretty clever. First precompute all of the rows and colums that would represent a winning card. This means that the `winner-p` function was very simple: search the precomputed rows and colums to see if any of them had every entry set, if so, the bingo card wins.

**v2 Addendum** In the past I have done extensive re-writes of days of advent of code. I don't intend to do that this year, but this day did seem to start out as an attempt to not use loops. Returning to this day made me realize that I _wasn't_ using multi-dimensional arrays so `loop` wasn't as needed. [I was able to re-write it](src/day-04-no-loops.lisp) with no loops. It turned out a little bit clearer and shorter to read. I also replaced the correct, but confusing, uses of `remove-if-not` with the more idiomatic `find-if`. I replaced a few of the loops using `iota` from the [always useful Alexandria library](https://alexandria.common-lisp.dev/draft/alexandria.html). Pretty useful if you have a small number of indices to iterate or map across.
