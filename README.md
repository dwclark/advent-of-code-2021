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

## [Day 5](src/day-05.lisp) It's Not a Grid, It's a Map

One of the fundamental "tricks" in advent of code is recognizing when to use a map to represent a grid and when to use a 2-d array. For that you have to use a second "trick" which is look at the data itself. Does it look like a large and sparse grid? If yes, use a map. If not, use a grid. This one falls into the former category.

The code itself isn't that interesting. This was one of the few times (if I remember) that I used regular expressions to parse the input. Actually, checking AOC 2020, I used it about as much that year. In any case, I did use the lisp reader more heavily, just not today. In both parts the strategy is the same, loop through all of the lines, extract the points on the line as a list, and then increment the total number of lines crossing through that point by one. Finally, add up all of the intersections.

I did like the fact that I made little functions for working with each line type and then using generic fill/increment functions to mutate the map.

## [Day 23](src/day-23.lisp) The Hardest Day I Did on my Own

If there's one think I have learned from AOC, it's how to apply [Dijkstra's Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to least cost path problems. A lot of problems involve understanding the essence of the algorithm, applying the algorithm in its standard form doesn't cut it. What's the essence of Dijkstra's algorithm?

1. From your current position, compute the frontier
2. Order the frontier from least costly to most costly in a priority queue or heap.
3. Select the next least costly move in the frontier. If it is the finished state, stop. Otherwise go back to step 1.

Because you are always keeping an exact order of costs, and you are always evaluating the least costly move, you are guaranteed to arrive at the least costly finished state before arriving at more costly finished states. Day 23 complicates the algorithm in the following ways:

- There are multiple moving pieces, so you have to compute the frontier for each piece separately
- You need to deduplicate redundant frontiers and only add one to the frontier. For example a -> 1 then b -> 3 is the same as b -> 3 then a -> 1
- Determining the frontier for any single piece is difficult because of all of the fiddly rules that determine what a legal move is
- There are several ways to move to the same board state, each will have different costs. If you find a way that is cheaper, you will need to update the cost for that board state if it has already been submitted for analysis. Using a Fibonacci heap is the best way to do this.
- The concept of "visited" becomes significanly muddier

## [Day 24](src/day-24.lisp) Just Do It By Hand

First off, I didn't solve this day's at all. I ended up just following along with [Eric Burden's explanation](https://www.ericburden.work/blog/2022/01/05/advent-of-code-2021-day-24/). This is one of those cases where an exact solution arrived at analytically is the only way to do the problem. Searching through a 14 digit space is simply going to take way too long for even the fastest computers with the most optimized code. Eric's solution requires the following insights:

1. There are 14 blocks of code, each starting with the input instruction
2. A lot of the code ends up being local to the block itself, it never makes a difference outside of the local block
3. In fact, the only two things that can affect the current block are the previous z value and the current input
4. The only way to end up with z being zero at the end of block 14 is for some combination of `div` and `mod` operations on z
5. You have to do a bit of meta-programming, you have to write code that abstracts the code itself
6. The 14 blocks form 7 pairs of push/pop operations on a stack

The key is 6, which is something I never would have realized had I not had it pointed out to me. I understood 1-4 after looking at the input code for a while. However, my guess was that later blocks would only result in z being 0 if there was a narrow range of inputs for each block. I thought that only certain operations would be legal (no division or mod by zero), and that by caching the results of downstream operations I would be able to short circuit early on upstream operations. This would reduce the search space to a much smaller number, making a straightforward iterative solution work.

I also somewhat understood 5 to be the case, but thought it would involve some sort of code optimization, leading to the elminiation of legal inputs and blocks, which would effectively be a different way of reducing the search space not involving caching. However, I didn't see a way to make that workable.

It should go without saying that the code there probably _does_ work for testing inputs, but it's worthless for arriving at a solution.

## [Day 25](src/day-25.lisp) At Least It Finishes Easily

This is the second year I made it to the very end of an AOC. Both times now day 25 has been a single part and relatively easy. This is good because by the end of it I am always exhauseted.

Day 25 was a fairly standard AOC day. There's a grid of things. Things move around according to rules. You run the game until a condition happens. Then you make a computation based on the number of moves and/or the position of the things in the grid.
