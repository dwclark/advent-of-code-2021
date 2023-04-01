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

## [Day 1](src/day-01.lisp)

Pretty simple day 1 problem. For part one just use the CL sequence function `count-if` to count the number of increasing numbers. For part 2, re-use `count-if` strategy after forming a new list that sums the numbers in the sliding window. I award myself bonus points for not using the `LOOP` macro.
