# bankSimu

official solution at secret gist:
https://gist.github.com/2jacobtan/1dbd89de5a003f83f9c92ba9bb13d1d0

---

use [bankSimu.hs](https://github.com/2jacobtan/bankSimu/blob/master/bankSimu.hs)

(run the "main" function)

Assignment from:

https://www.notion.so/Bank-Simulation-94b50cdebe0b4da1b1297a7b01744682

Annotated screenshot:

https://github.com/2jacobtan/bankSimu/blob/master/Screenshots%20of%20assignment/screenshot%20(annotated).png

# Notes:
The statistical distribution of "time until next customer arrival" was given as a cumulative distribution function, which cannot be used with slice sampling (which requires a probability density function). Hence, I took the derivative of the CDF to get the PDF.

I also manipulated the equation for the PDF, to get the inverse function of PDF, for use in slice sampling algorithm. 

Slice sampling algorithm was hand-coded.

Refer to https://www.desmos.com/calculator/gqdoaxeo4i for graphs of CDF and PDF.

Edit: slice sampling was technically not the right approach. Inverse Transform Sampling is what I should have used on the CDF.

# Other thoughts:
Have never encountered such a problem before, and took a while to explore different possible ways of solving the problem.

Some kind of brute-force algorithm may have sufficed, but I wanted to write an algorithm that was faster and more elegant.

Considered using existing slice sampling module, e.g. https://hackage.haskell.org/package/speedy-slice, but since the PDF was strictly monotonic, I could code a much faster algorithm using the inverse PDF function.

*** should have used record syntax, but I was lazy and not familiar with using it in Haskell when I did this assignment :(

Code for the two tasks could be merged, but I wanted to show the process of coming up with the solution, instead of "erasing the working".

# GHCI (REPL) output:
![GHCI outut](https://github.com/2jacobtan/bankSimu/blob/master/Screenshots%20of%20assignment/GHCI%20(REPL)%20output.PNG)

Simulating 10,000 customers for each task. Code runs in less than 3 seconds on GHCI interpreter.
