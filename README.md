## Free Monads and Probabilistic Programming

Towards understanding [Practical Probabilistic Programming with
Monads](http://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf)

1. Interpreting a syntax tree [Program](Program.hs)
2. Monad instance for the syntax data type: [Monad](Monad.hs)
3. Abstracting out the recursion: [Free](Free.hs)
4. Applying it to probabilistic programming: [Dist](Dist.hs)

### Idris Version

*Attempt* to port the above into Idris.

1. [Program](idris/Program.idr) - done
2. [Monad](idris/Mon.idr) - done
3. [Free](idris/Free.idr) - except runFree (and thus run). (iterFree and run' are implemented.) ([Free] (https://github.com/idris-hackers/idris-free/blob/master/Control/Monad/Free.idr) was helpful. I used some of it just with name changes.)
4. [Dist](idris/Dist.idr) - except runSample / histogram. runExact is implemented.

Several questions about the porting are in the code.

