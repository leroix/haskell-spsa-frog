# SPSA FROG

In situations where laser pulse-amplitude and -phase variations occur faster than available detectors can measure, 
Frequency-Resolved Optical Gating ([FROG](http://frog.gatech.edu/Pubs/Trebino-FROGreview-RSI-1997.pdf)) 
uses optical nonlinearities
to mix a laser pulse signal with a modified replica of itself. The resulting signal is then 
spectrally-resolved. By recording the spectrum at each delay, FROG gathers enough information
about the pulse to retrieve the pulse signal's amplitude and phase with a few, known, ambiguities.

Traditionally, FROG uses a generalized projection-based algorithm to retrieve the amplitude and phase.
Here, we experiment with replacing that algorithm with Simultaneous Perturbation Stocastic Approximation
 ([SPSA](http://jhuapl.edu/SPSA)). SPSA has strong theoretical foundations and is very simple to 
implement.

## Using SPSA FROG
Disclaimer: SPSA FROG is still very experimental and needs to be tuned.

`git clone https://github.com/leroix/haskell-spsa-frog`

The first time through, you need to download and install a ton of
dependencies, so hang in there.

    cd haskell-spsa-frog
    cabal-dev install \
        --enable-tests \
        --only-dependencies \
        --flags=developer
        -j

The `cabal-dev` command is just a sandboxing wrapper around the
`cabal` command.  The `-j` flag above tells `cabal` to use all of your
CPUs, so even the initial build shouldn't take more than a few
minutes.

```
cabal-dev configure --enable-tests --enable-benchmarks --flags=developer
cabal-dev build
```

Refer to the [tests](https://github.com/leroix/haskell-spsa-frog/blob/master/tests/Test/Test.hs) 
 to see how the functions in Math.FROG.Retrieval are used. It will be helpful
to understand the [haskell-SPSA module](http://hackage.haskell.org/package/spsa).


### Running Tests

Once you've built the code, you can run the entire test suite in a few
seconds.

```
dist/build/tests/tests +RTS -N
```

We use the direct executable rather than `cabal-dev tests` because it doesn't pass through options very well. The `+RTS -N` above tells GHC's runtime system to use all available cores. If you want to explore, the `tests` program (`dist/build/tests/tests`) accepts a `--help` option. Try it out.


#### Tests From GHCI

You can run all the tests from ghci.

```
cabal-dev ghci
```

starts up the REPL.

```
> import Test.Test
> runAllTests
```

Or you can run a single test

```
> runTest "Gaussian" -- follows test patterns in Test.Framework
> runGroup "Retrieval" -- if you want to run all tests in the "Retrieval" group
```


## Why Haskell?
http://www.haskell.org/haskellwiki/Why_Haskell_matters
