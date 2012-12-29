# ba-winter-challenge

The BA-Winter-Challenge code was created to power the 
[Bike Arlington Freezing Saddles Winter Bike Challenge](http://bikearlingtonforum.com/showthread.php?3500-Freezing-Saddles-Winter-Bike-Challenge-(sign-up-open\)).
Given a list of Strava clubs, this program creates a competition scoreboard ala the [National Bike Challenge](http://nationalbikechallenge.org/) rules.

The competition results are being posted [here](/Users/ronwalf/src/ba-winter-challenge/html/).

## Requirements and installation.
BA-Winter-Challenge requires a recent copy of [Haskell](http://hackage.haskell.org/platform/), being most recently tested on Haskell Platform 2012.2.0.0, as well as a number of other libraries that are managed via [Cabal](http://www.haskell.org/cabal/).
Once Haskell is installed, use cabal to build and install ridemap:

    $ cd ba-winter-challenge
    $ cabal install --user

This installs one executable, `ba-winter-challenge`, which takes a path to a config file (see `ba-challenge.json` for an example), and outputs a directory (`html` in this exapmple) containing the scoreboard and some cache files.

    $ ba-winter-challenge path/to/config.json
    $ scp -Cr html/ remote.host:/path/to/map/dir/

There is one relevant command line option:
    -c : Clear the cache of rides.  This should only be necessary if people change the ride info for older rides.

 
