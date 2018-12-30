# Market Data Pipeline

A small service that listens to a specified port and runs udp market data through a conduit pipeline, parses and outputs data to the command line or a file. 

## Getting Started

#### Build
This program is dependent on parser-gen 0.2.0.8, as of the time of writing this is not available on hackage. So first we need fetch it from github and add it to our cabal packages.
```
mkdir packages && cd packages
git clone https://github.com/tsurucapital/parsergen.git
cd parsergen
cabal install```
```
Go back to our main repo.
``` 
cabal install
```

#### Running
```
cabal run -- --help
```
The command line accepts a number of arguments as seen below:
```
  -r --reorder           Reorder records based on quote accept time
  -f --fileoutput        Output to files, if False (default), use
                         commandline.
     --hostaddress=ITEM  The binding address
     --hostport=INT      The binding port
  -? --help              Display help message
  -V --version           Print version information
     --numeric-version   Print just the version number
```

Start sending data over udp with the following specification.
```
Quote Packet Specification
    ITEM NAME                              len  Remark
    --------------------------------------+---+---------------
    Data Type                               2   B6
    Information Type                        2   03
    Market Type                             1   4
    Issue code                             12   ISIN code
    Issue seq.-no.                          3
    Market Status Type                      2
    Total bid quote volume                  7
    Best bid price(1st)                     5   Decimals
    Best bid quantity(1st)                  7   Decimals
    Best bid price(2nd)                     5
    Best bid quantity(2nd)                  7
    Best bid price(3rd)                     5
    Best bid quantity(3rd)                  7
    Best bid price(4th)                     5
    Best bid quantity(4th)                  7
    Best bid price(5th)                     5
    Best bid quantity(5th)                  7
    Total ask quote volume                  7
    Best ask price(1st)                     5
    Best ask quantity(1st)                  7
    Best ask price(2nd)                     5
    Best ask quantity(2nd)                  7
    Best ask price(3rd)                     5
    Best ask quantity(3rd)                  7
    Best ask price(4th)                     5
    Best ask quantity(4th)                  7
    Best ask price(5th)                     5
    Best ask quantity(5th)                  7
    No. of best bid valid quote(total)      5
    No. of best bid quote(1st)              4
    No. of best bid quote(2nd)              4
    No. of best bid quote(3rd)              4
    No. of best bid quote(4th)              4
    No. of best bid quote(5th)              4
    No. of best ask valid quote(total)      5
    No. of best ask quote(1st)              4
    No. of best ask quote(2nd)              4
    No. of best ask quote(3rd)              4
    No. of best ask quote(4th)              4
    No. of best ask quote(5th)              4
    *Quote accept time*                     8  HHMMSSuu
    End of Message                          1  0xff
```

#### Todo and future interests
    - Testing
    - Add Kafka sinks
    - Create an analysis service



