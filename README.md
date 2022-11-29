

<p><img align="left" width="100"> <h1>Probabilistic SKnife</h1></p>
Probabilstic SKnife (ProbSKnife) is a declarative prototype to evaluate the expected cost of a partitioning expecting to change the initial labelling. For every possible labelling it calculates:

1. The probability of changing to the labelling

2. The eligible partitionings that satisfy the labelling with the cost of migration from the starting partitionings

To calculate the expected cost, **ProbSKnife** is launched from a Python script that parse the results, groups them by the labelling minimum and aggregates by eligible partionings.

<br></br>
## Prerequisites

Before using **ProbSKnife** You need to install [ProbLog2](https://dtai.cs.kuleuven.be/problog/index.html) and Python.
## Tutorial

To try **ProbSKnife**:

1. Download or clone this repository.

2. Open a terminal in the project folder and run `python  .\main.py StartingPartitioning`.

3. The output is a table that resumes every reachable partitioning with its cost, its probability to be reached and the expected cost to reach it from the starting labelling.

   E.g. of a table record for the starting partitioning ```[((top, safe), [south, west]), ((top, safe), [east, north])]```
   ```
                                                                         partitionings  costs  probabilities  expectedCost
   0                    [((low, safe), [south, west]), ((low, safe), [east, north])]      0       0.125000       0.00000
   1                     [((low, safe), [south, west]), ((top, low), [east, north])]      0       0.031250       0.00000
   2    [((low, safe), [south, west]), ((top, low), [east]), ((top, safe), [north])]     20       0.031250       0.62500
   3                    [((low, safe), [south, west]), ((top, safe), [east, north])]      0       0.031250       0.00000
   4    [((low, safe), [south, west]), ((top, safe), [east]), ((top, low), [north])]     20       0.031250       0.62500
   5    [((top, low), [south, east]), ((top, safe), [west]), ((low, safe), [north])]     80       0.031250       2.50000
   6                     [((top, low), [south, west]), ((low, safe), [east, north])]      0       0.031250       0.00000
   7                      [((top, low), [south, west]), ((top, low), [east, north])]      0       0.023438       0.00000
   8     [((top, low), [south, west]), ((top, low), [east]), ((low, safe), [north])]     20       0.031250       0.62500
   9     [((top, low), [south, west]), ((top, low), [east]), ((top, safe), [north])]     20       0.023438       0.46875
   10                    [((top, low), [south, west]), ((top, safe), [east, north])]      0       0.023438       0.00000
   11   [((top, low), [south, west]), ((top, safe), [east]), ((low, safe), [north])]     20       0.031250       0.62500
   12    [((top, low), [south, west]), ((top, safe), [east]), ((top, low), [north])]     20       0.023438       0.46875
   13   [((top, low), [south]), ((top, safe), [west, east]), ((low, safe), [north])]     60       0.031250       1.87500
   14    [((top, low), [south]), ((top, safe), [west, east]), ((top, low), [north])]     60       0.023438       1.40625
   15    [((top, low), [south]), ((top, safe), [west, north]), ((top, low), [east])]     60       0.023438       1.40625
   16   [((top, low), [south]), ((top, safe), [west]), ((low, safe), [east, north])]     20       0.031250       0.62500
   17    [((top, low), [south]), ((top, safe), [west]), ((top, low), [east, north])]     20       0.023438       0.46875
   18   [((top, low), [south]), ((top, safe), [west]), ((top, safe), [east, north])]     20       0.023438       0.46875
   19   [((top, safe), [south, east]), ((top, low), [west]), ((low, safe), [north])]     80       0.031250       2.50000
   20                   [((top, safe), [south, west]), ((low, safe), [east, north])]      0       0.031250       0.00000
   21                    [((top, safe), [south, west]), ((top, low), [east, north])]      0       0.023438       0.00000
   22   [((top, safe), [south, west]), ((top, low), [east]), ((low, safe), [north])]     20       0.031250       0.62500
   23   [((top, safe), [south, west]), ((top, low), [east]), ((top, safe), [north])]     20       0.023438       0.46875
   24                   [((top, safe), [south, west]), ((top, safe), [east, north])]      0       0.023438       0.00000
   25  [((top, safe), [south, west]), ((top, safe), [east]), ((low, safe), [north])]     20       0.031250       0.62500
   26   [((top, safe), [south, west]), ((top, safe), [east]), ((top, low), [north])]     20       0.023438       0.46875
   27   [((top, safe), [south]), ((top, low), [west, east]), ((low, safe), [north])]     60       0.031250       1.87500
   28   [((top, safe), [south]), ((top, low), [west, east]), ((top, safe), [north])]     60       0.023438       1.40625
   29   [((top, safe), [south]), ((top, low), [west, north]), ((top, safe), [east])]     60       0.023438       1.40625
   30   [((top, safe), [south]), ((top, low), [west]), ((low, safe), [east, north])]     20       0.031250       0.62500
   31    [((top, safe), [south]), ((top, low), [west]), ((top, low), [east, north])]     20       0.023438       0.46875
   32   [((top, safe), [south]), ((top, low), [west]), ((top, safe), [east, north])]     20       0.023438       0.46875
   ```
   Finally, all the expected costs are aggregated to give the expected cost of a partitioning.
   ```
   The expected cost is 67.5
   ```
