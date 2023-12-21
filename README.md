# AlphaGambit

## Building the project

First, make sure you have Stack installed. Then, run `stack build` in the root directory. This will create an executable called `AlphaGambit-exe`. 

## Running the project

To simulate the choice of a single move, run `stack exec AlphaGambit-exe <search_type> <depth>`.

For example:
```bash
# Run parallel minimax at depth 5
stack exec AlphaGambit-exe parallel 5 

# Run alpha-beta pruning at depth 5
stack exec AlphaGambit-exe alpha_beta 5 

# Run sequential minimax at depth 4
stack exec AlphaGambit-exe sequential 5 
```

This command is useful for comparing methods at the single-move level (e.g. for measuring runtime and profiling). 

To watch a sample game get played, you can watch two instances of the parallelized minimax algorithm play each other by running:

```bash
stack exec AlphaGambit-exe play  
```