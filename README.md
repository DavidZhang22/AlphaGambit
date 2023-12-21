# AlphaGambit

Haskell implementation of search algorithms for chess. 

## Folder structure

`AlphaGambit` contains the core Haskell package, which builds to an executable. This folder contains its own README with installation instructions. 

The other files perform auxiliary functions based on the outputs of this executable: 

- `runtimes` contains runtime data for some runs of the AlphaGambit executable -- predicting a single move at various depths. 
- `sample_games` contains some contests between different pairs of algorithms.
- `visuals` contains Python scripts for generating