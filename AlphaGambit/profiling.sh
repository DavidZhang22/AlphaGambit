#!/bin/bash

# Define the Haskell executable name
executable="AlphaGambit-exe"

# Function to run the executable with given parameters and append to the appropriate CSV file
run_and_record() {
    mode=$1
    depth=$2
    csv_file=$3

    # Using 'time' to measure the execution time
    start_time=$(gdate +%s.%N)
    stack exec --silent $executable $mode $depth
    end_time=$(gdate +%s.%N)
    
    # Calculate duration
    duration=$(echo "$end_time - $start_time" | bc)

    # Append to the respective CSV file
    echo "$mode,$depth,$duration" >> $csv_file
}

# Headers for CSV files

modes=("parallel sequential alpha_beta jamboree")

mkdir -p new_runtimes

for mode in $modes; do
    echo "mode,depth,duration" > new_runtimes/${mode}.csv
done

# Run the executable once to warm it up
stack exec --silent $executable parallel 1

# Loop through all combinations of mode and depth
for mode in $modes; do
    for depth in {1..5}; do
        run_and_record $mode $depth new_runtimes/${mode}.csv
    done
done