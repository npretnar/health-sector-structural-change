#!/bin/bash

# Base directory containing all the subdirectories
base_dir="data/analysis"

# List of subdirectories
dirs=(
  "GE_Calibration_Mu_Growth_Past_2015"
  "GE_Calibration_Constant_Alpha_c"
  "GE_Calibration_Variable_Alpha_c"
  "GE_Calibration_HS_Markups_Full_Calibration"
  "GE_Calibration_No_Mu_Growth"
  "GE_Calibration_HS_Markups_Sim_Only"
)


# Loop through each subdirectory
for dir in "${dirs[@]}"; do
  full_path="$base_dir/$dir"
  echo "Entering $full_path"

  if [ -d "$full_path" ]; then
    cd "$full_path" || { echo "Failed to enter $full_path"; continue; }

    # Compile MAIN.f90 to produce 'health' executable
    gfortran -o health MAIN.f90 -fopenmp -ffpe-trap=zero,invalid,overflow,underflow \
             -L/usr/local/lib -lblas -llapack

    # Check if compilation succeeded
    if [ -f "health" ]; then
      echo "Compiled 'health' successfully in $dir"
      ./health .FALSE. .TRUE. 1234567
    else
      echo "Compilation failed in $dir"
    fi
    
    # execute R script post-analysis within sub-directory
    Rscript "2_counterfactual_analyses.R"
    
    # Return to base directory
    cd - > /dev/null
  else
    echo "Directory $full_path does not exist"
  fi
done
