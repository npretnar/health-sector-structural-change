#!/bin/bash

# shell script to run data cleaning and analysis programs
# scvript requires a bash or zsh shell architecture found in 
# Linux or MacOS

# this script should be executed in the directory ...
# ~/replication_package_pretnar_feldman_2025

### script starts here ###
# clear the results folder
rm -rf results/*

# set up the R environment #
Rscript "main_code/0_setup.R"

# run the initial data cleaning and analysis script in R #
Rscript "main_code/1_data_work.R"

# Prompt the user
echo ""
echo ""
echo ""
echo ""
echo ""
echo "You may choose either to re-run the whole Monte Carlo estimator or simply reproduce tables and figures in the paper from provided files."
echo "Please NOTE that running the whole Monte Carlo estimator requires at least 10 independent parallel threads (cores) and will take 7-10 days to complete!"
echo "If you just want to affirm that tables and figures can be produced from provided analysis data, type 0."
echo "Do you want to re-run the whole Monte Carlo estimator? If 'yes', type 1. If 'no', type 0."
read -p "Your choice: " choice

# Check user input
if [ "$choice" = "1" ]; then
    echo "You chose to re-run the whole estimator, which may take 7-10 days!"
    # Code line X goes here
    echo "Executing Code Monte Carlo run ..."
    chmod u+x "main_code/2_estimate_model.sh"
    bash main_code/2_estimate_model.sh
    
    # run the simulator for the HS appendix sims
    cp "data/analysis/GE_Calibration_Constant_Alpha_c/parms_min.txt" "data/analysis/GE_Calibration_HS_Markups_Sim_Only"

	# navigate to directory and rerun 
	cd "data/analysis/GE_Calibration_HS_Markups_Sim_Only"
	Rscript "2_counterfactual_analyses.R"
	
	# return
	cd - > /dev/null

else
    echo "You chose to simulate the estimator, which should take <30 seconds."
    # Code line Y goes here
    echo "Executing Code Model Simulation ..."
    chmod u+x "main_code/2_simulate_model.sh"
	bash main_code/2_simulate_model.sh
fi

# make parameter plots and calibration tables #
Rscript "main_code/3_parameter_plots.R"

