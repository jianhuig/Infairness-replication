#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=60
#SBATCH --time=0-00:15           # time (DD-HH:MM)
#SBATCH --mem=120G

module load r/4.5.0
R CMD BATCH --no-save --no-restore quick_sim.R