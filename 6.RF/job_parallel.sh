#!/bin/bash
#SBATCH -A SNIC2019-5-156 
#Asking for 10 min.
#SBATCH -t 00:20:00
#SBATCH -N 1
#SBATCH -c 12

ml GCC/8.2.0-2.31.1  OpenMPI/3.1.3
ml R/3.6.0 

Rscript --vanilla parallel.R

