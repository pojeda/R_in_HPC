#!/bin/bash
#SBATCH -A SNIC2020-9-66
#Asking for 10 min.
#SBATCH -t 00:10:00
#SBATCH -n 56

ml GCC/8.2.0-2.31.1  OpenMPI/3.1.3
ml R/3.6.0 

mpirun -np 1 Rscript --vanilla parallel_multinode.R

