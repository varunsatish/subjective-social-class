#!/bin/bash
#SBATCH --job-name=sc_coll_pred      # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks=1               # total number of tasks across all nodes
#SBATCH --cpus-per-task=1        # cpu-cores per task (>1 if multithread tasks)
#SBATCH --mem-per-cpu=30G         # memory per cpu-core (4G is default)
#SBATCH --time=60:00:00          # total run time limit (HH:MM:SS)
#SBATCH --mail-type=begin        # send email when process begins
#SBATCH --mail-type=fail         # send email if job fails
#SBATCH --mail-type=end          # send email when job ends
#SBATCH --mail-user=vs3041@princeton.edu

module purge
module load R/4.3.0  # use 4.3.1 on della

Rscript finding_collinear_predictors.R
