# for running in the terminal
# using Rscript + argument
# path as argv[1]

# Rscript --vanilla batch.R /home/project/

# output: stdout

argv <- commandArgs(TRUE)

if (length(argv) == 1) {
  path <- argv[1]
} else {
  path <- ""
}

name <- "surv_input_data.RData"

file_path <- paste(path, name, sep = '')

data <- load(file_path)

# run parallel_mainTx.R
