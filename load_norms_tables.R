

read_percentile_table <- function(file_name) {
  as.matrix(read_csv(here("cdi_benchmarks", file_name), skip = 1))
}

# Generate list of matrices for each available benchmark file
available_benchmarks <- list.files(here("cdi_benchmarks"))
norms_tables <- lapply(available_benchmarks, read_percentile_table)
names(norms_tables) <- available_benchmarks

saveRDS(norms_tables, file="cdi_benchmarks/norms_tables.rds")