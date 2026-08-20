library(TwoSampleMR)
library(ieugwasr)

#getdatafrom_https://api.opengwas.io/profile/
exposure_data <- extract_instruments(
  outcomes = "ebi-a-GCST90029012",
  p1 = 5e-08,
  clump = TRUE,
  r2 = 0.001,
  kb =10000,
)


outcome_data <- extract_outcome_data(
  outcomes = "ieu-b-2",
  snps = exposure_data$SNP,
  proxies = FALSE,
  maf_threshold = 0.01,
)

data <- harmonise_data(exposure_dat = exposure_data, outcome_dat = outcome_data)


result <- mr(data)