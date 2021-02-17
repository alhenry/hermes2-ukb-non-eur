pacman::p_load(data.table, tidyverse)

UKB_pheno <- fread("data/hermes2_ukb_pheno.tsv", key="eid")
ancestry_info <- fread("data/ukb_noneur_sr_vs_pc_derived_ethnicity.txt", key="ID1_9922")

ancestry_info[gen_ethnic == "black", gen_ethnic := "AFR"]
anc_groups <- c("AFR", "EAS", "SAS")

UKB_pheno[ancestry_info[gen_ethnic %in% anc_groups], nomatch=0L] %>%
  as_tibble %>%
  pivot_longer(cols = starts_with("pheno"), names_to = "pheno") %>%
  group_by(gen_ethnic, pheno, value) %>%
  tally %>%
  pivot_wider(names_from = value, values_from = n)

# results:
# N case AFR_pheno1 = 142, AFR_pheno2 = 86
# N case SAS_pheno1 = 241, SAS_pheno2 = 76
# other pheno / ancestry have very few N - not ideal for GWAS
