# prepare phenotype for non-EUR

library(data.table)
library(tidyverse)

DT_pheno <- fread("data/hermes2_ukb_pheno.tsv", key = "eid")
DT_ancestry <- fread("data/ukb_noneur_sr_vs_pc_derived_ethnicity.txt", key = "ID1_9922")
DT_rel <- fread("data/ukb_noneur_pcrelate_third_degree_relatives.txt", key = "ID1")
DT_sample <- fread("data/ukb-gen-imp-raw.sample", skip = 2,
                   col.names = c("ID_1", "ID_2", "missing", "sex"),
                   key = "ID_1")

DT_pheno[DT_ancestry, gen_ethnic := i.gen_ethnic]
df_pheno_subset <- DT_pheno[!is.na(gen_ethnic)][DT_sample[, .(ID_1)], nomatch=0L] %>% 
  as_tibble %>% 
  mutate(across(starts_with("pheno"),
                ~recode(.x, case = 1, control = 0, exclude = NA_real_)),
         gen_ethnic = recode(gen_ethnic, black = "AFR"))

# prioritized sample from case subset
eid_cases <- df_pheno_subset %>% 
  filter(rowSums(across(starts_with("pheno"), ~.x == 1)) > 0)

set.seed(999)
exclude <- c()
for (i in 1:nrow(DT_rel)){
  ids <- c(DT_rel[i,ID1], DT_rel[i,ID2])
  if (!any(ids %in% exclude)){
    if (ids[1] %in% eid_cases) {
      exclude <- c(exclude, ids[2])
    } else {
      exclude <- c(exclude, sample(ids, 1))
    }
  }
}

df_pheno_subset <- df_pheno_subset %>% 
  filter(!eid %in% exclude)

# write sample file
write_sample <- function(ancestry){
  df_pheno_subset %>%
    filter(gen_ethnic == ancestry) %>% 
    select(`#FID` = eid, IID = eid) %>% 
    write_tsv(glue::glue("data/{ancestry}_unrel_sample.plink2"))
}
write_sample("AFR")
write_sample("SAS")