
# Analyzing data:_______________________________________________________________
## All the functions used in this session are in the “functions.R” script


## 1) Frequency of inpatient records and their proportions:

aih_year <- tab_freq(dados_sih, ANO_CMPT) # By year
aih_reg <- tab_freq(dados_sih, Regiao) # By region
aih_icu <- tab_freq(dados_sih, USO_UTI) # Whether or not the inpatient record was admitted to an ICU
aih_death <- tab_freq(dados_sih, MORTE) # Whether or not the inpatient record led to death
aih_type <- tab_freq(dados_sih, VIA_PARTO) # By type of delivery


# Frequency of inpatient records and their proportions grouped 

icu_type <- tab_freq_group(dados_sih, VIA_PARTO, USO_UTI) # Number of ICU admissions for each type of delivery
death_year <- tab_freq_group(dados_sih, ANO_CMPT, MORTE) # Number of deaths per year
death_reg <- tab_freq_group(dados_sih, Regiao, MORTE) # Number of deaths per region


# Number of deaths per 100,000 inpatient records grouped by region of hospitalisation
n_reg <- c("Centro-Oeste" = 1758894,
           "Nordeste" = 7507070,
           "Norte" = 2807864,
           "Sudeste" = 8129446,
           "Sul" = 2932493)

death_reg_inp <- death_reg %>%
  mutate(deaths_per_inpatient_care = 100000 * (n / n_reg[Regiao]))

# Number of deaths per 100,000 inpatient records grouped by year of hospitalisation
n_year <- c("2011" = 1948403,
           "2012" = 1886902,
           "2013" = 1894324,
           "2014" = 1921625,
           "2015" = 1986282,
           "2016" = 1919829,
           "2017" = 1975248,
           "2018" = 2025997,
           "2019" = 1994444,
           "2020" = 1905720,
           "2021" = 1884170,
           "2022" = 1792823)

death_year_inp <- death_year %>%
  mutate(deaths_per_inpatient_care = 100000 * (n / n_year[ANO_CMPT]))



## Hospital stay/days:

# Total length of stay (days)
length_stay <- descriptive_uniq(dados_sih, DIAS_PERM)
length_stay_type <- descriptive(dados_sih, VIA_PARTO, DIAS_PERM) # Length of stay for each type of delivery

# ICU length of stay (days)
length_stay_icu <- descriptive_uniq_icu(dados_sih, UTI_MES_TO)
length_stay_type <- descriptive_icu(dados_sih, USO_UTI, VIA_PARTO, UTI_MES_TO) # ICU Length of stay for each type of delivery



#_______________________________________________________________________________




## 2) Cost analysis

info_val_tot <- descriptive_uniq(dados_sih, val_tot_corrigido) # Information on the Inpatient Cost (total) in R$
info_val_icu <- descriptive_uniq_icu(dados_sih, val_uti_corrigido) # Information on the ICU Cost in R$

info_val_tot <- descriptive_uniq(dados_sih, val_tot_int) # Information on the Inpatient Cost (total) in Int$
info_val_icu <- descriptive_uniq_icu(dados_sih, val_uti_int) # Information on the ICU Cost in Int$


# Cost analysis grouped

# By year
tot_year <- descriptive(dados_sih, ANO_CMPT, val_tot_corrigido) 
tot_year <- descriptive(dados_sih, ANO_CMPT, val_tot_int)
icu_year <- descriptive_icu(dados_sih, USO_UTI, ANO_CMPT, val_uti_corrigido) 
icu_year <- descriptive_icu(dados_sih, USO_UTI, ANO_CMPT, val_uti_int)

# By region
tot_reg <- descriptive(dados_sih, Regiao, val_tot_corrigido) 
icu_reg <- descriptive_icu(dados_sih, USO_UTI, Regiao, val_uti_corrigido)
tot_reg <- descriptive(dados_sih, Regiao, val_tot_int)
icu_reg <- descriptive_icu(dados_sih, USO_UTI, Regiao, val_uti_int)

# By type of delivery
val_tot_type <- descriptive(dados_sih, VIA_PARTO, val_tot_corrigido)
val_icu_type <- descriptive_icu(dados_sih, USO_UTI, VIA_PARTO, val_uti_corrigido) 
val_tot_type <- descriptive(dados_sih, VIA_PARTO, val_tot_int)
val_icu_type <- descriptive_icu(dados_sih, USO_UTI, VIA_PARTO, val_uti_int)


