
# Analyzing data:_______________________________________________________________
## All the functions used in this session are in the “functions.R” script


## 1) Frequency of inpatient records and their proportions:

aih_year <- tab_freq(dados_sih_neo, ANO_CMPT) # By year
aih_reg <- tab_freq(dados_sih_neo, Regiao) # By region
aih_icu <- tab_freq(dados_sih_neo, USO_UTI) # Whether or not the inpatient record was admitted to an ICU
aih_death <- tab_freq(dados_sih_neo, MORTE) # Whether or not the inpatient record led to death
aih_cid <- tab_freq(dados_sih_neo, DIAG_PRINC) # By diagnosis (CID-10)
  aih_cid <- aih_cid[aih_cid$DIAG_PRINC %in% top20_cid, ]


# Frequency of inpatient records and their proportions grouped 

cid_reg <- tab_freq_group(dados_sih_neo, Regiao, DIAG_PRINC) # Frequency of inpatient records by diagnosis (CID-10) by region
  cid_reg <- cid_reg[cid_reg$DIAG_PRINC %in% top20_cid, ]

death_year <- tab_freq_group(dados_sih_neo, ANO_CMPT, MORTE) # Number of deaths per year
death_reg <- tab_freq_group(dados_sih_neo, Regiao, MORTE) # Number of deaths per region
  
# Number of deaths per thousand inpatient records grouped by region of hospitalisation
n_reg <- c("Centro-Oeste" = 359859,
           "Nordeste" = 1061999,
           "Norte" = 346370,
           "Sudeste" = 1528833,
           "Sul" = 538067)

death_reg_inp <- death_reg %>%
  mutate(deaths_per_inpatient_care = 1000 * (n / n_reg[Regiao]))

# Number of deaths per thousand inpatient records grouped by year of hospitalisation
n_year <- c("2011" = 251856,
            "2012" = 257617,
            "2013" = 271044,
            "2014" = 278585,
            "2015" = 295822,
            "2016" = 307688,
            "2017" = 335113,
            "2018" = 350583,
            "2019" = 361864,
            "2020" = 356415,
            "2021" = 380832,
            "2022" = 387709)

death_year_inp <- death_year %>%
  mutate(deaths_per_inpatient_care = 1000 * (n / n_year[ANO_CMPT]))




# Which health condition has the most records admitted to an ICU?
icu_cid <- dados_sih_neo %>%
  filter(USO_UTI == "Yes") %>%
  group_by(DIAG_PRINC) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prop = round(prop.table(n) * 100, 3))




## Hospital stay/days:

# Total length of stay (days)
length_stay <- descriptive_uniq(dados_sih_neo, DIAS_PERM)

# ICU length of stay (days)
length_stay_icu <- descriptive_uniq_icu(dados_sih_neo, UTI_MES_TO)




#_______________________________________________________________________________




## 2) Cost analysis

info_val_tot <- descriptive_uniq(dados_sih_neo, val_tot_corrigido) # Information on the Inpatient Cost (total) in R$
info_val_icu <- descriptive_uniq_icu(dados_sih_neo, val_uti_corrigido) # Information on the ICU Cost in R$

info_val_tot <- descriptive_uniq(dados_sih_neo, val_tot_int) # Information on the Inpatient Cost (total) in Int$
info_val_icu <- descriptive_uniq_icu(dados_sih_neo, val_uti_int) # Information on the ICU Cost in Int$


# Cost analysis grouped

  # By year
tot_year <- descriptive(dados_sih_neo, ANO_CMPT, val_tot_corrigido) 
tot_year <- descriptive(dados_sih_neo, ANO_CMPT, val_tot_int)
icu_year <- descriptive_icu(dados_sih_neo, USO_UTI, ANO_CMPT, val_uti_corrigido) 
icu_year <- descriptive_icu(dados_sih_neo, USO_UTI, ANO_CMPT, val_uti_int)

  # By region
tot_reg <- descriptive(dados_sih_neo, Regiao, val_tot_corrigido) 
icu_reg <- descriptive_icu(dados_sih_neo, USO_UTI, Regiao, val_uti_corrigido)
tot_reg <- descriptive(dados_sih_neo, Regiao, val_tot_int)
icu_reg <- descriptive_icu(dados_sih_neo, USO_UTI, Regiao, val_uti_int)

  # By diagnosis (CID-10)
tot_cid <- descriptive(dados_sih_neo, DIAG_PRINC, val_tot_corrigido)
tot_cid <- descriptive(dados_sih_neo, DIAG_PRINC, val_tot_int)

icu_cid <- descriptive_icu(dados_sih_neo, USO_UTI, DIAG_PRINC, val_uti_corrigido)
icu_cid <- descriptive_icu(dados_sih_neo, USO_UTI, DIAG_PRINC, val_uti_int)

# Creating a variable with the 20 CIDs with the highest total cost that should be kept in the table when called up
top20_cid <- c('P073', 'P220', 'P229', 'P228', 'P072', 'P285', 'A419', 'P071', 'P599', 'P221', 'P050',	'P210',	'P219',	'P289',	'P369',	'P200',	'P070',	'P399',	'P051',	'A499')

# Keeping only the top 20 CIDs with the highest total cost
tot_cid <- tot_cid[tot_cid$DIAG_PRINC %in% top20_cid, ] 
icu_cid <- uti_cid[uti_cid$DIAG_PRINC %in% top20_cid, ]



# Cost of each diagnosis (CID-10) per thousand live births grouped by region of hospitalisation

cost_cid_reg <- dados_sih_neo %>%
  group_by(DIAG_PRINC, Regiao) %>% 
  summarise(cost = sum(val_tot_corrigido)) # Also run with "val_tot_int"

cost_cid_reg <- cost_cid_reg[cost_cid_reg$DIAG_PRINC %in% top20_cid, ] 

# Number of live births in the public system by region
live_births <- c(
  "Centro-Oeste" = 1164323,
  "Nordeste" = 5244313,
  "Norte" = 2284373,
  "Sudeste" = 4341985,
  "Sul" = 910109)

# Adding the column with the cost per 1000 live births by region
cost_cid_reg <- cost_cid_reg %>%
  mutate(val_per_live_births = 1000 * (cost / live_births[Regiao]))

