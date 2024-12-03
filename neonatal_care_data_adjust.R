# Loading packages
library(tidyverse)
library(microdatasus)
library(read.dbc)
library(readxl)
library(foreign)
library(dplyr)
library(psych)


# 1) Downloading the database of neonatal inpatient care from a local computer file
dados_sih_neo <- read_csv("dados_sih_neo.csv")


# 2) Selecting the variables of interest, according to their name in the database
dados_sih_neo <- dados_sih_neo |> select(c("ANO_CMPT", "DT_INTER", "DT_SAIDA", "QT_DIARIAS", "DIAS_PERM", "UTI_MES_TO", "UTI_INT_TO", "NASC", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "MUNIC_MOV", "IDADE", "COD_IDADE", "MORTE"))



# 3) Adjusting the database for future analyses

# Adding a column indicating whether or not that inpatient record was admitted to an ICU
dados_sih_neo$USO_UTI=ifelse(dados_sih_neo$MARCA_UTI %in% c("00"), c("No"), "Yes")


# Modifying the "death" (in Portuguese - "morte") column indicating whether or not that inpatient record led to death
dados_sih_neo$MORTE=ifelse(dados_sih_neo$MORTE %in% c("1"), c("Death"), "No")


# Creating the column for the state of the hospitalization
dados_sih_neo <- dados_sih_neo %>%
  mutate(REG_ESTAB = ifelse(substr(MUNIC_MOV, 1, 2) == "11", "Rondônia",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "12", "Acre", 
                     ifelse(substr(MUNIC_MOV, 1, 2) == "13", "Amazonas",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "14", "Roraima",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "15", "Pará",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "16", "Amapá",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "17", "Tocantins",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "21", "Maranhão",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "22", "Piauí",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "23", "Ceará",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "24", "Rio Grande do Norte",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "25", "Paraíba",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "27", "Alagoas",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "28", "Sergipe",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "29", "Bahia",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "31", "Minas Gerais",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "32", "Espírito Santo",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "33", "Rio de Janeiro",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "35", "São Paulo",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "41", "Paraná",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "42", "Santa Catarina",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "43", "Rio Grande do Sul",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "50", "Mato Grosso do Sul",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "51", "Mato Grosso",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "52", "Goiás",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "53", "Distrito Federal",
                     ifelse(substr(MUNIC_MOV, 1, 2) == "00", "Ignorado/Exterior", "Pernambuco"))))))))))))))))))))))))))))


# Creating the column for the region of the hospitalization
dados_sih_neo$Regiao=ifelse(dados_sih_neo$REG_ESTAB %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul"),c("Sul"),
                            ifelse(dados_sih_neo$REG_ESTAB %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo", "Minas Gerais"),c("Sudeste"),
                            ifelse(dados_sih_neo$REG_ESTAB %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal"),c("Centro-Oeste"),
                            ifelse(dados_sih_neo$REG_ESTAB %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"),c("Norte"), "Nordeste"))))

 
# Changing variable type
dados_sih_neo$ANO_CMPT <- as.factor(dados_sih_neo$ANO_CMPT)
dados_sih_neo$VAL_SH <- as.numeric(dados_sih_neo$VAL_SH)
dados_sih_neo$VAL_SP <- as.numeric(dados_sih_neo$VAL_SP)
dados_sih_neo$VAL_TOT <- as.numeric(dados_sih_neo$VAL_TOT)
dados_sih_neo$VAL_UTI <- as.numeric(dados_sih_neo$VAL_UTI)
dados_sih_neo$US_TOT <- as.numeric(dados_sih_neo$US_TOT)
dados_sih_neo$QT_DIARIAS <- as.numeric(dados_sih_neo$QT_DIARIAS)
dados_sih_neo$DIAS_PERM <- as.numeric(dados_sih_neo$DIAS_PERM)
dados_sih_neo$UTI_MES_TO <- as.numeric(dados_sih_neo$UTI_MES_TO)
dados_sih_neo$UTI_INT_TO <- as.numeric(dados_sih_neo$UTI_INT_TO)
dados_sih_neo$IDADE <- as.numeric(dados_sih_neo$IDADE)
dados_sih_neo$MORTE <- as.factor(dados_sih_neo$MORTE)
dados_sih_neo$USO_UTI <- as.factor(dados_sih_neo$USO_UTI)
dados_sih_neo$DIAG_PRINC <- as.factor(dados_sih_neo$DIAG_PRINC)


## 4) Adjusting costs (R$) for inflation december 2023: IPCA source - https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice)

# Correction index per year for 2023
correction_index <- c("2011" = 1.9999,
                      "2012" = 1.8950,
                      "2013" = 1.7916,
                      "2014" = 1.6814,
                      "2015" = 1.5219,
                      "2016" = 1.4225,
                      "2017" = 1.3837,
                      "2018" = 1.3299,
                      "2019" = 1.2878,
                      "2020" = 1.2345,
                      "2021" = 1.1148,
                      "2022" = 1.0527,
                      "2023" = 1.0000)


dados_sih_neo <- dados_sih_neo %>%
  mutate(
    val_tot_corrigido = VAL_TOT * correction_index[as.character(ANO_CMPT)],
    val_uti_corrigido = VAL_UTI * correction_index[as.character(ANO_CMPT)],
    val_sh_corrigido = VAL_SH * correction_index[as.character(ANO_CMPT)],
    val_sp_corrigido = VAL_SP * correction_index[as.character(ANO_CMPT)]
  )


# Converting costs in Reais (R$) to International Dollars (Int$) using PPP of 2.44 (for 2023)
dados_sih_neo <- dados_sih_neo %>%
  mutate(
    val_tot_int = val_tot_corrigido / 2.44,
    val_uti_int = val_uti_corrigido / 2.44,
    val_sh_int = val_sh_corrigido / 2.44,
    val_sp_int = val_sp_corrigido / 2.44
  )




