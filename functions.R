# Creating functions for later analysis_________________________________________



# Functions for calculating the frequency (n) and percentage (%) of variables:


    ## Calculates frequency and % of records grouped by a variable
tab_freq <- function(dados, group){
  dados %>%
    group_by({{group}}) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prop = round(prop.table(n) * 100, 3))
}



    ## Calculates frequency and % of one variable grouped by another variable
tab_freq_group <- function(dados, variavel, grupo){
  tab <- dados |> 
    group_by({{grupo}}, {{variavel}}) |> 
    count() |> 
    ungroup() |> 
    group_by({{grupo}}) |> 
    mutate(porc = 100*n/sum(n))
  
  return(tab)
  
}



    ## Calculates frequency and % of one variable grouped by 2 other variables
tab_freq_group2 <- function(dados, variavel, grupo1, grupo2){
  tab <- dados |> 
    group_by({{variavel}}, {{grupo1}}, {{grupo2}}) |> 
    count() |> 
    ungroup() |> 
    group_by({{grupo1}}, {{grupo2}}) |> 
    mutate(porc = 100*n/sum(n))
  
  return(tab)
  
}


#_______________________________________________________________________________


# Functions for calculating mean, median, standard deviation, minimum and maximum, sum..:


    ## Function of descriptive analysis of one variable grouped by another
descriptive <- function(dados, grupo, vd){
  dados %>%
    group_by({{grupo}}) %>% 
    summarise(media = mean({{vd}}),
              desvio_padrao = sd({{vd}}),
              mediana = median({{vd}}),
              q1 = quantile({{vd}}, p = 0.25),
              q3 = quantile({{vd}}, p = 0.75),
              min = min({{vd}}),
              max = max({{vd}}),
              sum = sum({{vd}}))
}


    ## Function of descriptive analysis of one variable grouped by another - only for those records admitted to the ICU
descriptive_icu <- function(dados, filtro, grupo, vd){
  dados %>%
    filter({{filtro}} == "Sim") %>%
    group_by({{grupo}}) %>% 
    summarise(media = mean({{vd}}),
              desvio_padrao = sd({{vd}}),
              mediana = median({{vd}}),
              q1 = quantile({{vd}}, p = 0.25),
              q3 = quantile({{vd}}, p = 0.75),
              min = min({{vd}}),
              max = max({{vd}}),
              soma = sum({{vd}}))
}


    ## Function for descriptive analysis of only one variable
descriptive_uniq <- function(dados, vd){
  dados %>%
    summarise(media = mean({{vd}}),
              desvio_padrao = sd({{vd}}),
              mediana = median({{vd}}),
              q1 = quantile({{vd}}, p = 0.25),
              q3 = quantile({{vd}}, p = 0.75),
              min = min({{vd}}),
              max = max({{vd}}),
              soma = sum({{vd}}))
}


  ## Function for descriptive analysis of only one variable - only for those records admitted to the ICU
descriptive_uniq_icu <- function(dados, vd){
  dados %>%
    filter(USO_UTI == "Sim") %>%
    summarise(media = mean({{vd}}),
              desvio_padrao = sd({{vd}}),
              mediana = median({{vd}}),
              q1 = quantile({{vd}}, p = 0.25),
              q3 = quantile({{vd}}, p = 0.75),
              min = min({{vd}}),
              max = max({{vd}}),
              soma = sum({{vd}}))
}

