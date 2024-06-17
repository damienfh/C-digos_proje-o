## Script realizado para analisar o fenômeno do crescimento da chefia feminina no Brasil a partir da PNADc de 2012 a 2022.

library(PNADcIBGE)
library(magrittr)
library(survey)
library(dplyr)


############################## Censos - IPUMS ##################################

ddi <- read_ipums_ddi("ipumsi_00047.xml")
data_natal_80_91 <- read_ipums_micro(ddi)

data_chefia %>% 
  group_by(RELATE, SEX) %>% 
  summarize(total_weight = sum(HHWT)) %>%
  mutate(proporcao = total_weight / sum(total_weight)) 


data_natal_80_91 %>% 
  filter(RELATE == 1 & YEAR == 1991) %>% 
  group_by(RELATE, SEX) %>% 
  summarize(total_weight = sum(HHWT)) %>%
  mutate(proporcao = total_weight / sum(total_weight)) 

rm(data_natal_2000, data_natal_60_70, data_natal_80_91)


data %>% 
  group_by(CHEFIA_FEMININA, BR2010A_RACE) %>% 
  summarize(total_weight = sum(HHWT)) %>%
  mutate(proporcao = total_weight / sum(total_weight)) 

data %>% 
  group_by(BR2010A_RACE) %>% 
  summarize(total_weight = sum(HHWT)) %>%
  mutate(proporcao = total_weight / sum(total_weight)) 
################################## PNADc #######################################
PNAD <- PNADcIBGE::get_pnadc(year=2022
                            , vars = c("V2005", "V2010", "VD3004", "V2007", "V2009", "VD4019", "VD4020", "VD4048", "V2001", "V1022"), interview = 1, labels=TRUE, deflator=TRUE, design=T)

 PNAD[["variables"]] %>%
   group_by(UPA)
 
 ####    ####    ####    ####    ####    ####    ####    ####    ####    ####    
 
 PNAD %>% 
   subset(V2005 == "Pessoa responsável pelo domicílio"& V1022 == "Urbana") %>% 
   svytotal(~V2007, .)
 
 PNAD %>% 
   subset(V2005 == "Pessoa responsável pelo domicílio"&V1022 == "Rural") %>% 
   svytotal(~V2007, .) 
 
 PNAD %>% 
   subset(V2005 == "Pessoa responsável pelo domicílio"& V1022 == "Urbana") %>% 
   svymean(~V2007, .) 
 
 PNAD %>% 
   subset(V2005 == "Pessoa responsável pelo domicílio"& V1022 == "Rural") %>% 
   svymean(~V2007, .)

############################


PNAD$variables <- transform(PNAD$variables, VD4019real=VD4019*CO1)

PNAD = PNAD[["variables"]] %>%
  group_by(ID_DOMICILIO) %>%
  mutate(Soma_Renda = sum(sum(VD4019real, na.rm = TRUE)+sum(VD4048, na.rm = TRUE)),
         Num_Pessoas = max(V2001, na.rm = TRUE),
         Renda_Per_Capita = Soma_Renda / Num_Pessoas) 


PNAD = pnadc_design(PNAD)

quintis_h <- quantile(PNAD$variables$Renda_Per_Capita, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)

PNAD <- transform(PNAD, Quintis_h = cut(PNAD$variables$Renda_Per_Capita, 
                                        breaks = quintis_h, 
                                        labels = FALSE, include.lowest = TRUE))




  PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& V1022 == 1) %>% 
  svymean(~V2007, .)

  PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 2) %>% 
  svymean(~V2007, .) 

  PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 3) %>% 
  svymean(~V2007, .) 

  PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 4) %>% 
  svymean(~V2007, .) 

  PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 5) %>% 
  svymean(~V2007, .) 

####    ####    ####    ####    ####    ####    ####    ####    ####    ####    

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 1) %>% 
  svytotal(~V2007, .)

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 2) %>% 
  svytotal(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 3) %>% 
  svytotal(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 4) %>% 
  svytotal(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Quintis_h == 5) %>% 
  svytotal(~V2007, .) 



data_natal_80_91 %>% 
  filter(RELATE == 1 & YEAR == 1980) %>% 
  group_by(BR1980A_COLOR, SEX) %>% 
  summarize(total_weight = sum(HHWT)) %>%
  mutate(proporcao = total_weight / sum(total_weight)) 

################################################################################


#### V2007 = Sexo
#### V2005 = Condiçao no domicílio
#### V2010 = Cor ou raça


############################ Chefia por escolaridade #################################

### Pensar em filtrar por sexo

PNAD = PNAD %>%
  mutate(Escolaridade = case_when(
    VD3004 %in% c("Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente ") ~ "Fundamental Incompleto",
    VD3004 %in% c("Fundamental completo ou equivalente", "Médio incompleto ou equivalente") ~ "Fundamental Completo",
    VD3004 %in% c("Médio completo ou equivalente", "Superior incompleto ou equivalente") ~ "Médio Completo",
    VD3004 == "Superior completo" ~ "Superior Completo",
  ))

PNAD = pnadc_design(PNAD)

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Fundamental Incompleto") %>% 
  svymean(~V2007, .)

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Fundamental Completo") %>% 
  svymean(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Médio Completo") %>% 
  svymean(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Superior Completo") %>% 
  svymean(~V2007, .) 


####    ####    ####    ####    ####    ####    ####    ####    ####    ####    

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Fundamental Incompleto") %>% 
  svytotal(~V2007, .)

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Fundamental Completo") %>% 
  svytotal(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Médio Completo") %>% 
  svytotal(~V2007, .) 

PNAD %>% 
  subset(V2005 == "Pessoa responsável pelo domicílio"& Escolaridade == "Superior Completo") %>% 
  svytotal(~V2007, .) 



############################ Pirâmide Etária ###################################

PNAD <- PNAD %>% 
  mutate(VD4019real=VD4019*CO1)

PNAD = PNAD %>%
  group_by(ID_DOMICILIO) %>%
  mutate(Soma_Renda = sum(sum(VD4019real, na.rm = TRUE)+sum(VD4048, na.rm = TRUE)),
         Num_Pessoas = max(V2001, na.rm = TRUE),
         Renda_Per_Capita = Soma_Renda / Num_Pessoas) 


PNAD_F <- PNAD %>%
  filter(ID_DOMICILIO %in% PNAD$ID_DOMICILIO[V2005 == "Pessoa responsável pelo domicílio" & V2007 == "Mulher"])


 PNAD_M <- PNAD %>%
  filter(ID_DOMICILIO %in% PNAD$ID_DOMICILIO[V2005 == "Pessoa responsável pelo domicílio" & V2007 == "Homem"])


PNAD_F = pnadc_design(PNAD_F)

PNAD_M = pnadc_design(PNAD_M)


mean(PNAD_F$Renda_Per_Capita, na.rm = T)

mean(PNAD_M$Renda_Per_Capita, na.rm = T)



PNAD_F %>% 
  subset(Intervalo_etário == "85 a 89") %>% 
  svytotal(~V2007, .) 

PNAD_M %>% 
  subset(Intervalo_etário == "85 a 89") %>% 
  svytotal(~V2007, .) 


data1 = data1 %>% 
  mutate(CHEFIA_FEMININA = ifelse(SEX == 2 & RELATE == 1, 1, 0))


data1 %>% 
  group_by(MI, CHEFIA_FEMININA) %>%
  summarize(total_weight = sum(HHWT)) %>%
  mutate(proporcao = total_weight / sum(total_weight)) 




##############################

limites <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120)

PNAD$Intervalo_etário <- cut(PNAD$V2009, breaks = limites, 
                             labels = c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", 
                                        "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49",
                                        "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74",
                                        "75 a 79", "80 a 84", "85 a 89", "90 a 94", "95 a 99",
                                        "100 a 104", "105 a 109", "110 a 114", "115 a 119"), 
                             right = FALSE)


PNAD %>% 
  select(V2009, V2007) %>% 
  View()


PNAD %>% 
  group_by(Intervalo_etário, V2007) %>%
  summarize(count = n()) %>%
  mutate(proporcao = count / sum(count)) %>% 
  print(n = 48)
