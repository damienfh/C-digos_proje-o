## Código elaborado para projetar projetar a população da Indonésia entre 2000 e 2010.

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("ipumsi_00011.xml")
data <- read_ipums_micro(ddi)

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(clipr)
library(openxlsx)
library(readxl)


glimpse(data)

data2010$ID2010A_PROV5 

summary(data2010$ID2010A_PROV5)

data2010 = data %>% 
  filter(YEAR == 2010)

data2010 = data2010 %>% 
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  ))

data2000 = data %>% 
  filter(YEAR == 2000)

data2000 = data2000 %>% 
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  ))


unique(data2010$MIGRATE5)

table(data2010$MIGRATE5)

glimpse(data2010)

prop.table(table(data2010$MIGRATE5, data2010$AGE))

# Tabela de frequência de MIGRATE5 por SEX (2010)
table_sex_migrate_2010 <- table(data2010$SEX, data2010$MIGRATE5)
prop.table(table_sex_migrate_2010, margin = 1)
round(prop.table(table_sex_migrate_2010, margin = 1), 3)

# Tabela de frequência de MIGRATE5 por AGE (2010)
table_age_migrate_2010 <- table(data2010$AGE, data2010$MIGRATE5)
prop.table(table_age_migrate_2010, margin = 1)
round(prop.table(table_age_migrate_2010, margin = 1), 3)

# Tabela de frequência de MIGRATE5 por SEX (2000)
table_sex_migrate_2000 <- table(data2000$SEX, data2000$MIGRATE5)
prop.table(table_sex_migrate_2000, margin = 1)
round(prop.table(table_sex_migrate_2000, margin = 1), 3)

# Tabela de frequência de MIGRATE5 por AGE (2000)
table_age_migrate_2000 <- table(data2000$AGE, data2000$MIGRATE5)
prop.table(table_age_migrate_2000, margin = 1)
round(prop.table(table_age_migrate_2000, margin = 1), 3)

###############################################################################

## Criação de data.frames com a frequência de migrantes por idade e sexo para 1990, 2000 e 2010

# 2010 

table_migrate_2010 <- table(data2010$AGE, data2010$MIGRATE5, data2010$SEX)
prop.table(table_migrate_2010, margin = 1)
round(prop.table(table_migrate_2010, margin = 1), 3)

# 2000 

table_migrate_2000 <- table(data2000$AGE, data2000$MIGRATE5, data2000$SEX)
prop.table(table_migrate_2000, margin = 1)
round(prop.table(table_migrate_2000, margin = 1), 3)

# 1990 

table_migrate_1990 <- table(data1990$AGE, data1990$MIGRATE5, data1990$SEX)
prop.table(table_migrate_1990, margin = 1)
round(prop.table(table_migrate_1990, margin = 1), 3)

# salvando em um arquivo CSV

write.xlsx(table_migrate_1990, file = "Indonésia_MIGR_1990.xlsx")
write.xlsx(table_migrate_2000, file = "Indonésia_MIGR_2000.xlsx")
write.xlsx(table_migrate_2010, file = "Indonésia_MIGR_2010.xlsx")


############################ Suavizando a curva da frequência de migrantes por sexo e idade ################################

Migration_Indo <- read_excel("Migration_Indo.xlsx")

glimpse(Migration_Indo)

# Vetor de valores
valores <- c(87415, 84080, 79000, 72065, 69915, 70635, 56590, 60710, 62165, 67275, 77620, 91245, 112070, 149675, 195005, 242970,
             210995, 208480, 194005, 187210, 199195, 161945, 173130, 157880, 146100, 171330, 122375, 112380, 97725, 97570, 112520,
             80555, 78615, 68320, 64360, 82455, 51260, 47840, 38630, 38225, 47440, 31160, 29290, 25450, 25565, 36170, 20260, 18390,
             14760, 16385, 20300, 13535, 13395, 11670, 11165, 19790, 8215, 7515, 6830, 6765, 10490, 4835, 5625, 4370, 5005, 9580,
             3435, 3300, 2820, 2490, 3995, 1720, 1575, 1645, 1695, 3150, 1050, 905, 625, 565, 995, 535, 425, 395, 460, 735, 200,
             190, 145, 125, 265, 95, 125, 3515)

# Ajuste da curva suavizada
ajuste <- smooth.spline(1:length(valores), valores)

# Plot dos valores originais e da curva suavizada
df <- data.frame(x = 1:length(valores), y = valores)
df2 <- data.frame(x = ajuste$x, y = ajuste$y)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = df2, aes(x = x, y = y), color = "red") +
  ggtitle("Padrão da migração interna de longa distância Indonésia 2000 - 2010") +
  labs(x = "Idade", y = "Número de Observaçõe") +
  theme_classic()


### Valores ###

valores_A_2000 = pull(Osmigrantes, A2000)
valores_A_2010 = pull(Osmigrantes, A2010)
valores_M_2000 = pull(Osmigrantes, M2000)
valores_M_2010 = pull(Osmigrantes, M2010)
valores_H_2000 = pull(Osmigrantes, H2000)
valores_H_2010 = pull(Osmigrantes, H2010)

### Ajuste ###

ajuste1 <- smooth.spline(1:length(valores_A_2000), valores_A_2000)
ajuste2 <- smooth.spline(1:length(valores_A_2010), valores_A_2010)
ajuste3 <- smooth.spline(1:length(valores_M_2000), valores_M_2000)
ajuste4 <- smooth.spline(1:length(valores_M_2010), valores_M_2010)
ajuste5 <- smooth.spline(1:length(valores_H_2000), valores_H_2000)
ajuste6 <- smooth.spline(1:length(valores_H_2010), valores_H_2010)

# Plot dos valores originais e da curva suavizada

df7 <- data.frame(x = ajuste6$x, y = ajuste6$y)


write.xlsx(df7, file = "df7.xlsx")


######## Teste Nepal #########

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("ipumsi_00024.xml")
data <- read_ipums_micro(ddi)


data = data %>% 
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  ))



table_migrate <- table(data$AGE, data$MIGRATE5, data$SEX)
prop.table(table_migrate, margin = 1)
round(prop.table(table_migrate, margin = 1), 3)

table_migrate2 <- table(data$AGE, data$MIGRATE5)
prop.table(table_migrate2, margin = 1)
round(prop.table(table_migrate, margin = 1), 3)

write.xlsx(table_migrate2, file = "Nepal_MIGR2.xlsx")

data$MIGRATE5



###################### Modelo Rogers e Castro, 1981 #############################


install.packages("rcbayes")
library(rcbayes)
library(tibble)
library(ggplot2)

### Curva padrão ###

pars <- c(a1= 0.05, alpha1= 0.1,
          a2= 0.2, alpha2= 0.1, mu2= 21, lambda2= 0.4,
          a3= 0.02, alpha3= 0.22, mu3= 67, lambda3= 0.6,
          a4= 0.01, lambda4= 0.01,
          c= 0.0)

pars <- c(a1= 0.05, alpha1= 0.1,
          a2= 0.2, alpha2= 0.07, mu2= 21, lambda2= 0.5,
          c= 0.01)

#a1: decréscimo do pré-trabalho (crianças)
#alpha1: também faz parte do decréscimo do pré trabalho, mas no quesito nível.
#a2: nível da migração laboral (pico trabalhadores)
#alpha2: intensidade do decréscimo da força de trabalho
#mu2: pico modal da função (idade de maior migração)
#lambda2: Quanto mais baixo mais o sino da força laboral fica largo
#c: Constante do nível geral da função

ages <- 0:100
mx <- mig_calculate_rc(ages = ages, pars = pars)

# plot to see what the schedule looks like
df <- tibble(age = ages, mx = mx)
df %>%
  ggplot(aes(age, mx)) +
  geom_line() +
  ggtitle("Rogers-Castro age-specific migration schedule (13-parameter)")

fl_ages <- 0:75
fl_migrants <- c(2897,
                 2775,
                 2670,
                 2944,
                 4379,
                 7045,
                 8593,
                 9070,
                 8683,
                 5976,
                 2449,
                 139,
                 512,
                 3,
                 1155,
                 2760,
                 3882,
                 3899,
                 4116,
                 5638,
                 8490,
                 10002,
                 10203,
                 10253,
                 11610,
                 13514,
                 14783,
                 15112,
                 14915,
                 14585,
                 14094,
                 13877,
                 13919,
                 13802,
                 12618,
                 11032,
                 10042,
                 9901,
                 9777,
                 8836,
                 7334,
                 6230,
                 6149,
                 6095,
                 5447,
                 4192,
                 3523,
                 3434,
                 3406,
                 2771,
                 1886,
                 1315,
                 1206,
                 1198,
                 926,
                 470,
                 126,
                 89,
                 108,
                 86,
                 43,
                 14,
                 14,
                 3,
                 86,
                 216,
                 312,
                 321,
                 320,
                 346,
                 399,
                 427,
                 431,
                 434
)

fl_pop <- c(2461443,
            2447442,
            2441492,
            2407166,
            2356288,
            2313344,
            2283764,
            2273533,
            2272767,
            2271010,
            2265408,
            2273901,
            2293549,
            2289738,
            2261599,
            2233326,
            2208660,
            2188823,
            2188023,
            2174602,
            2160524,
            2166331,
            2156134,
            2138934,
            2145926,
            2185269,
            2208234,
            2201949,
            2180371,
            2150739,
            2123413,
            2090772,
            2052082,
            2014650,
            1983057,
            1947597,
            1916786,
            1886136,
            1848442,
            1815289,
            1781761,
            1743248,
            1704863,
            1667202,
            1620296,
            1568195,
            1517743,
            1467010,
            1414168,
            1357684,
            1296784,
            1234403,
            1173202,
            1111432,
            1049453,
            989664,
            931038,
            871851,
            813586,
            758042,
            710299,
            670432,
            636904,
            612727,
            588106,
            561048,
            537478,
            514180,
            490236,
            465325,
            439064,
            410990,
            381047,
            349577,
            317098
)

df <- tibble(age = fl_ages, mx = fl_migrants / fl_pop)
df %>%
  ggplot(aes(age, mx)) +
  geom_point() +
  ggtitle("Observed migration rates")


rc_res <- mig_estimate_rc(
  ages=fl_ages, migrants=fl_migrants, pop=fl_pop,
  pre_working_age = TRUE,
  working_age = TRUE,
  retirement = TRUE,
  post_retirement = FALSE)

rc_res[["fit_df"]] %>%
  ggplot(aes(ages, data)) +
  geom_point(aes(color = "data")) +
  geom_line(aes(x = ages, y = median, color = "fit")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(name = "", values = c(data = "red", fit = "black")) +
  ylab("migration rate")


write.xlsx(df, file = "Curva_Indonésia4.xlsx")
