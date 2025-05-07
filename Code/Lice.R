#Introduction----
#This work is still in progress .....
#07-05-2025 
#I am working on this small lice data, just to keep improving my linear modeling practice

#Libraries----
require(tidyverse)
require(readxl)
library(emmeans)
library(performance)
library(MASS)

#Load Data----
lice <- read_excel("/Users/aia/Desktop/Data Science Library/Data for play/Lice_modeling/Data/Lice.xlsx",
                   sheet = "workingsheet")
view(lice)

#Data manipulation and Descriptive Statistics----
lice %>% 
dplyr::select(-Data_collector) %>% 
  group_by(Location) %>% 
  summarise(total= sum(across(where(is.numeric))),
            mean_Menacanthis_straminus = mean(Menacanthis_straminus),
            mean_Menopon_galinae= mean(Menopon_galinae),
            se_Me.straminus = sd(Menacanthis_straminus)/sqrt(length(Menacanthis_straminus)),
            se_Me.galinae = sd(Menopon_galinae)/sqrt(length(Menopon_galinae)),
            across(where(is.numeric), sum))%>% 
  as.data.frame()

#Checking for normality of data----
#Menacanthis.straminus data check is here
#qqplot
qqnorm(lice$Menacanthis_straminus)
qqline(lice$Menacanthis_straminus)
#shapiro-wilk test
shapiro.test(lice$Menacanthis_straminus)
#Menacanthis.straminus data check ends here

#Menopon.galinae data check starts here
#qqplot
qqnorm(lice$Menopon_galinae)
qqline(lice$Menopon_galinae)
#shapiro-wilk test
shapiro.test(lice$Menopon_galinae)
#Menopon.galinae data check ends here

#Model Building----
# Model Building for Menacanthis straminus

M.straminus.model1 <- glm(Menacanthis_straminus ~ Location, data = lice, 
                         family =  quasipoisson(link = "log"))
summary(M.straminus.model1)

check_model(M.straminus.model1)
check_overdispersion(M.straminus.model1)
emmeans(M.straminus.model1, pairwise ~ Location,
        adjust = "Tukey")
model_performance(M.straminus.model1)


M.straminus.model2 <- glm(Menacanthis_straminus ~ Location, data = lice, 
                          family =  quasipoisson(link = "identity"))
check_model(M.straminus.model2)
check_overdispersion(M.straminus.model2)
emmeans(M.straminus.model2, pairwise ~ Location,
        adjust = "Tukey")
model_performance(M.straminus.model2)

M.straminus.model3 <- glm.nb(Menacanthis_straminus ~ Location, data = lice)
summary(M.straminus.model3)

check_model(M.straminus.model3)
check_overdispersion(M.straminus.model3)
emmeans(M.straminus.model3, pairwise ~ Location,
        adjust = "Tukey")
model_performance(M.straminus.model3)

#       ----------- Menopon_galinae-------------------

M.galinae.quasi <- glm(Menopon_galinae ~ Location, data = lice, 
                       family = quasipoisson(link = "log")
)
summary(M.galinae.quasi)
check_overdispersion(M.galinae.quasi)
emmeans(M.galinae.quasi, pairwise ~ Location,
        adjust = "Tukey")
check_model(M.galinae.quasi)
check_homogeneity(M.galinae.quasi)    # Heteroskedastic mfk!
check_zeroinflation(M.galinae.quasi)  # model OK
model_performance(M.galinae.quasi)


lice %>% 
  dplyr::select(-Data_collector) %>% 
  mutate(prev_M.straminus = ifelse(Menacanthis_straminus>0, 1, 0),
         prev_Menopon_galinae = ifelse(Menopon_galinae>0, 1, 0)) %>%
  dplyr::select(-Menacanthis_straminus,-Menopon_galinae) %>% 
  group_by(Location) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  mutate(prev_M.straminus= prev_M.straminus*2, #prevalence=(count(lice present)/50)*100
         prev_Menopon_galinae= prev_Menopon_galinae*2) %>% 
  as.data.frame()



citation("emmeans")
citation("performance")


