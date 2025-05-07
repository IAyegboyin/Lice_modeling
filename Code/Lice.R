#Introduction----
#07-05-2025 
#This work is still in progress .....

#I am working on this small lice data, just to keep improving my linear modeling practice. 
#The data used in this work is gotten from Nosa Osawe's github project nammed "Tick"
#I hope I can develop some better models apart from the one the Author did in his work

#Libraries----
Library(tidyverse)
Library(readxl)
Library(emmeans)
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

#This is the model developed by the data owner
M.straminus.model <- glm(Menacanthis_straminus ~ Location, data = lice, 
                         family =  quasipoisson(link = "log"))
summary(M.straminus.model)

check_model(M.straminus.model)
check_overdispersion(M.straminus.model)
emmeans(M.straminus.model, pairwise ~ Location,
        adjust = "Tukey")
model_performance(M.straminus.model)

#This is good as the model created by the author of the work 
M.straminus.model1 <- glm(Menacanthis_straminus ~ Location, data = lice, 
                          family =  quasipoisson(link = "identity"))
check_model(M.straminus.model1)
check_overdispersion(M.straminus.model1)
emmeans(M.straminus.model1, pairwise ~ Location,
        adjust = "Tukey")
model_performance(M.straminus.model1)

#Trying to see if this model will work better but the value of AIC is too large.
#Although it was not compared with other model but can keep it here also
M.straminus.model2 <- glm.nb(Menacanthis_straminus ~ Location, data = lice)
summary(M.straminus.model2)

check_model(M.straminus.model2)
check_overdispersion(M.straminus.model2)
emmeans(M.straminus.model2, pairwise ~ Location,
        adjust = "Tukey")
model_performance(M.straminus.model2)

#This is the model developed by the data owner for Menopon_galinae
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

#A new and better code with lower AIC was developed using negative binomial
M.galinae.nb <- glm.nb(Menopon_galinae ~ Location, data = lice)

summary(M.galinae.nb)
check_overdispersion(M.galinae.nb)
emmeans(M.galinae.nb, pairwise ~ Location,
        adjust = "Tukey")
check_model(M.galinae.nb)
check_homogeneity(M.galinae.nb)    # Heteroskedastic mfk!
check_zeroinflation(M.galinae.nb)  # model OK
model_performance(M.galinae.nb)

#Prevalence check -----
#Checking prevalence of the two Lice species
lice.prevalence <- lice %>% 
  dplyr::select(-Data_collector) %>% 
  mutate(prev_M.straminus = ifelse(Menacanthis_straminus>0, 1, 0),
         prev_Menopon_galinae = ifelse(Menopon_galinae>0, 1, 0)) %>%
  dplyr::select(-Menacanthis_straminus,-Menopon_galinae) %>% 
  group_by(Location) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  mutate(prev_M.straminus= prev_M.straminus*2, #prevalence=(count(lice present)/50)*100
         prev_Menopon_galinae= prev_Menopon_galinae*2) %>% 
  as.data.frame()

#Visualization of prevalence----
#Visualization of the prevalence of the lice species
ggplot(lice.prevalence, aes(x = Location, y = prev_M.straminus)) + 
  geom_col() + 
  labs(y = "Prevalence (%)", title = "M. straminus Prevalence by Location")

#Citations---- 
# Citation for the packages used in the work
citation("emmeans")
citation("performance")
citation("tidyverse")
citation("MASS")

