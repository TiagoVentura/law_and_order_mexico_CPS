# ------------------------------------------------------------------------------- #
# Article: Voting for Law and Order in Mexico
# Authors: Cantu, Ley, Ventura
# Update: June 15
# ------------------------------------------------------------------------------- #
library(here)


# Packages ----------------------------------------------------------------
library(tidyverse)
library(summarytools)
library(lubridate)
library(scales)
library(conflicted)
library(rebus)
library(patchwork)
library(extrafont)
library(broom)
library(here)
source(here("code", "utils.R"))
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")



# Paths -------------------------------------------------------------------

#path_tiago <- c("C:/Users/venturat/Dropbox/artigos/Ventura Violence/voting_for_violence_mexico_p3")
#setwd(path_tiago)

#Functions ---------------------------------------------------------------


# short view
short_view <- function(data){
  data %>% slice(1:50) %>% View()
}


# Download the data -------------------------------------------------------

load("./data/mexico_cleaned2_data.Rdata")

dim(d)

# Check Covariates --------------------------------------------------------

levels(d$education)
class(d$income)
levels(d$work)
levels(d$age)


# Network -----------------------------------------------------------------


network <- d %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999")))) %>%
  select(responseid, starts_with("network"), -contains("click"), - contains("submit"))



# long format

network_long <- network %>%
                pivot_longer(cols=-(responseid), 
                             names_to="group", 
                             values_to="values") 

howmany <- stack(network[,c(2:9)])

# Modeling I --------------------------------------------------------------
n.row= nrow(network)
n.cat= 8

howmany$values<-as.integer(howmany[,1])

#off.values2<- as.data.frame(log(cbind(rep(.0086,n.row),rep(.0053,n.row),rep(.00540,n.row),rep(.0052,n.row),rep(.004756,n.row),rep(.01063,n.row),rep(.0041,n.row),rep(.00337,n.row),rep(.00286,n.row),rep(.01773,n.row),rep(.0076,n.row),rep(.0049,n.row),rep(.018,n.row), rep(.049478004,n.row), rep(.001342105,n.row)))) 

#off.values2<- as.data.frame(cbind(rnorm(n.row,log(.0086),-log(.0086)/20), rnorm(n.row, log(.0053),-log(.0053)/20), rnorm(n.row, log(.00540),-log(.00540)/20), rnorm(n.row, log(.0052),-log(.0052)/20), rnorm(n.row, log(.004756), -log(.004756)/20), rnorm(n.row, log(.01063),-log(.01063)/20), rnorm(n.row, log(.0041),-log(.0041)/20), rnorm(n.row, log(.00337),-log(.00337)/20), rnorm(n.row, log(.00286),-log(.00286)/20), rnorm(n.row, log(.01773),-log(.01773)/20), rnorm(n.row, log(.0076),-log(.0076)/20),rnorm(n.row,log(.0049),-log(.0049)/20),rnorm(n.row,log(.018),-log(.018)/20), rnorm(n.row,log(0.049478004),-log(0.049478004)/20), rnorm(n.row,log(0.001342105),-log(0.001342105)/20)  )) 

#colnames(off.values2)<- c("silvia","patricia","antonio","francisco", "angel","maestra", "policia","abogado","medico", "hijo","fallecido","casado","discapac", "empleoprovincial", "preso")
#colnames(119:143)

#offset2 <- stack(off.values2[,1:15])[,1]
id <- rep(unique(network$responseid),n.cat)

#id<- rep(1:n.row,n.cat)

category <- rep(1:n.cat,each=n.row)


N <- length(howmany$values)
C <- max(category)
G <- length(unique(id))

#G <- max(id)


##
## Personal Network
##
library(lme4)
net.lmer <-glmer.nb(howmany$values ~ 1 + (1|category) + (1|id),  nAGQ=0)

a.personal<- coef(net.lmer)

net.personal.lmer <- data_frame(values=a.personal[[1]][,1], 
                                responseid=row.names(a.personal[[1]]))


# this is probably something I need to adjust
net.personal.lmer$values_adj <- exp(net.personal.lmer$values)/0.008
mean(net.personal.lmer$values_adj)
## why .0064

plot(density(net.personal.lmer$values_adj),xlim=range(0:600))

##
## Group Network
##

ncol(network)

# Filter values with offsets
network_filter <- network %>%
                    left_join(net.personal.lmer) %>%
                    filter(!is.na(values))    
  
n.row= nrow(network_filter)
n.cat= 8


howmany<- stack(network_filter[,10:17])
howmany$values<-as.integer(howmany[,1])


#howmany.ini<-howmany
#howmany$values[howmany$values==99]<- NA
#howmany$values[howmany$values>30]<- 30
#howmany.ini$values[howmany.ini$values<2]<- NA
#howmany.ini$values[howmany.ini$values==99]<- 1
#howmany.ini$values[howmany.ini$values>1]<- NA

offset2=rep(net.personal.lmer$values_adj,n.cat)
id <- rep(unique(network_filter$responseid),n.cat)
category <- rep(1:n.cat,each=n.row)


N <- length(howmany$values)
C <- max(category)
G <- length(unique(id))

# N <- length(howmany$values)
# C <- max(category)
# G <- max(id)
length(howmany$values)
length(offset2)
length(category)

net.lmer2<-glmer.nb(howmany$values ~ 1 + offset(log(offset2)) + (1|category) + (1|id), nAGQ=0)

a.final<- ranef(net.lmer2)
#a.sd <- se.ranef(net.lmer2)
net.cat.lmer<-a.final[[2]][,1]

net.greg.lmer<-data_frame(values=a.final[[1]][,1], 
                          responseid=row.names(a.final[[1]]))

# This here: where does this numbers come from. 

group <- exp(coef(net.lmer2)[[2]][1])

str_remove(colnames(network_filter)[10:17],"network_")

colnames(network_filter)[10:17]

rownames(group)<- str_remove(colnames(network_filter)[10:17],"network_")

# prision real data: 255 mil
# victims of crime from survey data: 13%

group*126200000


##
## Full Network
##

ab <- vector("list",n.cat)
residuals <- vector("list",n.cat)

for (i in 1:n.cat){
  ab[[i]]<- data_frame(yhat=exp(net.cat.lmer[i]+ net.greg.lmer$values), 
                        responseid=net.greg.lmer$responseid,
                        group=colnames(network_filter)[10:17][[i]])
} 

ab2<- bind_rows(ab)


ab2 <- left_join(ab2, network_long) %>%
          mutate(res=sqrt(values)-sqrt(yhat))

#residual<- log(howmany$values+1)-log(tab$values+1)

resid.mat <- matrix(ab2$res,length(unique(ab2$responseid)),length(unique(ab2$group)))
colnames(resid.mat) <- colnames(network_filter)[10:17]

network.matrix<-cor(resid.mat,use="complete.obs")

names <- str_remove(colnames(network.matrix), "network_") %>%
          str_replace(., "_", " ") %>%
          str_to_title(.)

colnames(network.matrix) <- names
rownames(network.matrix) <- names

##
## Correlation Plots
##

#Alternativa Color
#corRaw <- cor(1-network.matrix)
#library(spatstat) # "im" function 
#plot(im(corRaw[nrow(corRaw):1,]), main="Correlation Matrix Map")

#Agnes Cluster
library(cluster)
dissimilarity <- 1 - network.matrix
distance <- as.dist(dissimilarity)

library(ggcorrplot)

ggcorrplot(
  network.matrix,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_minimal(),
  colors = c("#6D9EC1", "white", "#E46726")) 

ggsave(filename=here("output", "correlation_matrix.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

library(ggdendro)
hc <- hclust(distance, "ave")
ggdendrogram(hc, xlab="")
plot(hc, xlab="")

# save(filename=here("output", "correlation_matrix.png") ,
#      width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

crime <- ab2 %>% filter(group %in% c("network_victim_crime", "network_police_violence")) 

#save(crime, file=here("data", "network_results.Rdata"))
#save(crime, file=here("data", "network_results_large_ind_net.Rdata"))

#save(ab2, file=here("data", "network_results_all_k.Rdata"))

crime_wide <- crime %>% 
                  pivot_wider(names_from=group, 
                              values_from = c(yhat, res, values))

## Some regressions

library(tidyverse)
load("./data/mexico_cleaned2_data.Rdata")


d <- left_join(d, crime_wide) %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999"))))




d <- d %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999")))) %>%
        mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
            ifelse(.x %in% c("-999", "No lo se"), 
                   NA, .x)) %>%
        mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              fct_relevel(.x, "Muy Seguro", "Seguro", "Poco Seguro")) %>%
        mutate_at(vars(lo_1, lo_2, lo_3, lo_4, lo_5), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x))  %>% 
         mutate_at(vars(crime_victimization, police_victimization, income_assistance,), ~
              ifelse(.x %in% c("-999", "No lo se", "No se"), 
                     NA, .x)) %>%
          mutate(trust_police=as.numeric(na_if(trust_police,"-999")))



# Crime victimization


model_crime <- tribble(
  ~model_name, ~formula, ~data, 
  "Crime Victimization", as.numeric(as.factor(crime_victimization)) ~ 
    income + I(income^2) +
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    police_victimization +
    as.numeric(lo_1) +
    as.numeric(fear_1) + trust_police, d, 
  "Network Raw", as.numeric(network_victim_crime) ~ 
    income + I(income^2) +
    income +  work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization +  as.numeric(lo_1) +
    as.numeric(fear_1) + trust_police, d, 
  "Network Poisson Model", res_network_victim_crime ~ 
    income + I(income^2) +  work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization + as.numeric(lo_1) +
    as.numeric(fear_1) + trust_police, d)



library(broom)

summary(lm(model_crime$formula[[3]], d))

model_crime <- model_crime %>%
                rowwise() %>%
                mutate(model=list(lm(formula, data)), 
                        parameters=list(tidy(model))) %>% 
                  unnest(parameters) %>%
                 mutate(lb=estimate - 1.96*std.error, 
                        up= estimate + 1.96*std.error, 
                        lb90=estimate - 1.64*std.error, 
                        up90= estimate + 1.64*std.error) %>% 
                filter(!(term %in% c("(Intercept)")))
                  

summary(model_crime$model[[3]])

ggplot(model_crime, aes(y=estimate, x=term, 
                        ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Estimates", 
       title = "Explaining Crime Victimization") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~model_name, scales="free") +
  theme(axis.text.y=element_text(size=16,hjust=0))




# Entire Netwokr ----

## Filter values with offsets ----


network_filter <- network %>%
  left_join(net.personal.lmer) %>%
  filter(!is.na(values))    


n.row= nrow(network_filter)
n.cat= 16


howmany<- stack(network_filter[,2:17])
howmany$values<-as.integer(howmany[,1])


#howmany.ini<-howmany
#howmany$values[howmany$values==99]<- NA
#howmany$values[howmany$values>30]<- 30
#howmany.ini$values[howmany.ini$values<2]<- NA
#howmany.ini$values[howmany.ini$values==99]<- 1
#howmany.ini$values[howmany.ini$values>1]<- NA
net.personal.lmer$values_adj <- exp(net.personal.lmer$values)/0.005
plot(density(net.personal.lmer$values_adj))
mean(net.personal.lmer$values_adj)
offset2=rep(net.personal.lmer$values_adj,n.cat)
id <- rep(unique(network_filter$responseid),n.cat)
category <- rep(1:n.cat,each=n.row)


N <- length(howmany$values)
C <- max(category)
G <- length(unique(id))

# N <- length(howmany$values)
# C <- max(category)
# G <- max(id)
length(howmany$values)
length(offset2)
length(category)

net.lmer2<-glmer.nb(howmany$values ~ 1 + offset(log(offset2)) + (1|category) + (1|id), nAGQ=0)

a.final<- ranef(net.lmer2)
#a.sd <- se.ranef(net.lmer2)
net.cat.lmer<-a.final[[2]][,1]

net.greg.lmer<-data_frame(values=a.final[[1]][,1], 
                          responseid=row.names(a.final[[1]]))

#save(net.greg.lmer, file=here("data", "net_greg_ind.Rdata"))



# This here: where does this numbers come from. 

group <- exp(coef(net.lmer2)[[2]][1])

str_remove(colnames(network_filter)[2:17],"network_")

colnames(network_filter)[2:17]
rownames(group)<- str_remove(colnames(network_filter)[2:17],"network_")


group*126200000

##
## Full Network
##

ab <- vector("list",n.cat)
residuals <- vector("list",n.cat)

for (i in 1:n.cat){
  ab[[i]]<- data_frame(yhat=exp(net.cat.lmer[i]+ net.greg.lmer$values), 
                       responseid=net.greg.lmer$responseid,
                       group=colnames(network_filter)[2:17][[i]])
} 

ab2<- bind_rows(ab)


ab2 <- left_join(ab2, network_long) %>%
  mutate(res=sqrt(values)-sqrt(yhat))

#residual<- log(howmany$values+1)-log(tab$values+1)

resid.mat <- matrix(ab2$res,length(unique(ab2$responseid)),length(unique(ab2$group)))
colnames(resid.mat) <- colnames(network_filter)[2:17]

network.matrix<-cor(resid.mat,use="complete.obs")

names <- str_remove(colnames(network.matrix), "network_") %>%
  str_replace(., "_", " ") %>%
  str_to_title(.)

names <- c("Silvia", "Antonio", "Police Officer", "Physician", "Had a Kid", "Died", "Got Married", 
           "Died from Covid", "Public Empoloyee", "Does Social Work", "Partisan Activist", "Was Candidate", 
           "Crime Victim", "Police Violence", "Is in Prision", "Lost Job (Covid)")


colnames(network.matrix) <- names
rownames(network.matrix) <- names
##
## Correlation Plots
##

#Alternativa Color
#corRaw <- cor(1-network.matrix)
#library(spatstat) # "im" function 
#plot(im(corRaw[nrow(corRaw):1,]), main="Correlation Matrix Map")

#Agnes Cluster
library(cluster)
dissimilarity <- 1 - network.matrix
distance <- as.dist(dissimilarity)

library(ggcorrplot)
ggcorrplot(
  network.matrix,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_minimal(),
  colors = c("#6D9EC1", "white", "tomato2")) 

ggsave(filename=here("output", "correlation_matrix.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

library(ggdendro)

hc <- hclust(distance, "ave")
ggdendrogram(hc, xlab="")

png(filename=here("output", "dendogram.png"))
plot(hc, xlab="")
dev.off()

crime <- ab2 %>% filter(group %in% c("network_victim_crime", "network_police_violence")) 

# save(crime, file=here("data", "network_results_all.Rdata"))
# 
# save(ab2, file=here("data", "network_results_all_k.Rdata"))


crime_wide <- crime %>% 
  pivot_wider(names_from=group, 
              values_from = c(yhat, res, values))

## Some regressions

library(tidyverse)
load("./data/mexico_cleaned2_data.Rdata")


d <- left_join(d, crime_wide) %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999"))))


# Clean Covariates

d <- d %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999")))) %>%
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4, contains("po_"), contains("issue_")), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x)) %>%
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              fct_relevel(.x, "Muy Seguro", "Seguro", "Poco Seguro")) %>%
  mutate_at(vars(lo_1, lo_2, lo_3, lo_4, lo_5), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x))  %>% 
  mutate_at(vars(crime_victimization, police_victimization, income_assistance,), ~
              ifelse(.x %in% c("-999", "No lo se", "No se"), 
                     NA, .x)) %>%
  mutate(trust_police=as.numeric(na_if(trust_police,"-999")))  %>%
  mutate(income_quantile=ntile(income, 4), 
         income_quantile_labels=ifelse(income_quantile==4, "Top-Quartile", 
                                       ifelse(income_quantile==1, "Bottom-Quartile", 
                                              "Middle"))) %>%
  rowwise() %>%
  mutate(fear_av=mean(c(fear_1, fear_2, fear_3), na.rm = TRUE)) %>%
  ungroup()



d %>% select(responseid, ipaddress,lo_1, lo_2, lo_3, lo_4, lo_5) %>% 
  filter(ipaddress=="189.190.190.15")

list_lo_invert <- list("0"="5",
                       "1"="4",
                       "2"="3", 
                       "3"="1",
                       "4"="0")

list_lo_correct <- list("0"="0",
                       "1"="1",
                       "2"="3", 
                       "3"="4",
                       "4"="5")

d <- d %>%  
  mutate_at(vars(lo_1, lo_2, 
                 lo_3, lo_4, 
                 lo_5), ~ na_if(.x, "-999")) %>%
  mutate_at(vars(lo_1, lo_2, lo_3), ~ fct_recode(.x, !!!list_lo_correct)) %>% 
  mutate_at(vars(lo_4, lo_5), ~ fct_recode(.x, !!!list_lo_invert)) %>% 
  mutate_at(vars(lo_1, lo_2, lo_3, lo_4, lo_5), ~ as.numeric(as.character(.x))) %>%
  rowwise() %>%
  mutate(lo_av=mean(c(lo_1, lo_2, lo_3, lo_4, lo_5), na.rm = TRUE))


# Crime victimization


model_crime <- tribble(
  ~model_name, ~formula, ~data, 
  "Crime Victimization", as.numeric(as.factor(crime_victimization)) ~ 
    income_quantile_labels +
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    police_victimization +
    as.numeric(po_criminal_rights) +fear_av +
    trust_police +
    as.numeric(issue_security), d, 
  "Network Raw", as.numeric(network_victim_crime) ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization  + as.numeric(po_criminal_rights) +
    fear_av + trust_police +
    as.numeric(issue_security), d, 
  "Network Poisson Model", res_network_victim_crime ~ 
    income_quantile_labels +
    work  +
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization + as.numeric(po_criminal_rights) +
    fear_av + trust_police +
    as.numeric(issue_security), d)


library(broom)

list_terms <- list("Income (Top Quartile)"="income_quantile_labelsTop-Quartile", 
                   "Income (Middle)"="income_quantile_labelsMiddle",
                   "Employed"="workEmployed", 
                   "Female"="genderFemale", 
                   "Crime Victim"="crime_victimizationSi",             
                   "Police Violence"="police_victimizationSi", 
                   "Fear of Crime"="fear_av",
                   "Punitive Preference"="as.numeric(po_criminal_rights)", 
                   "Trust Police"="trust_police", 
                   "Age"="as.numeric(age)", 
                   "Education"="as.numeric(education)", 
                   "Security (Policy Priority)"="as.numeric(issue_security)")
 
model_crime <- model_crime %>%
  rowwise() %>%
  mutate(model=list(lm(formula, data)), 
         parameters=list(tidy(model))) %>% 
  unnest(parameters) %>%
  mutate(lb=estimate - 1.96*std.error, 
         up= estimate + 1.96*std.error, 
         lb90=estimate - 1.64*std.error, 
         up90= estimate + 1.64*std.error) %>% 
  filter(!(term %in% c("(Intercept)"))) %>%
  mutate(term=fct_inorder(fct_recode(term,!!!list_terms)))


ggplot(model_crime, aes(y=estimate, x=fct_rev(term), 
                        ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Estimates", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~model_name, scales="free") +
  theme(axis.text.y=element_text(size=16))

ggsave(filename=here("output", "model_comparison_crime.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

model_police <- tribble(
  ~model_name, ~formula, ~data, 
  "Police Victimization", as.numeric(as.factor(police_victimization)) ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization +
    as.numeric(po_criminal_rights) +
    fear_av  + trust_police +
    as.numeric(issue_security), d, 
  "Network Raw", as.numeric(network_police_violence) ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization +  as.numeric(po_criminal_rights) +
    fear_av  + trust_police +
    as.numeric(issue_security), d, 
  "Network Poisson Model", res_network_police_violence ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization + as.numeric(po_criminal_rights) +
    fear_av + trust_police +
    as.numeric(issue_security), d) 


library(broom)

model_police <- model_police %>%
  rowwise() %>%
  mutate(model=list(lm(formula, data)), 
         parameters=list(tidy(model))) %>% 
  unnest(parameters) %>%
  mutate(lb=estimate - 1.96*std.error, 
         up= estimate + 1.96*std.error, 
         lb90=estimate - 1.64*std.error, 
         up90= estimate + 1.64*std.error) %>% 
  filter(!(term %in% c("(Intercept)"))) %>%
  mutate(term=fct_inorder(fct_recode(term,!!!list_terms)))


ggplot(model_police, aes(y=estimate, x=fct_rev(term), 
                            ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Estimates", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~model_name, scales="free") +
  theme(axis.text.y=element_text(size=16,hjust=1))


## latex table

models <- bind_rows(model_crime, model_police) %>%
          select(model_name, model) %>%
          distinct()

library(stargazer)
c("gdp(%)", "\\multirow{2}{*}{\\parbox{2cm}{revenue per capita}}")
names(coef(models$model[[2]]))
# Labels
new_names = c("(Intercept)" = "Intercept", 
              "income_quantile_labelsMiddle" =  "Income (Middle)",             
              "income_quantile_labelsTop-Quartile" = "Income (Top Quartile)",
              "workEmployed"   ="Employed",          
              "as.numeric(age)"  = "Age",
              "as.numeric(education)"= "Education",                                
              "genderFemale"     =   "Female",
              "crime_victimizationSi" = " Crime Victim",
              "police_victimizationSi"     = "Police Violence Victim",
              "as.numeric(po_criminal_rights)"  =  "Punitive Preferences",
              "fear_av"    =  "Fear of Crime",  
              "trust_police" = "Trust in the Police", 
              "as.numeric(issue_security)" = "Security Top Priority")

stargazer(models$model[[3]], models$model[[1]], models$model[[6]],models$model[[4]], 
          intercept.bottom = FALSE, 
          dep.var.labels.include = FALSE,
          column.labels = c("\\multirow{2}{*}{\\parbox{5cm}{Crime Victimization (Network Residuals)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{crime Victimization (Survey Questions)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{Police Violence (Network Residuals)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{Police Violence (Survey Questions)}}"),
          omit.stat = c("rsq", "f", "ser"), 
          covariate.labels =new_names, 
          model.numbers = FALSE, 
          title="Regression Estimates: Correlates of Contextual and Individual Victimization", 
          label="reg_net", 
          out=here("output", "table_res_regression.tex"))

c("gdp(%)", "\\multirow{2}{*}{\\parbox{2cm}{revenue per capita}}")

# only crime

stargazer(models$model[[3]], models$model[[1]],
          intercept.bottom = FALSE, 
          dep.var.labels.include = FALSE,
          column.labels = c("\\multirow{2}{*}{\\parbox{5cm}{Crime Victimization (Network Residuals)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{crime Victimization (Survey Questions)}}"),
          omit.stat = c("rsq", "f", "ser"), 
          covariate.labels = c("Intercept", 
                               "Income (Middle)", "Income (Top Quartile)", 
                               "Employed" , "Age",  "Education",  "Female", 
                               "Crime Victim" , "Police Victimization", 
                               "Punitive Preferences", "Fear of Crime", 
                               "Trust in the Police"), 
          model.numbers = FALSE)


