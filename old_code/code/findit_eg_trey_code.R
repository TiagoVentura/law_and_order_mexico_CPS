library(tidyverse)
library(FindIt)

# Data setup --------------------------------------------------------------

# NOTE: this code is from before forcats was common... yuck

load(here::here("Data", "cdf.rdata"))

# Setup factors
cdf$ordDonor <- factor(cdf$ordDonor, ordered = F,
                       rev(c("1Japan", "2China", "3African Development Bank", 
                             "4European Union", "5World Bank", "6United States")))

cdf$ordProject <- factor(cdf$ordProject, ordered = F,
                         rev(c("1Electrfication", "2Roads", "3Conflict Mitigation", 
                               "4Piped Water Supply", "5Education", "6Healthcare")))

cdf$ordGov <- factor(cdf$ordGov, ordered = F,
                     c("1Minimal", "1Substantial"))

cdf$ordCost <- factor(cdf$ordCost, ordered = T,
                      c("330 percent", "210 percent", "1none"))

# Ensure no missing 
cdf <- cdf %>% filter(!is.na(ordDonor))

# Set up clusters, pairs, and make choice numeric
cdf <-
  cdf %>% 
  mutate(respno = as.factor(as.character(cdf$respno)),
         pair = as.numeric(cdf$respno) * 1000 + cdf$round,
         choice = as.numeric(choice))


# Split data --------------------------------------------------------------

# NOTE: this is not how I would split the data these days; probably holdover code from Egami/Imai's replication scripts
# Either way, need to split the data to get SEs when collapsing/screening interactions

# Sample for training and test observations
train.ind <- sample(unique(cdf$respno), 
                    length(unique(cdf$respno)) / 2 - 0.5, 
                    replace = FALSE)
test.ind <- setdiff(unique(cdf$respno), train.ind)


# Assign train and test data objects
cdf.train <- cdf[is.element(cdf$respno,train.ind),]
cdftest <- cdf[is.element(cdf$respno,test.ind),]


# Causal anova, screening for interactions and collapsing -----------------

# Estimate AMCEs
?CausalANOVA
ca1.r <- CausalANOVA(choice ~ ordDonor + ordProject + ordGov + ordCost,
                     # Training data
                     data = cdf.train, 
                     cluster = cdf.train$respno, 
                     # Allow for up to 3 way interactions
                     nway = 3,
                     diff = T, 
                     pair.id = cdf.train$pair,
                     # Screen for interactions
                     screen = T, 
                     # Allow collapsing of factors within groups
                     collapse = TRUE, 
                     seed = 23)

# Inference (i.e. get some SEs)
ca1.rt <- test.CausalANOVA(ca1.r, 
                           # Test data
                           newdata = cdftest, 
                           diff = TRUE,
                           pair.id=cdftest$pair, 
                           cluster=cdftest$respno)



# Plotting ----------------------------------------------------------------

# NOTE: the results are stored in annoying lists so it takes some work to 'tidy' them
# I haven't updated this plotting code in ~4 years
# I wouldn't do it this way again from scratch, but including for reference

# Pull out estimates
rbind(cbind(0,0,0,0),
      cbind(0,0,0,0),
      cbind(0,0,0,0),
      cbind(0,0,0,0)) %>% as_tibble() %>%
  `colnames<-`(c(c("AME", "sd", "2.5% CI", "97.5% CI"))) -> addsRt

labelsRt <- rev(c("DONOR:", "United States, World Bank, European Union", 
                  "African Development Bank, China, Japan",
                  "PROJECT:", "Healthcare", "Education, Water Supply", 
                  "Conflict Mitigation, Roads", "Electrification",
                  "GOV INVOLVEMENT:", "Minimal", "Substantial",
                  "COST:", "30 percent", "10 percent", "None"))


# Put results in object for plotting
plotterRt <- rbind(ca1.rt$CI.table[[1]], 
                   ca1.rt$CI.table[[2]], 
                   ca1.rt$CI.table[[3]], 
                   ca1.rt$CI.table[[4]])
plotterRt %>% rbind(addsRt) %>% 
  mutate(order = c(seq(1,11,1), .1, 1.1, 6.1, 8.1)) %>%
  arrange(-as.numeric(order)) %>% 
  cbind(labelsRt) -> plotterRt

# Order labels for ggplot
plotterRt$labelsRt <- factor(plotterRt$labelsRt, levels = unique(as.character(plotterRt$labelsRt)))


(collapse_plot <- ggplot(data = plotterRt) + 
    geom_linerange(aes(x = labelsRt, ymin = 0, ymax = as.numeric(AME)), size = .5, color = "gray35", linetype = 3) +
    geom_hline(yintercept = 0, color = "darkred", size = 1, alpha = 1) +
    geom_pointrange(data = filter(plotterRt, labelsRt != "DONOR:" & labelsRt != "PROJECT:" & labelsRt != "COST:" & labelsRt != "GOV INVOLVEMENT:"),
                    aes(y = as.numeric(AME), x = labelsRt,
                        ymin = as.numeric(`2.5% CI`),
                        ymax = as.numeric(`97.5% CI`)),
                    shape = 21, fill = "white", size = .5) +
    
    theme_minimal() +
    coord_flip() +
    scale_x_discrete(labels = labelsRt) +
    geom_vline(xintercept = 15, alpha = .25) + #donor
    geom_vline(xintercept = 12, alpha = .25) + #project
    geom_vline(xintercept = 7, alpha = .25) + #cost
    geom_vline(xintercept = 4, alpha = .25) + #gov  
    labs(x = "", y ="Estimate", title = "Full sample with collapsing",
         caption = paste("N = ", substr(length(ca1.rt$data$choice)*2,1,2), ",",
                         substr(length(ca1.rt$data$choice)*2,3,7), sep = ""))  +
    theme(panel.grid = element_blank(),
          axis.line = element_line(),
          axis.text.y = element_text(face = c('plain', 'plain',  'plain',
                                              'bold','plain', 'plain', 'bold',
                                              'plain','plain','plain','plain', 'bold',
                                              'plain','plain','bold')),
          plot.caption = element_text(color = "gray25", size= 8)) +
    scale_y_continuous(limits = c(-.3, .55)))





data(Carlson)

## Specify the order of each factor
Carlson$newRecordF<- factor(Carlson$newRecordF,ordered=TRUE,
                            levels=c("YesLC", "YesDis","YesMP",
                                     "noLC","noDis","noMP","noBusi"))
Carlson$promise <- factor(Carlson$promise,ordered=TRUE,levels=c("jobs","clinic","education"))
Carlson$coeth_voting <- factor(Carlson$coeth_voting,ordered=FALSE,levels=c("0","1"))
Carlson$relevantdegree <- factor(Carlson$relevantdegree,ordered=FALSE,levels=c("0","1"))
cbind(Carlson$contestresp, Carlson$respcodeS)
## ####################################### 
## Without Screening and Collapsing
## ####################################### 
#################### only AMEs ####################
fit1 <- CausalANOVA(formula=won ~ newRecordF + promise + coeth_voting + relevantdegree,
                    data=Carlson, pair.id=Carlson$contestresp, diff=TRUE,
                    cluster=Carlson$respcodeS, nway=1)
summary(fit1)
plot(fit1)

#################### AMEs and two-way AMIEs ####################
fit2 <- CausalANOVA(formula=won ~ newRecordF + promise + coeth_voting + relevantdegree,
                    int2.formula = ~ newRecordF:coeth_voting,
                    data=Carlson, pair.id=Carlson$contestresp,diff=TRUE,
                    cluster=Carlson$respcodeS, nway=2)
summary(fit2)
plot(fit2, type="ConditionalEffect", fac.name=c("newRecordF","coeth_voting"))
ConditionalEffect(fit2, treat.fac="newRecordF", cond.fac="coeth_voting")

## Not run: 
#################### AMEs and two-way and three-way AMIEs ####################
## Note: All pairs within thee-way interactions should show up in int2.formula (Strong Hierarchy).
fit3 <- CausalANOVA(formula=won ~ newRecordF + promise + coeth_voting + relevantdegree,
                    int2.formula = ~ newRecordF:promise + newRecordF:coeth_voting
                    + promise:coeth_voting,
                    int3.formula = ~ newRecordF:promise:coeth_voting,
                    data=Carlson, pair.id=Carlson$contestresp,diff=TRUE,
                    cluster=Carlson$respcodeS, nway=3)
summary(fit3)

plot(fit3, type="AMIE", fac.name=c("newRecordF","promise", "coeth_voting"),space=25,adj.p=2.2)

## End(Not run)

## ####################################### 
## With Screening and Collapsing
## #######################################
## Sample Splitting
train.ind <- sample(unique(Carlson$respcodeS), 272, replace=FALSE)
test.ind <- setdiff(unique(Carlson$respcodeS), train.ind)
Carlson.train <- Carlson[is.element(Carlson$respcodeS,train.ind), ]
Carlson.test <- Carlson[is.element(Carlson$respcodeS,test.ind), ]

#################### AMEs and two-way AMIEs ####################
fit.r2 <- CausalANOVA(formula=won ~ newRecordF + promise + coeth_voting + relevantdegree,
                      data=Carlson.train, pair.id=Carlson.train$contestresp,diff=TRUE,
                      screen=TRUE, collapse=TRUE,
                      cluster=Carlson.train$respcodeS, nway=2)
summary(fit.r2)

## refit with test.CausalANOVA
fit.r2.new <- test.CausalANOVA(fit.r2, newdata=Carlson.test, diff=TRUE,
                               pair.id=Carlson.test$contestresp, cluster=Carlson.test$respcodeS)

summary(fit.r2.new)
plot(fit.r2.new)
plot(fit.r2.new, type="ConditionalEffect", fac.name=c("newRecordF","coeth_voting"))
ConditionalEffect(fit.r2.new, treat.fac="newRecordF", cond.fac="coeth_voting")


