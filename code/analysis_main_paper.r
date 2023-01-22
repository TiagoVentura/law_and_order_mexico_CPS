##############################################################################
# File-Name: analysis_main_paper.r
# Date: January 21 2023
# author: Tiago Ventura 
# Purpose: this code generates all the figures presented in the paper. 
# Data in: "data/processed_survey_data.rds"
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.2 LTS
##############################################################################


# Packages ----------------------------------------------------------------
library(here)
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


# Open data ---------------------------------------------------------------
conjoint_data =read_rds(here("data", "processed_survey_data.rds"))

# Modeling ----------------------------------------------------------------
features = c("political_party", "gender", "occupation", "security_proposal")
references <- c(levels(conjoint_data$feat_political_party)[1],
                levels(conjoint_data$feat_gender)[1],
                levels(conjoint_data$feat_occupation)[1], 
                levels(conjoint_data$feat_security_proposal)[1])

list_terms <- c("Political Party", paste0("    ", names(levels_party)), " ",
                "Gender", paste0("    ", names(levels_gender)),"  ", 
                "Occupation", paste0("    ", names(levels_occup)), "   ",
                "Security Proposal", paste0("    ", names(levels_sec)), "    ")

# function to plot the conjoint results
tidy_conjoint_model <- function(model, subsample, features=features,
                                references=references, list_terms){
  
  
  
  # to make it more general, it is just to change the  4 for the number of features
  model %>% 
    tidy(.) %>% 
    mutate(lb=estimate - 1.96*std.error, 
           up= estimate + 1.96*std.error, 
           lb90=estimate - 1.64*std.error, 
           up90= estimate + 1.64*std.error) %>% 
    filter(!(term %in% c("(Intercept)"))) %>% 
    mutate(term=str_remove_all(term, paste0("feat_", features, collapse = "|")),
           term=str_replace_all(term, "_", " "),
           term=as.factor(term), 
           term=str_c("    ", term)) %>%
    bind_rows(data_frame(term=c(str_to_title(str_replace_all(features, "_", " ")), 
                                " ", "  ", "   ", "    ",
                                paste0("    ", references)), 
                         estimate=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         std.error=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         statistic=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         p.value=c(rep(NA, 4), rep(NA, 4), rep(0,4)), 
                         lb=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         up=c(rep(NA, 4), rep(NA, 4), rep(0,4)), 
                         lb90=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         up90=c(rep(NA, 4), rep(NA, 4), rep(0,4)))) %>% 
    mutate(subsample=subsample, 
           term=fct_relevel(term, list_terms), 
           term=fct_rev(term))
}

# Create the main moderators

conjoint_data <- conjoint_data %>%
  mutate(cv=crime_victimization, 
         fs=feat_security_proposal, 
         fo=feat_occupation) 



# Figure 1: Average Marginal Component Effects ----------------------------------------------------------------
library(estimatr)

# model
model <- lm_robust(outcome_num~ feat_political_party + feat_gender + 
                     feat_security_proposal + feat_occupation, data=conjoint_data, 
                   clusters = responseid,
                   se_type = "stata") 

# extract results
res <- tidy_conjoint_model(model=model, subsample="all", features = features,
                           references=references, list_terms = list_terms) %>%
  mutate(title="AMCE")


# plot
ggplot(res, aes(y=estimate, x=term, 
                ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "") +
  geom_hline(yintercept = 0, color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=14,hjust=0, face="bold"), 
        strip.text = element_text(size=16),
        panel.background = element_rect(fill = "gray95", color = NA), 
        strip.background = element_rect(fill="white"), 
        panel.grid.major = element_line(color = "white")) +
  facet_grid(~title) +
  geom_vline(xintercept = 14, linetype="dashed", color="black",alpha = .5) + #project
  geom_vline(xintercept = 18, linetype="dashed",  color="black",alpha = .5) +
  geom_vline(xintercept = 24, linetype="dashed", color="black", alpha = .5) +
  geom_vline(xintercept = 7, linetype="dashed", color="black",alpha = .5) #gov  


ggsave(filename=here("output", "fig1.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Figure 2A:  ---------------------------------------------------------
# remove nas from raw network data
conjoint_data <- conjoint_data %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999"))))

# load results from network models (see code network_models.r)
load(here("data", "network_results.Rdata"))

# convert to wide format
crime_wide <- crime %>% 
  pivot_wider(names_from=group, 
              values_from = c(yhat, res, values))
crime_wide$res_network_police_violence
# merge with network
conjoint_data <- left_join(conjoint_data, crime_wide)


# Model for  for interactive effects

# with security proposal
mod_fs <- conjoint_data %>% 
  mutate(nvc=res_network_victim_crime) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              nvc*fs + fo, 
            data=., clusters = responseid)

# with occupation
mod_fo <- conjoint_data %>% 
  mutate(nvc=res_network_victim_crime) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              fs + nvc*fo, 
            data=., clusters=responseid)



# Get names: Constitutive Terms
list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("nvc:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("nvc:", list_ct_fo))



# Extract results for graph

mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      mod_fs, 
                      effect = .x, 
                      moderator = "nvc", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      mod_fo, 
                      effect = .x, 
                      moderator = "nvc", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))


# Plot Marginal Effects Interactive

ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="", 
       y="Marginal Effect", 
       x="Overdispersion of Crime in the Respondents Network", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 

 
ggsave(filename=here("output", "fig2a.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Overdispersion of Crime in the Respondents Network", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 
 
 ggsave(filename=here("output", "fig2b.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Figure 3 ----------------------------------------------------------------

# recode partisanship variables

conjoint_data <- conjoint_data %>% 
   mutate(negative_partisanship_recode=ifelse(str_detect(negative_partisanship, "MORENA") &
                                              str_detect(positive_partisanship, "Ninguno|No responde|No sabe"),
                                              "Anti-Morena", 
                                              ifelse(str_detect(negative_partisanship, "PRI") & 
                                                     str_detect(positive_partisanship, "Ninguno|No responde|No sabe"),
                                                     "Anti-Pri",
                                              ifelse(str_detect(negative_partisanship, "PAN") &
                                                      str_detect(positive_partisanship, "Ninguno|No responde|No sabe"), 
                                                      "Anti-Pan", NA))))
 
conjoint_data <- conjoint_data %>%
                   mutate(positive_partisanship_recode=ifelse(
                           str_detect(positive_partisanship, "(MORENA)"), 
                                         "MORENA Voters", 
                                         ifelse(str_detect(positive_partisanship, "(PAN)"), 
                                                "PAN Voters",
                                                ifelse(str_detect(positive_partisanship, "(PRI)"), 
                                                       "PRI Voters", "Independents"))))
                                     
# function to adjust the graph

reframe_graph <- function(data, levels_gender=levels_gender, 
                          levels_party=levels_party, 
                          levels_occup=levels_occup, 
                          levels_sec=levels_sec){
  
  data %>%
    mutate(term_c=str_trim(term), 
           features=ifelse(term_c%in% names(levels_gender), 
                           "Gender", 
                           ifelse(term_c%in% names(levels_party), 
                                  "Party", 
                                  ifelse(term_c %in% names(levels_occup), 
                                         "Occupation",
                                         ifelse(term_c%in% names(levels_sec), 
                                                "Security Proposal", NA)))), 
           features=fct_relevel(features, "Party", "Gender", 
                                "Occupation", "Security Proposal")) 
} 
 
# Interactive Effects By Features -----------------------------------------
res_int <- conjoint_data %>%
  nest_by(feat_political_party) %>% 
  drop_na() %>%
  mutate(model=map2(data, feat_political_party, 
                    ~ lm_robust(outcome_num~ feat_security_proposal + feat_gender +
                                  feat_occupation, 
                                data=.x, 
                                clusters = responseid) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model) 


res_int_r <- reframe_graph(data=res_int, levels_gender, 
                           levels_party, levels_occup, levels_sec)  %>%
  drop_na() %>%
  filter(!str_detect(term, "Victims|Independents|Public"))



# Figure 3a
res_int_r <- res_int_r %>%
  mutate(term=str_trim(str_replace_all(term, "/", " - \n")), 
         term = fct_recode(term, "Education to  \n Youth"="Education to  Youth" , 
                           "Police \n Militarization"="Police  Militarization"))

ggplot(data=res_int_r %>% filter(features=="Security Proposal"), 
       aes(y=estimate, x=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Average Component Interactive Effects (ACIE)", 
       title = "Candidate Choice Conjoint Estimates", 
       caption="Reference Categories (Zero Red Line in the Graphs) is Victims Oriented Policies.") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~term) +
  theme(axis.text.y=element_text(size=16), 
        strip.text = element_text(size=13))

ggsave(filename=here("output", "fig3a.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Interactive Effects By Features -----------------------------------------

# Occupation and Proposal

res_int <- conjoint_data %>%
  nest_by(feat_occupation) %>% 
  drop_na() %>%
  mutate(model=map2(data, feat_occupation, 
                    ~ lm_robust(outcome_num~ feat_security_proposal + feat_gender +
                                  feat_political_party, 
                                data=.x, 
                                clusters = responseid) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model) 


res_int_r <- reframe_graph(data=res_int, levels_gender, 
                           levels_party, levels_occup, levels_sec)  %>%
  drop_na() %>%
  filter(!str_detect(term, "Victims|Independents|Public"))

# ACIE for Proposal 

res_int_r <- res_int_r %>%
  mutate(term=str_trim(str_replace_all(term, "/", " - \n")), 
         term = fct_recode(term, "Education to  \n Youth"="Education to  Youth" , 
                           "Police \n Militarization"="Police  Militarization"))

ggplot(data=res_int_r %>% filter(features=="Security Proposal"), 
       aes(y=estimate, x=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Average Component Interactive Effects (ACIE)", 
       title = "Candidate Choice Conjoint Estimates", 
       caption="Reference Categories (Zero Red Line in the Graphs) is Victims Oriented Policies.") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~term) +
  theme(axis.text.y=element_text(size=14), 
        strip.text = element_text(size=13))


ggsave(filename=here("output", "fig3b.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# sesssion
sessionInfo()
  # 
  # R version 4.0.3 (2020-10-10)
  # Platform: x86_64-pc-linux-gnu (64-bit)
  # Running under: Ubuntu 20.04.2 LTS
  # 
  # Matrix products: default
  # BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
  # LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
  # 
  # locale:
  #   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
  # [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
  # [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
  # [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
  # 
  # attached base packages:
  #   [1] stats     graphics  grDevices utils     datasets  methods   base     
  # 
  # other attached packages:
  #   [1] modelsummary_0.8.0   stargazer_5.2.2      estimatr_0.28.0      broom_0.7.7         
  # [5] extrafont_0.17       patchwork_1.0.1      rebus_0.1-3          conflicted_1.0.4    
  # [9] scales_1.1.1         lubridate_1.8.0      summarytools_0.9.6   forcats_0.5.0       
  # [13] stringr_1.4.0        dplyr_1.0.8          purrr_0.3.4          readr_2.1.2         
  # [17] tidyr_1.2.0          tibble_3.1.6         ggplot2_3.3.5        tidyverse_1.3.0.9000
  # [21] here_1.0.1          
  # 
  # loaded via a namespace (and not attached):
  #   [1] colorspace_2.0-3      pryr_0.1.4            ellipsis_0.3.2        rio_0.5.16           
  # [5] rprojroot_2.0.2       parameters_0.11.0     base64enc_0.1-3       fs_1.5.0             
  # [9] rstudioapi_0.13       farver_2.1.0          fansi_1.0.2           xml2_1.3.3           
  # [13] codetools_0.2-16      cachem_1.0.5          knitr_1.37            wesanderson_0.3.6    
  # [17] texreg_1.37.5         Formula_1.2-4         jsonlite_1.8.0        Rttf2pt1_1.3.8       
  # [21] dbplyr_2.1.1          compiler_4.0.3        httr_1.4.2            backports_1.4.1      
  # [25] assertthat_0.2.1      fastmap_1.1.0         cli_3.2.0             htmltools_0.5.2      
  # [29] tools_4.0.3           gtable_0.3.0          glue_1.6.2            rebus.base_0.0-3     
  # [33] tables_0.9.6          Rcpp_1.0.8            carData_3.0-4         jquerylib_0.1.4      
  # [37] cellranger_1.1.0      vctrs_0.3.8           extrafontdb_1.0       insight_0.12.0       
  # [41] xfun_0.30             rebus.datetimes_0.0-1 openxlsx_4.1.5        rvest_1.0.2          
  # [45] lifecycle_1.0.1       pacman_0.5.1          rebus.numbers_0.0-1   hms_1.1.1            
  # [49] RColorBrewer_1.1-2    yaml_2.3.5            curl_4.3.2            memoise_2.0.0        
  # [53] pander_0.6.3          sass_0.4.0            stringi_1.7.6         highr_0.9            
  # [57] bayestestR_0.8.0      checkmate_2.0.0       zip_2.1.1             rlang_1.0.2          
  # [61] pkgconfig_2.0.3       matrixStats_0.57.0    evaluate_0.15         rapportools_1.0      
  # [65] labeling_0.4.2        tidyselect_1.1.2      plyr_1.8.6            magrittr_2.0.2       
  # [69] R6_2.5.1              magick_2.4.0          generics_0.1.2        DBI_1.1.2            
  # [73] pillar_1.7.0          haven_2.3.1           foreign_0.8-79        withr_2.5.0          
  # [77] abind_1.4-5           rebus.unicode_0.0-2   performance_0.6.1     modelr_0.1.8         
  # [81] crayon_1.5.0          car_3.0-9             utf8_1.2.2            tzdb_0.2.0           
  # [85] rmarkdown_2.10        grid_4.0.3            readxl_1.3.1          data.table_1.14.2    
  # [89] webshot_0.5.2         reprex_2.0.1          digest_0.6.29         munsell_0.5.0        
  # [93] viridisLite_0.4.0     kableExtra_1.2.1      bslib_0.2.5.1         tcltk_4.0.3 