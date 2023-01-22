## Replication Materials: _Voting for Law and Order: Evidence from a Survey Experiment in Mexico_

Replication materials for Ventura, Ley and Cantu "Voting for Law and Order: Evidence from a Survey Experiment in Mexico", Comparative Political Studies, 2023,

> __Abstract:__
> OIn this article, we examine the demand-and-supply dynamic of security policies. We argue there are two informational shortcuts through which voters process policy alternatives and choose among them: (1) their own personal experiences with violence and (2) candidates' profiles. We test our argument through an original survey experiment conducted in Mexico. We model voters' decisions to support candidates campaigning over a variety of security proposals. Our survey design takes advantage of recent developments in network models to better measure the effects of crime exposure on voters' preferences. We find that higher exposure to crime victimization is associated with an increased support for only some iron-fist policies, therefore highlighting the importance of unpacking security policies instead of generalizing the results of crime exposure.  We show null effects of partisan advantages and reveal the role of non-partisan heuristics, such as the candidate's professional experience, in preferences for security policies.


The latest pre-print can be found [here](). The published version is [here]()

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder **Codes**. The survey data is under the folder **data**. Results are exported to the folder **output**. 


## Codes


- `clean_survey_data.r`: this code takes the raw survey data and clean the data for the conjoint analysis. 

- `network_models.r`: this code produces the network measure of contextual victimization. The output of this code is used in the `analysis_main_paper.r` code. Result from appendix A are also generated from this code. 

- `analysis_main_paper.r`: this code generates all the figures presented in the paper. 

- `analysis_sif_survey.r`: this code generates the figures and tables presented in the supplemental analysis from appendix B to B


- `utils.R`: this code has a set of functions I use to facilitate the analysis. 


## Data

Thi repo contains three data files

- `raw_survey_data.Rdata`: contains the raw survey data used in the paper

- `processed_survey_dataa.rds`: contains the processed survey data generated from the code `clean_survey_data.r`

- `network_results.Rdata`: contains the results of the network model for victimization generated from the code `network_models.r`.

The codebook for these three data sources is available [here]("https://github.com/TiagoVentura/law_and_order_mexico_CPS/blob/main/codebook.md")

## Ackowledgments

This research is part of the \textit{Inter-American Development Bank} project: ``Transparency, trust, and Social Media'', 1300600-01-PEC. PI: Ernesto Calvo, 2019-2020. We thank Elizabeth Zechmeister, Noam Lupu, and Maita Schade from LAPOP, who coordinated the probabilistic selection of respondents from a Netquest panel of Mexican voters. We received invaluable feedback from the members of the interdisciplinary Lab for Computational Social Science (iLCSS-UMD) at University of Maryland, College Park, as well as important suggestions from Ernesto Calvo, Isabella Alcañiz, Ariel White, Lucía Tiscornia, Gustavo Flores-Macías, and Giancarlo Visconti.


## Session Info

    sessionInfo()

    ## R version 4.0.3 (2020-10-10)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 20.04.2 LTS
    ##
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
    
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
    ##  [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
    ##  [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    
    ## loaded via a namespace (and not attached):
    ## [1] compiler_4.0.3  fastmap_1.1.0   cli_3.2.0       htmltools_0.5.2 tools_4.0.3     yaml_2.3.5   ## [7] rmarkdown_2.10  knitr_1.37      xfun_0.30       digest_0.6.29   rlang_1.0.2     evaluate_0.15  
