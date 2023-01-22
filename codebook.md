# Codebook for survey 

Below we describe the main variables used in the conjoint analysis. 

## Conjoint Variables

- responseid: survey respondents' id
- task: task of the conjoint (task 1 or task 2)
- candidate: candidate in the pair of conjoint(1 or 2)
- feat_political_party: conjoint feature political party
- feat_security_proposal: conjoint feature security proposal
- feat_occupation: conjoint feature occupation
- feat_gender: conjoint feature gendr
- outcome: candidate chosen by the respondent
- outcome_num: outcome recoded as numeric, where 0 means did not vote for this candidate, and 1 means                 voted for this candidate

## Network Variables

#### Processed data:

- yhat_network_victim_crime: prediction of the network model for crime victimization
- yhat_network_police_violence: prediction of the network model for police victimization
- res_network_victim_crime: residuals of the network model for crime victimization
- res_network_police_violence: residuals of the network model for police victimization

#### Raw data: Responses for the question "How many people you know that..."

- network_silvia: First name is Silvia 
- network_antonio: First name is Antonio
- network_police: Work as Police Officers
- network_medicos: Work as Physicians
- network_hijo:  Had a kid last year        
- network_died:   Had died last year       
- network_married: Got married last year
- network_covid: Have been hospitalized for the coronavirus
- network_public_employee:Work as Government Employees
- network_social_work: Carry out social work in poor neighborhoods
- network_partisan: Are members of a political party
- network_candidate:Have been political candidates for your municipality or province\
- network_victim_crime:  Have been victims of a crime, such as assault, robbery, or injury
- network_police_violence: Have been victims of Police abuse
- network_prision: Who are currently in prison
- network_job_covid: Have lost their jobs as a result of the current coronavirus crisis.

## Covariates

- negative_partisanship: survey measure for negative partisanship
- positive_partisanship: survey measure for positive partisanship
- income: subjective survey measure for personal income
- gender: survey measure for respondents' gender
- work: survey response for employent situation
- age: survey response for respondents' age
- education: survey response for respondents' educational levels
- crime_victimization: self-reported crime victimizatin
- police victimization: self-reported police victimization
- trust_police: survey measure for trust in local police
- issue_security: issue importance measure for security as a personal concern
- fear_x: battery of questions for fear of crime
- po_criminal_rights: survey measure for agreement with the restrictions of rights for people who committed crimes. 

