# function for interactive marginal effects
interaction_plot_continuous_ggplot <- function(model, effect, 
                                               moderator,
                                               interaction, 
                                               varcov="default", 
                                               minimum="min", 
                                               maximum="max", 
                                               incr="default", 
                                               num_points = 50, conf=.95, mean=FALSE, median=FALSE, alph=80, rugplot=T, histogram=T, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient"){
  
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]])
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]])
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }

  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Make the histogram color
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), xlab=xlabel, ylab=ylabel, main=title)
  
  # Plot estimated effects
  lines(y=delta_1, x=x_2)
  lines(y=upper_bound, x=x_2, lty=2)
  lines(y=lower_bound, x=x_2, lty=2)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
  # Add a vertical line at the mean
  if (mean){
    abline(v = mean(mod_frame[[moderator]]), lty=2, col="red")
  }
  
  # Add a vertical line at the median
  if (median){
    abline(v = median(mod_frame[[moderator]]), lty=3, col="blue")
  }
  
  # Add Rug plot
  if (rugplot){
    rug(mod_frame[[moderator]])
  }
  
  out = tibble(delta_1, upper_bound, lower_bound, moderator=x_2, treatment=effect)
  
  return(out)
}

# function for interactive marginal effect with a binary moderator
interaction_plot_binary <- function(model, effect, moderator, interaction, varcov="default", conf=.95, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient", factor_labels=c(0,1)){
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- c(0,1)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(-.5, 1.5), xlab=xlabel, ylab=ylabel, main=title, xaxt="n")
  
  # Plot points of estimated effects
  points(x=x_2, y=delta_1, pch=16)
  
  # Plot lines of confidence intervals
  lines(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), lty=1)
  points(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), pch=c(25,24), bg="black")
  lines(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), lty=1)
  points(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), pch=c(25,24), bg="black")
  
  # Label the axis
  axis(side=1, at=c(0,1), labels=factor_labels)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
  out = tibble(delta_1, upper_bound, lower_bound, moderator=x_2, treatment=effect)
  
  
}
# short view
short_view <- function(data){
  data %>% slice(1:50) %>% View()
}

# ggplot theme ------------------------------------------------------------
RColorBrewer::display.brewer.all()
my_font <- "Palatino"
my_bkgd <- "white"
#my_bkgd <- "#f5f5f2"
pal <- RColorBrewer::brewer.pal(9, "Spectral")


my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_rect(color="black"), 
                  strip.background = element_rect(color="black", fill="gray85"), 
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 6, fill = "white", colour = NA), 
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 14, family = my_font),
                  legend.title = element_text(size=14),
                  plot.title = element_text(size = 22, face = "bold", family=my_font),
                  plot.subtitle = element_text(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  
                  axis.text = element_text(size=14, family=my_font),
                  axis.title.x = element_text(hjust=1),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 14, face="bold"), 
                  plot.margin = margin(1, 1, 1, 1, "cm"), 
                  plot.caption = element_text(size=11))
theme_set(theme_bw() + my_theme)



# Cluster Stde ------------------------------------------------------------

cse <- function(model, cluster)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}


