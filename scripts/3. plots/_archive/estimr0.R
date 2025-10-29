# Clear matrix #
#--------------#
rm(list = ls())
# Set working-directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# packages
library(dplyr)
library(EpiEstim)

# https://www.repidemicsconsortium.org/earlyR/

#-----------------------#
# set working directory #
setwd("../../data/analyses/") # for this chunk open data/analyses/ folder in relative path
# rki <- read.csv2("DataAnalysisRKI.csv") # With Policy Measures
rki <- read.csv2("DataAnalysisRKI.csv") # "Pure" cases
rki$state <- gsub("Ã¼", "ue", as.character(rki$state)) # fix umlaute in state names

  # Make report date a real date  
  rki$reportdate <- as.Date(rki$reportdate, "%Y-%m-%d")

# Data per state (daily cases and cummulative number)
rki_states <- rki %>%
  group_by(state,reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))

rki_states$cumcases <- 1
for(i in 2:nrow(rki_states)){
  if(rki_states$state[i] == rki_states$state[i-1]){
  rki_states$cumcases[i] <- rki_states$cases[i] + rki_states$cumcases[i-1]
  }
  else {
    rki_states$cumcases[i] <- rki_states$cases[i]
  }
}

# First report date per state 
rki_state_start <- rki_states %>%
  group_by(state) %>%
  summarize(firstreportdate = min(reportdate))

# When first ten cases happened 
rki_state_start_10 <- subset(rki_states, rki_states$cumcases >= 10)
rki_state_start_10 <- rki_state_start_10 %>%
                          group_by(state) %>%
                          summarize(firstsinceten = min(reportdate))

# Merge this information (fist case; first ten cases) to state data
rki_states <- merge(rki_states,rki_state_start, by = "state")
rki_states <- merge(rki_states,rki_state_start_10, by = "state")
rki_states$dayssincefirst <- rki_states$reportdate - rki_states$firstreportdate
rki_states$dayssinceten <- rki_states$reportdate - rki_states$firstsinceten
rki_states$firstreportdate <- NULL
rki_states$firstsinceten <- NULL



# Create state data #
#-------------------#
states <- unique(rki_states$state)
startdate <- as.Date("2020-03-01", "%Y-%m-%d")

# According to the RKI - 04/08/2020 #
#-----------------------------------#
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText4
#-----------------------------------------------------------------
# Das serielle Intervall definiert das durchschnittliche Intervall 
# vom Beginn der Erkrankung eines ansteckenden Falles bis zum 
# Erkrankungsbeginn eines von diesem angesteckten Falles. Das 
# serielle Intervall ist meistens länger als die Inkubationszeit, 
# weil die Ansteckung im Allgemeinen erst dann erfolgt, wenn ein 
# Fall symptomatisch geworden ist. Das serielle Intervall lag in 
# einer Studie mit 425 Patienten im Mittel (Median) bei 7,5 (29) 
# und in einer anderen Studie bei geschätzten vier Tagen, 
# basierend auf der Analyse von 28 Infizierenden/Infizierten-Paaren (30).

# parameter values by source
#configr <- make_config(method = "parametric_si", mean_si = 7.5, std_si = 4.75) # RKI
configr <- make_config(method = "parametric_si", mean_si = 4, std_si = 4.75)    # https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 

for(i in 1:length(states)){
  tempstate <- subset(rki_states, rki_states$state %in% states[i])
  tempstate <- select(tempstate, reportdate, cases)
  tempstate <- subset(tempstate, tempstate$reportdate >= startdate)
  
  # Estimate R0 #
  #-------------#
  er <- estimate_R(tempstate[,"cases"]
                   , method = "parametric_si"
                   , config = configr) # one of 4
  # Save Output #
  output <- as.data.frame(er$R)
  output$date <- tempstate$reportdate[2:(nrow(output)+1)]
  write.csv2(er$R, paste0("../rnull/",states[i],"_r.csv"))
}

# Estimate R0 #
###############

test <- subset(rki_states, rki_states$state == state[1])
#colnames(test)[3] <- "I"
test <- test[,c(2:3)]
sdate <- as.Date("2020-03-01", "%Y-%m-%d")
test <- subset(test, test$reportdate >= sdate)

# covid <- rnorm(1000, mean = 7, sd = 2.5)


# According to the RKI - 04/08/2020 #
#-----------------------------------#
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText4
#-----------------------------------------------------------------
# Das serielle Intervall definiert das durchschnittliche Intervall 
# vom Beginn der Erkrankung eines ansteckenden Falles bis zum 
# Erkrankungsbeginn eines von diesem angesteckten Falles. Das 
# serielle Intervall ist meistens länger als die Inkubationszeit, 
# weil die Ansteckung im Allgemeinen erst dann erfolgt, wenn ein 
# Fall symptomatisch geworden ist. Das serielle Intervall lag in 
# einer Studie mit 425 Patienten im Mittel (Median) bei 7,5 (29) 
# und in einer anderen Studie bei geschätzten vier Tagen, 
# basierend auf der Analyse von 28 Infizierenden/Infizierten-Paaren (30).

# parameter values by source
#configr <- make_config(method = "parametric_si", mean_si = 7.5, std_si = 4.75) # RKI
configr <- make_config(method = "parametric_si", mean_si = 4, std_si = 4.75)    # https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 

#method = "uncertain_si", n1 = 500, n2 = 100, mean_si = 5, std_si = 2.5, std_mean_si = 5, min_mean_si = 1, max_mean_si = 15, std_std_si = 2.5, min_std_si = 1, max_std_si = 3.25)
er <- estimate_R(test[,2]
                 , method = "parametric_si"
                 , config = configr) # one of 4
}

summary(er)
plot(er$R)













#write.csv2(rki_states,"DataAnalysisRKIstates.csv")


#------#
# SEIR #
#------#

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# define parameter values
parameters_values <- c(
  beta  = 0.004, # infectious contact rate (/person/day)
  gamma = 0.5    # recovery rate (/day)
)

# set initial values for S, I, R
initial_values <- c(
  S = 999,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

# time
time_values <- seq(0, 10) # days

sir_values_1 <- deSolve::ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_values_1 <- as.data.frame(sir_values_1)

# plot
with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})
# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

# R0
(999 + 1) * parameters_values["beta"] / parameters_values["gamma"]










test <- subset(rki_states, rki_states$state == state[1])
colnames(test)[3] <- "I"
test <- test[,c(2:3)]
sdate <- as.Date("2020-01-01", "%Y-%m-%d")
test[1,] <- c((test[1,1]-1),0)

#install.packages("growthrates")
fit <- growthrates::fit_easylinear(test$reportdate, test$I)
growthrates::coef(fit)
growthrates::rsquared(fit)
growthrates::deviance(fit)

par(mfrow = c(1, 2))
growthrates::plot(fit, log = "y")
growthrates::plot(fit)

fit <- growthrates::all_easylinear(cumcases ~ reportdate | state, data = rki_states, h = 12)
growthrates::fit


# R0
# growth rate r
r <- 0.1 # Jan 4th Wuhan
# r ~ N(0.1, 0.028)
# serial interval (mu, sigma) ~ N(M,E) where M = (3.96, 4.75)
mu <-
sigma <- 

R0 <- exp(r * mu - 0.5 * r^2 * sigma^2)

#Loading package
#install.packages("R0")
library(R0)

# estimate R0.GT object that complies with generation.time distribution requirements
GT <- est.GT(infector.onset.dates = dat$symp1
      ,infectee.onset.dates = dat$symp2)


mGT<-generation.time("empirical", dat$symp)

estimate.R(epid = test# dataset
          ,GT     # Generation Time repartition function
          ,t = test$reportdate # datevector
          ,begin = # begin date YYYY-mm-dd 
          ,end = # end date
          #,date.first.obs = # optional
          #,time.step = 1 # optional, number of day between each obs
          #,AR = # attack rate as a percentage from total pop
          #,pop.size = # population size in which the incident cases were observed
          #,S0 = # initial proportion of the population considered susceptible
          #,methods = c("EG","ML","AR","TD","SB")# list of methods used for R0
        )

#install.packages("EpiEstim")
require(EpiEstim)

#est.GT()

test <- subset(rki_states, rki_states$state == state[1])
colnames(test)[3] <- "I"
test$I <- as.character
test <- test[,c(2:3)]
sdate <- as.Date("2020-01-01", "%Y-%m-%d")
test[1,] <- c((test[1,1]-1),0)

covid <- rnorm(1000, mean = 7, sd = 2.5)

# ?make_config
configr <- make_config(method = "parametric_si", mean_si = 7, std_si = 2.5)
  
#method = "uncertain_si", n1 = 500, n2 = 100, mean_si = 5, std_si = 2.5, std_mean_si = 5, min_mean_si = 1, max_mean_si = 15, std_std_si = 2.5, min_std_si = 1, max_std_si = 3.25)
er <- estimate_R(test[,2]
                 , method = "parametric_si"
                 , config = configr) # one of 4
summary(er)
                 
                 
                 
                 
                 
                 
                 
                 
                 , t = reportdate            # date vector
                 , time.step = 1             # Optional.  If date of first observation is specified, number of day between eachincidence observation
                 #, si_sample = test[,2])

states <- unique(rki_states$state)
for(i in 1:length(states)){
  estimate_R(subset(rki_states, rki_states$state == state[i]), method = "", rki_states)
}


estimate_R()
?estimate_R


library(EpiEstim)
plot_Ri <- function(estimate_R_obj) {
  p_I <- plot(estimate_R_obj, "incid", add_imported_cases = TRUE)  # plots the incidence
  p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
  p_Ri <- plot(estimate_R_obj, "R")
  return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
}

hubei_res_parametric_si <- estimate_R(hubei_confirmed_cases, 
                                      method = "parametric_si", config = make_config(list(mean_si = 7.5, 
                                                                                          std_si = 3.4)))

plot_Ri(hubei_res_parametric_si)


setwd("../../data/analyses/") # for this chunk open data/analyses/ folder in relative path
rki <- read.csv2("DataAnalysisRKI.csv")


#rki <- subset(rki, gsub("-", "", as.character(rki$reportdate)) <= 20200316)
rki <- subset(rki, select = c("reportdate","cases"))
rki$reportdate <- as.Date(rki$reportdate, "%Y-%m-%d")
rki <- rki[order(rki$reportdate),]
#rki$reportdate_num <- as.numeric(gsub("-","",as.character(rki$reportdate)))
