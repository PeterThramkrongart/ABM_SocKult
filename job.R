## Example 2 - Sheep/Wolf Predation
Sys.setenv(JAVA_HOME = "C:\\Program Files\\NetLogo 6.0.4\\runtime")
pacman::p_load(rJava, RNetLogo, tidyverse, parallel)

library(nlexperiment)

setwd("C:/Program Files/NetLogo 6.0.4/app") #path where netlogo.jar file is stored - ymmv

path_out = "C:\\Users\\thram\\OneDrive\\cog data\\SocKult\\ABM_SocKult\\"



### This script is used for running multiple combinations of simulations through netlogo

#setting limit for the amount of ram allowed to be used 
memory.limit(size = 65000)#65000 mb... that is quite a lot, may be lowered
gc()#clearing space...

#list of all parameters to be run
param_values <-  list(
  SF_setup_YN = TRUE,
  numlearners = c(100,1000),
  search_distance = c(5, 10, 100),
  broadcast_freq = c(0, 5),
  prune_sd_mod = c(0, 0.1, 1, 2),
  censorship_mod = c(0, 0.5, 1, 2),
  always_search_YN = TRUE,
  SF_density_mod = 2.5,
  Prior_sd = 0.25,
  prior_sample_size = 5,
  agent_prior = 0.01,
  broadcast_val = 0.5
)

nl_default_mapping(param_values)

## Not run:
# Set the path to your NetLogo installation
nl_netlogo_path("c:/Program Files/NetLogo 6.0.4/app")

#here we define the nl_experiment to be run.
experiment1 <- nl_experiment(
  model_file = "models/SocKult/MadsenModCleaned.nlogo",
  iterations = 50,#n ticks
  param_values = param_values,
  mapping = nl_default_mapping,
  step_measures = measures(# here we specify the measures to be taken from netlogo
    glob_p_h = "glob-p-h",
    glob_purity = "glob-purity",
    glob_prior = "glob-prior",
    glob_conn_Av = "glob-conn-Av",
    glob_var = "glob-var",
    glob_sd = "glob-sd",
    glob_p_h_sd = "glob-p-h-sd",
    glob_p_h_var = "glob-p-h-var"
  )
  ,
  repetitions = 10,
  # repeat simulations 10 times
  random_seed = 1:10
)


#adding reports from each agent from the first and last tick
experiment1 <-
  nl_set_agent_reports(
    experiment1,
    agents_before = list(turtles = agent_set(
      vars = c("who", "my-p-h", "prior-val"),
      agents = "turtles"
    )),
    agents_after = list(turtles = agent_set(
      vars = c("who", "my-p-h", "prior-val"),
      agents = "turtles"
    ))
  )
#clean again...
gc()
#running the experiment.... This took 8 or some hours I think on 15 cores, and used around 30gb of ram...
resultCleaned <-
  nl_run(experiment1, parallel = TRUE, max_cores = 15)

gc()#cleaning again...

#grapping thge results from tick 50
ABM_DataAfter <-
  nl_get_result(resultCleaned, type = "agents_after", sub_type = "turtles")

ABM_DataAfter$step_id <- 50
#results from tick 1
ABM_DataBefore <-
  nl_get_result(resultCleaned, type = "agents_before", sub_type = "turtles")
ABM_DataBefore$step_id <- 1



distributionData <- rbind(ABM_DataBefore, ABM_DataAfter) #putting the data together to look at the distributions later)
ABM_Data <- nl_get_result(resultCleaned, type = "step")#grapping general results from each_tick

ABM_Data <- as_tibble(ABM_Data)

ABM_Data <-
  ABM_Data %>% mutate_at(
    c(
      "censorship_mod",
      "prune_sd_mod",
      "broadcast_freq",
      "run_id",
      "param_set_id",
      "numlearners"
    ),as.factor
  )

ABM_Data$censorship_mod <-
  ABM_Data$censorship_mod %>% str_replace("^0$", "None")

ABM_Data$prune_sd_mod <-
  ABM_Data$prune_sd_mod %>% str_replace("^0$", "Stochastic")

ABM_Data$broadcast_freq <-
  ABM_Data$broadcast_freq %>%
  str_replace("^0$", "None")


distributionData <-
  distributionData %>% mutate_at(
    c(
      "censorship_mod",
      "prune_sd_mod",
      "broadcast_freq",
      "run_id",
      "param_set_id",
      "numlearners"
    ),
    as.factor
  )

distributionData$censorship_mod <-
  distributionData$censorship_mod %>% str_replace("^0$", "None")

distributionData$prune_sd_mod <-
  distributionData$prune_sd_mod %>% str_replace("^0$", "Stochastic")

distributionData$broadcast_freq <-
  distributionData$broadcast_freq %>%
  str_replace("^0$", "None")


write.csv(ABM_Data, paste(path_out,"ABM_Data10.csv", sep = ""))
write.csv(distributionData, paste(path_out, "distributionData10.csv", sep = ""))
