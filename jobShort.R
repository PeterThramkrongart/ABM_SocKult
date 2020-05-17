## Example 2 - Sheep/Wolf Predation
Sys.setenv(JAVA_HOME = "C:\\Program Files\\NetLogo 6.0.4\\runtime")
pacman::p_load(rJava, RNetLogo, tidyverse, parallel)

library(nlexperiment)

setwd("C:/Program Files/NetLogo 6.0.4/app") #path where netlogo.jar file is stored - ymmv

path_out = "C:\\Users\\thram\\OneDrive\\cog data\\SocKult\\ABM_SocKult\\"

memory.limit(size = 65000) 
gc()
param_values <-  list(
  SF_setup_YN = TRUE,
  numlearners = 1000,
  search_distance = c(5, 10, 100),
  broadcast_freq = c(0, 5),
  prune_sd_mod = 0.1,
  censorship_mod = c(0, 0.5, 1),
  always_search_YN = TRUE,
  SF_density_mod = 2.5,
  Prior_sd = 0.25,
  prior_sample_size = 5,
  agent_prior = 0.01,
  broadcast_val = 0.5
)

nl_default_mapping(param_values)

# Set the path to your NetLogo installation
nl_netlogo_path("c:/Program Files/NetLogo 6.0.4/app")
experiment1 <- nl_experiment(
  model_file = "models/SocKult/MadsenModCleaned.nlogo",
  iterations = 200,
  param_values = param_values,
  mapping = nl_default_mapping,
  step_measures = measures(
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
  repetitions = 5,
  # repeat simulations 10 times
  random_seed = 1:5
)



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

gc()

resultCleaned <-
  nl_run(experiment1, parallel = TRUE, max_cores = 15)

gc()

ABM_DataShortAfter <-
  nl_get_result(resultCleaned, type = "agents_after", sub_type = "turtles")

ABM_DataShortAfter$step_id <- 50
ABM_DataShortBefore <-
  nl_get_result(resultCleaned, type = "agents_before", sub_type = "turtles")
ABM_DataShortBefore$step_id <- 1



distributionDataShort <- rbind(ABM_DataShortBefore, ABM_DataShortAfter)
ABM_DataShort <- nl_get_result(resultCleaned, type = "step")

ABM_DataShort <- as_tibble(ABM_DataShort)

ABM_DataShort <-
  ABM_DataShort %>% mutate_at(
    c(
      "censorship_mod",
      "prune_sd_mod",
      "broadcast_freq",
      "run_id",
      "param_set_id",
      "numlearners"
    ),as.factor
  )

ABM_DataShort$censorship_mod <-
  ABM_DataShort$censorship_mod %>% str_replace("^0$", "None")

ABM_DataShort$prune_sd_mod <-
  ABM_DataShort$prune_sd_mod %>% str_replace("^0$", "Stochastic")

ABM_DataShort$broadcast_freq <-
  ABM_DataShort$broadcast_freq %>%
  str_replace("^0$", "None")


distributionDataShort <-
  distributionDataShort %>% mutate_at(
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

distributionDataShort$censorship_mod <-
  distributionDataShort$censorship_mod %>% str_replace("^0$", "None")

distributionDataShort$prune_sd_mod <-
  distributionDataShort$prune_sd_mod %>% str_replace("^0$", "Stochastic")

distributionDataShort$broadcast_freq <-
  distributionDataShort$broadcast_freq %>%
  str_replace("^0$", "None")


write.csv(ABM_DataShort, paste(path_out,"ABM_DataShort5.csv", sep = ""))
write.csv(distributionDataShort, paste(path_out, "distributionDataShort5.csv", sep = ""))
