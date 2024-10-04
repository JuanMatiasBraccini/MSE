# source: https://github.com/nmfs-fish-tools/SSMSE?tab=readme-ov-file#installing-the-ssmse-r-package

# Objective: to test how well we can achieve a performance metric under uncertainty in the OM
# Scenario 1: Steepness is specified correctly in the EM. OM (h=0.65), EM (h=0.65)
# Scenario 2: Steepness is specified incorrectly in the EM. OM (h=0.1), EM (h=0.65)
# Some process error added to the OM
# Run MSE loop for 6 years with stock assessment done every 3 years and forecasting catch to
#    maintain 40% of unfished spawning stock biomass (management objective)
# The model last year is 100 so the OM is initially conditioned through year 100.
# Then, assessments will occur in years 100 and 103.
# The OM runs through year 106.

#remotes::install_github("nmfs-fish-tools/SSMSE")
library(SSMSE)
library(r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE
library(tictoc)
library(tidyverse)
library(tidyr) 
library(dplyr)

# Create a folder for the output in the working directory-------------------------------------------------------------------------
fn.user=function(x1,x2)paste(x1,Sys.getenv("USERNAME"),x2,sep='/')
if(!exists('handl_OneDrive')) source(fn.user(x1='C:/Users',
                                             x2='OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R'))
out.path=handl_OneDrive("Analyses/MSE/SSMSE")  #location of MSE input and output files

run_SSMSE_dir <- file.path(paste(out.path,"run_SSMSE-ex",sep='/'))
if(!dir.exists(run_SSMSE_dir))dir.create(run_SSMSE_dir)

# Create the OM-------------------------------------------------------------------------
# Scenario 1: already included as external package data in SSMSE
# Scenario 2: Need to create OM (use Scenario 1, change h value and save as "cod_SR_BH_steep_1")
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
develop_OMs(OM_name = "cod", out_dir = run_SSMSE_dir, par_name = "SR_BH_steep",   #Change steepness value
            par_vals = 1, refit_OMs = FALSE, hess = FALSE) #refit_OM = FALSE as the model is not being refit, just run through without fitting)
cod_1_path <- file.path(run_SSMSE_dir, "cod_SR_BH_steep_1") # path for OM model for scenario 2

# Add process error thru recruitment deviates and time-varying selectivity------------------------------------------
#note: Recruitment deviations, implementation error, and changes in parameters in the projection period of the OM 
#      can be added through the 'future_om_list' input to 'run_SSMSE'

# 1st, set up the list to add recruitment deviations in the projection period. 
# The same recruitment deviation patterns are used across scenarios, but different 
# patterns are used across iterations in the same scenario. We also want these deviations to 
# have the same standard deviations as the historical deviations with 0 mean

# 1. Rec Devs. Start from a list created by a helper function
template_mod_change <- create_future_om_list()
# add recruitment deviations
rec_dev_specify <- template_mod_change[[1]]
rec_dev_specify$pars <- "rec_devs" # apply change to rec devs
rec_dev_specify$scen <- c("replicate", "all")
# using 1 to 100 means the sd or mean will be calculated by taking the sd across years from 1 to 100
rec_dev_specify$input$first_yr_averaging <- 1
rec_dev_specify$input$last_yr_averaging <- 100
# The following 2 lines suggest that this change is immediately applied in year 101, with no transitory period 
#     for using sd 0 to the new sd.
rec_dev_specify$input$last_yr_orig_val <- 100
rec_dev_specify$input$first_yr_final_val <- 101
rec_dev_specify$input$ts_param <- "sd" # this change is for the sd
# no input value needed since it will be calclated from the historical rec devs.
rec_dev_specify$input$value <- NA

# 2. Selectivity. 
#note: allow selectivity to vary annually for 1 selectivity parameter of the fishery throughout 
#       the projection period
#put together the change for selectivity (random values around the orig val, with an sd of 0.2)
mod_change_sel <- template_mod_change[[1]]
mod_change_sel$scen[2] <- "all" # apply to all scenarios
# The following 2 lines suggest that this change is immediately applied in year
# 101, with no transitory period for using sd 0 to the new sd.
# historical values are NA in this case, because they are not used to determine
# the sd to use.
mod_change_sel$input$last_yr_orig_val <- 100
mod_change_sel$input$first_yr_final_val <- 101
mod_change_sel$input$ts_param <- "sd" # this change is for the sd
mod_change_sel$input$value <- 0.2 # set to use in the projection period

# 3. Add these two changes together into an object to pass to run_SSMSE
future_om_list_recdevs_sel <- list(rec_dev_specify,mod_change_sel)

# Add observation error through sampling from OM------------------------------------------
# The argument sample_struct specifies the structure for sampling from the OM (and passing to the EM). 
# The function create_sample_struct can be used to construct a simple sampling structure consistent with an input data file
# create_sample_struct identifies sampling patterns from the historical period of the OM and replicates 
#  those patterns in the projection period

datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
sample_struct_1_scen <- create_sample_struct(dat = datfile, nyrs = 6) # note warning
sample_struct_1_scen$lencomp <- NULL # don't use length sampling due to NA
sample_struct_list_all <- list("h-ctl" = sample_struct_1_scen, "h-1" = sample_struct_1_scen) #use same sampling structure for both scenarios


# Management Procedure------------------------------------------
# Use the same management procedure for the 2 scenarios:
  # 1. Conduct a stock assessment every 3 years to get stock status.
  # 2. Project from this stock assessment using the SS3 forecast file to get projected future catch.
  # 3. Put this projected catch (without implementation error, in the case of this example) back into the OM. 
  #   Extend the OM forward in time to get the true values for the population.
fore <- r4ss::SS_readforecast(system.file("extdata", "models", "cod", "forecast.ss", package = "SSMSE"),verbose = FALSE)
fore$Forecast
# 3 
fore$Btarget
# 0.4
# 3  means our forecasts from the assessment will use fishing mortality (F) to attempt to achieve  
          #   a relative (to unfished) spawning stock biomass of 40%


#the control rule fore$BforconstantF and fore$BfornoF values are set low to make it unlikely that they will be used
fore$BforconstantF; fore$BfornoF
fore$Flimitfraction #set to 1 so that the forecasted catch is set equal to the overfishing limit (for simplicity)
fore$Nforecastyrs #However, an assessment will be conducted every 3 years and thus 3 years of projections is required. 
      # SSMSE will automatically modify this value in the estimation model to the appropriate number of forecasting years


# Run SSMSE------------------------------------------
#create a directory to store our results, and use run_SSMSE to run the MSE analysis loop (note this will take some
#  time to run, ~ 20 min
run_res_path <- file.path(run_SSMSE_dir, "results")
if(!dir.exists(run_res_path)) dir.create(run_res_path)
niters=2 # ... In a real MSE analysis, run 100+ iterations   
tic()  # 219 secs per iteration-scenario
res <- run_SSMSE(scen_name_vec = c("h-ctl", "h-1"), # name of the scenario
                 out_dir_scen_vec = run_res_path, # directory in which to run the scenario
                 iter_vec = c(niters, niters), # run with niters iterations each
                 OM_name_vec = NULL, # specify directories instead
                 OM_in_dir_vec = c(cod_mod_path, normalizePath(cod_1_path)), # OM files
                 EM_name_vec = c("cod", "cod"), # cod is included in package data
                 MS_vec = c("EM", "EM"), # The management strategy is specified in the EM
                 nyrs_vec = c(6, 6), # Years to project OM forward, longer projections are typical in MSE analyses
                 nyrs_assess_vec = c(3, 3), # Years between assessments
                 future_om_list = future_om_list_recdevs_sel,
                 run_parallel = FALSE, # Run iterations in parallel
                 sample_struct_list = sample_struct_list_all, # How to sample data for running the EM.
                 sample_struct_hist_list = NULL, # because this is null, will just use sampling
                  # as in the current OM data file for the historical period.
                 seed = 12345) # Set a fixed integer seed that allows replication
toc()

list.dirs(run_res_path, recursive = FALSE)  #folders with scenarios results
list.dirs(file.path(run_res_path, "h-ctl"), recursive = FALSE)
list.dirs(file.path(run_res_path, "h-ctl", "1"), recursive = FALSE) #There should be 1 folder for the OM, which is run multiple times in this same folder during the MSE analysis. 
                      # There are multiple folders for the EMs, as a new folder is created each time an assessment is done.


# Performance metrics and MSE summaries------------------------------------------
# Note: Quantitative performance metrics should be specified before conducting an MSE. 
#   Typically, a suite of performance metrics will be examined
# We will only look at what the achieved relative biomass was for the last 3 years of projection in the MSE to 
#  determine how it compares to the intended management target of 40% of unfished Spawning Stock Biomass. 

# The function SSMSE_summary_all can be used to summarize the model results in a list of 3 dataframes, 
#       one for scalar outputs (named scalar), one for timeseries outputs (ts), one for derived quantities (dq). 
# This function also creates summary csv files in the folder where the results are stored
summary <- SSMSE_summary_all(run_res_path)


check_convergence <- function(summary, min_yr = 101, max_yr = 120, n_EMs)
  {
  require(dplyr) # note: not the best way to do this
  if (any(!is.na(summary$scalar$params_on_bound))) {
    warning("Params on bounds")
  } else {
    message("No params on bounds")
  }
  summary$ts$model_type <- ifelse(grepl("_EM_", summary$ts$model_run), "EM", "OM")
  calc_SSB <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>%
    select(iteration, scenario, year, model_run, model_type, SpawnBio)
  OM_vals <- calc_SSB %>%
    filter(model_type == "OM") %>%
    rename(SpawnBio_OM = SpawnBio) %>%
    select(iteration, scenario, year, SpawnBio_OM)
  EM_vals <- calc_SSB %>%
    filter(model_type == "EM") %>%
    rename(SpawnBio_EM = SpawnBio) %>%
    select(iteration, scenario, year, model_run, SpawnBio_EM)
  bind_vals <- full_join(EM_vals, OM_vals, by = c("iteration", "scenario", "year")) %>%
    mutate(SSB_ratio = SpawnBio_EM / SpawnBio_OM)
  filter_SSB <- bind_vals %>%
    filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if (nrow(filter_SSB) > 0) {
    warning("Some large/small SSBs relative to OM")
  } else {
    message("All SSBs in EM are no greater than double and no less than half SSB vals in the OM")
  }
  return_val <- bind_vals
}
values <- check_convergence(summary = summary, min_yr = 101, max_yr = 106, n_EMs = niters)


# plot SSB by year and model run
ggplot2::ggplot(
  data = subset(summary$ts, model_run %in% c("cod_OM", "cod_SR_BH_steep_1_OM", "cod_EM_103")),
  ggplot2::aes(x = year, y = SpawnBio)) +
  ggplot2::geom_vline(xintercept = 100, color = "gray") +
  ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_run)) +
  ggplot2::scale_color_manual(values = c("#D65F00", "black", "blue")) +
  ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
  ggplot2::guides(linetype = FALSE) +
  ggplot2::facet_wrap(. ~ scenario,ncol=1) +
  ggplot2::theme_classic()


# calculate and plot the performance metric (average spawning stock biomass (SSB) from years 104 to 106)
# get_SSB_avg calculates the SSB in each year for each
# iteration of the operating model, then takes the average over the years from
# min_yr, to max_year. It uses the summary object as input to do these
# calculations.
get_SSB_avg <- function(summary, min_yr, max_yr) {
  OM_vals <- unique(summary$ts$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  SSB_yr <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>%
    filter(model_run %in% OM_vals) %>%
    select(iteration, scenario, year, SpawnBio) %>%
    group_by(iteration, scenario) %>%
    summarize(avg_SSB = mean(SpawnBio), .groups = "keep") %>%
    ungroup()
  SSB_yr
}
avg_SSB <- get_SSB_avg(summary, min_yr = 104, max_yr = 106)

# function to summarize data in plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}
# Now, plot the average relative spawning stock biomass for years 104 - 106
ggplot(data = avg_SSB, aes(x = scenario, y = avg_SSB)) +
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(width = 0.9), color = "blue"
  ) +
  labs(
    title = "Long-term average  SSB\n(years 104-106)",
    x = "Scenario", y = "SSB"
  ) +
  theme_classic()


# Mis-specifying the value of steepness (h-1) will results in higher realized relative spawning stock 
    #  biomass than correctly specifying it. 
# This gives us some idea of the consequences of misspecifying steepness in the stock assessment.



# Delete files------------------------------------------
unlink(run_SSMSE_dir, recursive = TRUE)


# Custom management strategy ------------------------------------------
#Note: The function should be created in a separate file. 
#     In this case, assume this function is available in a file custom_funs.R.

# Example of setting future catches as half the sampled catches in a given year
constant_catch_MS <- function(OM_dat, nyrs_assess, catch_yr = 100,
                              frac_catch = 0.5, ...)
  
  
# Run SSMSE
  run_result_custom <- run_SSMSE(
    scen_name_vec = "constant-catch", # name of the scenario
    out_dir_scen_vec = run_res_path, # directory in which to run the scenario
    iter_vec = 1,
    OM_name_vec = "cod", # specify directories instead
    OM_in_dir_vec = NULL,
    MS_vec = "constant_catch_MS", # use the custom function
    custom_MS_source = "custom_funs.R", # File where the custom function is available.
    nyrs_vec = 6, # Years to project OM forward
    nyrs_assess_vec = 3, # Years between assessments
    future_om_list = future_om_list_recdevs_sel,
    sample_struct_list = list(sample_struct_list[[1]]), # How to sample data for running the MS.
    seed = 12345
  ) # Set a fixed integer seed that allows replication
  
{ # need to include ... to allow function to work
  # set catch the same as the previous year (sampled catch).
  # catch is in the same units as the operating model, in this case it is in
  # biomass.
  catch <- data.frame(
    year = (OM_dat$endyr + 1):(OM_dat$endyr + nyrs_assess), # the years to project the model forward
    seas = 1, # hard coded from looking at model
    fleet = 1, # hard coded from looking at model
    catch = OM_dat$catch[OM_dat$catch$year == catch_yr, "catch"] * frac_catch,
    catch_se = 0.05
  ) # hard coded from looking at model
  catch_bio <- catch # catch in biomass. In this case, catch is in biomass for both. Could also be left as NULL
  catch_F <- NULL # catch in terms of F, can be left as NULL.
  discards <- NULL # discards can be left as NULL if there are no discards
  catch_list <- list(
    catch = catch,
    catch_bio = catch_bio,
    catch_F = catch_F,
    discards = discards
  )
}
