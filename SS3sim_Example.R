library(ss3sim)

#Anderson et la 2014:
#Steps
#1. implement alternative dynamics
#2. run Operating Model
#3. sample true population time series to generate and observed data set
#4. rum Estimation Model
#5. synthesizes the results

# Folders with plain text cases, OM and EM
d <- system.file("extdata", package = "ss3sim")
case_folder <- file.path(d, "eg-cases")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")


#Create case files to investigate effect of observation error in indices
run_ss3sim(iterations = 1, scenarios =
             c("D0-E0-F0-M0-cod",
               "D1-E0-F0-M0-cod",
               "D0-E1-F0-M0-cod",
               "D1-E1-F0-M0-cod"),
           case_files = list(F = "F", D = c("index", "lcomp", "agecomp"),
                             E = "E", M = "M"),
           case_folder = case_folder, om_dir = om,
           em_dir = em)