# Header ---------------
# This script uses SSMSE and RatPack to perform Management Strategy Procedures to inform the development of the 
#   Shark resource Harvest Strategy based on the 4 indicator species

# The software default harvest control rule is considered
# Alternative states of nature are considered thru multiple OM (Operating Model):
#       alternative hypotheses on Steepness, Natural mortality, selectivity and illegal fishing
# Alternative reference points are considered thru multiple EM (Estimation Model):
#       alternative values of limit, threshold and target reference points
# The Alternative OM and EM are defined in 'hndl.mse.comp' below based on the 'Scenarios' spreadsheet
# Most input files are set up automatically by this script but there is some manual tweaking to be done:
#     RatPack: In the first run, create files and manually update the .OPD, .HSE and .proj files in 
#               the 'inputs' folder using values from input_HSE.txt and input_OPD.txt. 
#              Do this for S1 and then copy from S1 to other Scenarios and then manually update relevant parts only.For Scenarios 
#               with changes to OM, use the re-fit values (control.ss_new) from SSMSE to update the OPD
#     SSMSE:   In the first run, create files and re fit the OM for scenarios with changes to OM. For scenarios with
#               no changes to the OM, just copy across.
#              VIP: For OM/EM, if there are time changing parameters (i.e., blocks in Q or Selectivity), copy, 
#             'timevary Q parameters' or 'timevary selex parameters' from 'control.ss_new to 'control.ss' 

# SSMSE reference material:
#     https://nmfs-fish-tools.github.io/SSMSE/manual
#     https://github.com/nmfs-fish-tools/SSMSE/blob/main/README.md
#     https://github.com/k-doering-NOAA/ssmse-wfc
#     https://noaa-fisheries-integrated-toolbox.github.io/SSMSE

#HCL
  #DPIRD's:
    # 50-100% effort reduction if performance indicator (relative biomass) between limit and threshold
    # 10-50% effort reduction if performance indicator (relative biomass) between threshold and target
  #MSC guidelines:
    # Bring back to threshold within 1 generation if between limit and threshold and 
    # within another generation if between threshold and target

rm(list=ls(all=TRUE))
#library(remotes)
#remotes::install_github("nmfs-fish-tools/SSMSE", ref='merge_catch_bias')  #install branch
library(SSMSE)
library(r4ss)
library(tictoc)
library(tidyverse)
library(readxl)
library(doParallel)
library(ggrepel)
library(gridExtra)
library(fields)
library(ggpubr)
library(cowplot)
library(geomtextpath)
library(patchwork)
library(flextable)
library(grid)
library(ggh4x)
library(ggthemes)
library(ggpubr)

#library(Hmisc)
set.lib.path=TRUE
if(grepl('4.0.3',version[['version.string']])) set.lib.path=FALSE
if(set.lib.path).libPaths("C:/Users/myb/AppData/Local/R/win-library/4.4")

# Set up paths and source functions  ---------------
fn.user=function(x1,x2)paste(x1,Sys.getenv("USERNAME"),x2,sep='/')
if(!exists('handl_OneDrive')) source(fn.user(x1='C:/Users',
                                             x2='OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R'))
source.hnld=handl_OneDrive("Analyses/MSE/Git_MSE/")
fn.source=function(script)source(paste(source.hnld,script,sep=""))
fn.source("2024_Shark_auxiliary functions.R")

# source the ratpack data script (it's on Bitbucket)
source(handl_OneDrive('Workshops/2024_Andre Punt_MSE/Software/ratpackmser/R/read_and_plot_ratpack_output.R'))
source(handl_OneDrive('Workshops/2024_Andre Punt_MSE/Software/ratpackmser/R/Buffer_analysis_functions.R'))

#SS files used in stock assessments
in.path=handl_OneDrive("Analyses/Population dynamics")  #path to SS3 stock assessments files 

#paths for each method
out.path.SSMSE=handl_OneDrive("Analyses/MSE/Shark harvest strategy/SSMSE")   
out.path.RatPack=handl_OneDrive("Analyses/MSE/Shark harvest strategy/RatPack")   

#overall outputs
outs.path=handl_OneDrive("Analyses/MSE/Shark harvest strategy/z_Outputs") 

# Define MSE components ---------------
#Components: 
# 1. Management objectives
# 2. Operating Models
# 3. Estimation models (management scenarios)
# 4. Harvest control rules
# 5. Performance measures
# 6. Future data collection

  #Scenarios
hndl.mse.comp=handl_OneDrive("Analyses/MSE/Shark harvest strategy/Scenarios.xlsx")
Management_objectives=read_excel(hndl.mse.comp,  sheet = "Management objectives",skip = 0)
Operating_models=read_excel(hndl.mse.comp,       sheet = "Operating model",skip = 0)
Management_scenarios=read_excel(hndl.mse.comp,   sheet = "Estimation model",skip = 0)

Perform.ind=Management_objectives%>%filter(Used.in.MSE=='Yes')%>%distinct(Perf.ind.name,.keep_all = T)%>%pull(Perf.ind.name)
Perform.ind.levels=c("Recruits","SSB","Depletion","F/FMSY","B/BMSY","Total catch","Catch variability")
Perform.ind.levels2=c("Recruits","SSB","Depletion","Kobe","Total catch","Catch variability")  
Perf.ind_pos=c('Recruits','SSB','Depletion','Kobe','Total catch')
Perf.ind_neg=c('Catch variability')


  #Historic catch ranges
Target.commercial.catch=Management_objectives%>%
                            filter(Ojective.status=='Current')%>%
                            mutate(Type=ifelse(grepl('minimum',Objective),'Min.tones',
                                        ifelse(grepl('maximum',Objective),'Max.tones',
                                               NA)))%>%
                            filter(!is.na(Type))%>%
                            dplyr::select(Species,Type,Value)%>%
                            spread(Type,Value)%>%
                            relocate(Species,Min.tones,Max.tones)

Catch.species.dataset=read.csv(handl_OneDrive("Analyses/Population dynamics/PSA/Annual_ktch_by_species.and.data.set.csv"))

# Define global parameters ---------------
First.Run.SSMSE=FALSE                  # set to TRUE to generate OMs, Folders, etc (don't run again as it will over write them)
re.fit.SSMSE.OM=FALSE                  # set to TRU to re fit OM following changes to control file 
run.SSMSE=FALSE                        # set to TRUE to run SSMSE
First.Run.RatPack=FALSE                # set to TRUE to generate OPD,HSE and proj files (don't run again as it will over write them)
run.RatPack=FALSE                      # set to TRUE to run RatPack
niters <- 2                            # number of simulations per scenario (50, 100 will take way too long)
assessment.year=2022                   # year latest stock assessment 
last.year.obs.catch=2021               # last year with observed catch
Proj.years=10                          # number of projected years (25)
Proj.years.between.ass=5               # years between assessments in the projected period
Proj.years.obs=seq(1,Proj.years,by=1)  # sampled years in the projected period
Proj.assessment.years=seq(assessment.year,(assessment.year+Proj.years),Proj.years.between.ass)[-1]
proj.CV=0.2                            # CV in the projected period
Proj.Effective.pop.size.future=100     #projection length comp sample size
Keep.species=sort(unique(Operating_models$Species))
N.sp=length(Keep.species)
species_logistic.selectivity.NSF=sort(unique(Operating_models%>%filter(!is.na(NSF.selectivity))%>%pull(Species)))  
species_IUU_indonesia=sort(unique(Operating_models%>%filter(!is.na(Indo.IUU))%>%pull(Species)))    

subset.scenarios=TRUE  #test scenario grid?? way time consuming

RiskColors=c('Negligible'="cornflowerblue", 'Low'="chartreuse3", 'Medium'="yellow1",
             'High'="orange", 'Severe'="brown1")

theme_set(theme_light() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(colour = 'black'),
                  text = element_text(family = "Calibri", size = 12)))

# Create relevant directories and handles for each MSE framework  ---------------
  #SSMSE
for(s in 1:N.sp) fn.create.folder(x=paste(out.path.SSMSE,Keep.species[s],sep='/'))

  #RatPack
for(s in 1:N.sp) fn.create.folder(x=paste(out.path.RatPack,Keep.species[s],sep='/'))
hndl.RatPack=handl_OneDrive('Analyses/MSE/RatPack') #path to original files






# Create MSE scenarios ---------------
SCENARIOS=fn.create.list(Keep.species)
for(i in 1:N.sp)
{
  #Alternative OMs
  #note: these are the SS models used in the stock assessment sensitivity tests       
  OM_scenarios=Operating_models%>%
    filter(Species==Keep.species[i])%>%
    mutate(Assessment.path=case_when(!Scenario.source=='New'~ sub('.*_', '', Scenario.source),
                                     Scenario.source=='New'~'S1'))%>%
    dplyr::select(Assessment.path,Difference,Difference.value)%>%
    data.frame%>%mutate(row.number=row_number())
  
  #Define MSE scenarios (combination of OMs and Harvest strategies)
  dd=merge(Management_scenarios,OM_scenarios,by=NULL)%>%
    arrange(row.number,Ref.point)%>%
    mutate(Scenario=paste0('S',row_number()))%>%
    relocate(Ref.point,Scenario)%>%dplyr::select(-row.number)
  if(subset.scenarios)
  {
    dd=dd%>%
      mutate(Keep=ifelse(Ref.point=='0.5_1_1.2' | is.na(Difference),'YES','NO'))%>%
      filter(Keep=='YES')%>%dplyr::select(-Keep)
  }
  SCENARIOS[[i]]=dd%>%mutate(Scenario=paste0('S',row_number()))%>%mutate(Species=Keep.species[i])
}

re.fit.SSMSE.scenarios=SCENARIOS
for(i in 1:N.sp)
{
  different.OM=c(re.fit.SSMSE.scenarios[[i]]%>%
                       filter(is.na(Difference))%>%
                       distinct(Difference, .keep_all = T)%>%
                       pull(Scenario),
                 re.fit.SSMSE.scenarios[[i]]%>%filter(!is.na(Difference))%>%pull(Scenario))
  re.fit.SSMSE.scenarios[[i]]=re.fit.SSMSE.scenarios[[i]]%>%filter(Scenario%in%different.OM)
}

Not.tested.in.Rat.Pack='rep.cycle' #cannot test rep cycle in RatPack

# Run SSMSE loop over each species-scenario combination ---------------
#note: remove iterations within Outputs/Scenario folder
Current.fleets=fn.create.list(Keep.species)
Current.fleets$`Gummy shark`=Current.fleets$`Whiskery shark`=c("Other","Southern.shark_2")
Current.fleets$`Dusky shark`=Current.fleets$`Sandbar shark`=c("Other","Southern.shark_2","Survey")

#1. Create SSMSE folders and OM & EM files
if(First.Run.SSMSE)
{
  #specify time changing parameters
  BLK.pat=fn.create.list(Keep.species)  
  BLK.pat$'Sandbar shark'=c("Size_DblN_peak_Southern.shark_1(3)_BLK1mult_1994","Size_DblN_peak_Southern.shark_1(3)_BLK1mult_2000",
                            "Size_DblN_ascend_se_Southern.shark_1(3)_BLK1mult_1994","Size_DblN_ascend_se_Southern.shark_1(3)_BLK1mult_2000",
                            "Size_DblN_descend_se_Southern.shark_1(3)_BLK1mult_1994","Size_DblN_descend_se_Southern.shark_1(3)_BLK1mult_2000")
  BLK.pat$'Whiskery shark'=c("LnQ_base_Southern.shark_1(3)_BLK1repl_1975","LnQ_base_Southern.shark_1(3)_BLK1repl_1984")
  tic()
  for(i in 1:N.sp)
  {
    #set up paths
    sp_path_assessment=paste(in.path,paste0('1.',Keep.species[i]),assessment.year,'SS3 integrated',sep='/')
    sp_path_OM=paste(out.path.SSMSE,Keep.species[i],'OM',sep='/')
    sp_path_EM=paste(out.path.SSMSE,Keep.species[i],'EM',sep='/')
    sp_path_out=paste(out.path.SSMSE,Keep.species[i],'Outputs',sep='/')
    fn.create.folder(x=sp_path_OM)
    fn.create.folder(x=sp_path_EM)
    fn.create.folder(x=sp_path_out)
    
    Scenarios=SCENARIOS[[i]]
    
    for(s in 1:nrow(Scenarios))
    {
      print(paste('SSMSE create files for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      Indoktch=NULL
      if(!is.na(Scenarios$Difference[s]) & Scenarios$Difference[s]=="IUU")
      {
        Indoktch=Catch.species.dataset%>%filter(Name==Keep.species[i] & Data.set=="Indonesia")
      }
      fn.create.SSMSE.files(sp_path_assessment, sp_path_OM, sp_path_EM, Scen=Scenarios[s,],block.pattern=BLK.pat[[i]])
    } #end s
    rm(sp_path_assessment,sp_path_OM,sp_path_EM,Scenarios)
  } #end i
  toc()
}

#2. Re fit model to implement Scenario changes to OM and have all MSE inputs right (par.ss, etc)
#note: before running this, must make manual changes to OM (block patterns, etc)
if(re.fit.SSMSE.OM)
{
  tic() #953 secs
  for(i in 1:N.sp)
  {
    sp_path_OM=paste(out.path.SSMSE,Keep.species[i],'OM',sep='/')
    Scenarios=re.fit.SSMSE.scenarios[[i]]
    for(s in 1:nrow(Scenarios))
    {
      print(paste('SSMSE re fit OM for',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      fn.run.SS(where.inputs=paste(sp_path_OM,Scenarios$Scenario[s],sep='/'),
                where.exe=handl_OneDrive('SS3/ss_win.exe'),
                args="-nohess")
    }

  }
  toc()
}

#3. Execute SSMSE
if(run.SSMSE)
{
  tic()   #11 sec per species-scenario-simulation-proj.year 
  SSMSE_outputs=fn.create.list(Keep.species)
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]  
    dumy.out=fn.create.list(Scenarios$Scenario)
    sp_path_OM=paste(out.path.SSMSE,Keep.species[i],'OM',sep='/')
    sp_path_EM=paste(out.path.SSMSE,Keep.species[i],'EM',sep='/')
    sp_path_out=paste(out.path.SSMSE,Keep.species[i],'Outputs',sep='/')
    
    for(s in 1:nrow(Scenarios))
    {
      print(paste('SSMSE run for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      
      #flush scenario folder to remove previous runs
      if(dir.exists(paste0(sp_path_out,'/',Scenarios$Scenario[s],'/')))
      {
        fn.remove.subfolder(Loc=paste0(sp_path_out,'/',Scenarios$Scenario[s],'/'))
      }
      
      #run SSMSE
      dumy.out[[s]]=fn.run.SSSMSE(Scen=Scenarios[s,], sp_path_OM, sp_path_EM, sp_path_out,
                                  Nsims=niters, Neff.future=Proj.Effective.pop.size.future,
                                  proj.yrs=Proj.years, proj.yrs.with.obs=Proj.years.obs, 
                                  yrs.between.assess=Proj.years.between.ass,
                                  cur.fleets=Current.fleets[[i]], future.cv=proj.CV,
                                  specify.future.OM=TRUE, apply.future.meanbodywt=TRUE) 
    } #end s
    SSMSE_outputs[[i]]=dumy.out
    rm(sp_path_OM,sp_path_EM,Scenarios,sp_path_out)
  } #end i
  toc()
}


# Run RatPack loop over each species-scenario combination --------------

#1. Create RatPack folders and OM & EM files  
if(First.Run.RatPack)
{
  #original files
  rat.pack.folders=c('inputs','outputs','PGMSY','Results','Stock_Synthesis','Stock_Synthesis3.30base')
  rat.pack.files=c('run.bat','Whiskery_test.proj','Whiskery.OPD','Whiskery.HSE')
  
  tic()
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    aaid=which(Scenarios$Difference%in%Not.tested.in.Rat.Pack)
    if(length(aaid)>0) Scenarios=Scenarios[-aaid,]  
    for(s in 1:nrow(Scenarios))
    {
      print(paste('RatPack create files for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      sp_path_scen=paste(out.path.RatPack,Keep.species[i],Scenarios$Scenario[s],sep='/')
      
      Indoktch=NULL
      if(!is.na(Scenarios$Difference[s]) & Scenarios$Difference[s]=="IUU")
      {
        Indoktch=Catch.species.dataset%>%filter(Name==Keep.species[i] & Data.set=="Indonesia")
      }
      
      #1.1 create folders
      fn.create.folder(x=sp_path_scen)
      for(r in 1:length(rat.pack.folders)) fn.create.folder(x=paste0(sp_path_scen,'/',rat.pack.folders[r]))
      
      #1.2 copy files
        #exe
      fn.copy.file(x='ratpackMSE.exe',from=hndl.RatPack,to=sp_path_scen)
        #SS3 files
      list.3.30base.files=list.files(paste0(hndl.RatPack,'/Stock_Synthesis3.30base'))
      list.3.30base.files=list.3.30base.files[-grep(".pdf",list.3.30base.files)]
      if(!file.exists(paste0(sp_path_scen,'/Stock_Synthesis3.30base/runsspar.r')))
      {
        invisible(lapply(list.3.30base.files, function(x) file.copy(from= paste(paste0(hndl.RatPack,'/Stock_Synthesis3.30base'), x, sep = "/"),
                                                                    to = paste0(sp_path_scen,'/Stock_Synthesis3.30base'), recursive = TRUE)))
      }
        #.proj, .OPD, .HSE files
      for(x in 1:length(rat.pack.files))
      {
        fn.copy.RatPack.files(inpath=hndl.RatPack,
                              sp=Keep.species[i],
                              outpath=sp_path_scen,
                              file.name=rat.pack.files[x])
      }
      
      #2.1 Generate relevant info to manually populate HSE
      dumy=fun.populate.HSE(i,SS.path=paste(in.path,paste0('1.',Keep.species[i]),assessment.year,'SS3 integrated',
                                            Scenarios$Assessment.path[s],sep='/'),
                            scen=Scenarios[s,], proj.yrs=Proj.years, yrs.between.assess=Proj.years.between.ass)
      sink(paste0(sp_path_scen,'/inputs/input_HSE.txt'))
      print(dumy,row.names=F)
      sink()
      rm(dumy)
      
      #2.2 Generate relevant info to manually populate OPD   
      dumy=fun.populate.OPD(i,SS.path=paste(in.path,paste0('1.',Keep.species[i]),assessment.year,'SS3 integrated',
                                            Scenarios$Assessment.path[s],sep='/'),
                            Scen=Scenarios[s,],Neff.future=Proj.Effective.pop.size.future,Nregions=1)
      sink(paste0(sp_path_scen,'/inputs/input_OPD.txt'))
      print(dumy,row.names=F)
      sink()
      rm(dumy)
    } #end s
  } #end i
  toc()
}

#2. Execute RatPack 
if(run.RatPack)
{
  SS.file.years=c(assessment.year,Proj.assessment.years)
  last.yr.EM=SS.file.years[length(SS.file.years)]
  
  tic()  #5 secs per species-scenario-simulation-proj.year
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    aaid=which(Scenarios$Difference%in%Not.tested.in.Rat.Pack)
    if(length(aaid)>0) Scenarios=Scenarios[-aaid,]  
    for(s in 1:nrow(Scenarios))
    {
      print(paste('RatPack run for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      sp_path_scen=paste(out.path.RatPack,Keep.species[i],Scenarios$Scenario[s],sep='/')
      sp=str_remove(Keep.species[i],' shark')
      
        #Check input files
      check_proj_file(paste0(sp_path_scen, "/inputs"), paste0(sp,"_test.proj"))
      check_OPD_file(paste0(sp_path_scen, "/inputs"), paste0(sp,".OPD"))
      check_HSE_file(paste0(sp_path_scen, "/inputs"), paste0(sp,".HSE"))
      
        #run RatPack
      fn.run.RatPack.exe(where.exe=sp_path_scen,exe.name='run.bat')
      
      
      #run SS EM for last assessment year if Report file not created by Ratpack
      spi=paste0(sp,'_sim_')
      for(k in 1:niters)
      {
        for(x in length(SS.file.years))
        {
          nn=paste0(spi,k,'_year_',SS.file.years[x])
          if(!file.exists(paste0(sp_path_scen,'/Stock_Synthesis/',nn,'/Report.sso')))
          {
            nn_minus1=paste0(spi,k,'_year_',SS.file.years[1]) #get par.ss from 1st assessment
            fn.re.run.SS(WD=paste0(sp_path_scen,'/Stock_Synthesis/',nn),
                         prev.ass=paste0(sp_path_scen,'/Stock_Synthesis/',nn_minus1))
            
          }
        }
      }
      
    } #end s
  } #end i
  toc()
}
   
# Report tested MSE scenarios  ----------------------------------------------------------
write.csv(do.call(rbind,SCENARIOS)%>%
            dplyr::select(-c(Ref.point,EM,Assessment.path))%>%
            relocate(Species)%>%
            rename(OM.Difference=Difference,
                   OM.Difference.value=Difference.value,
                   EM.Limit=Limit,
                   EM.Threshold=Threshold,
                   EM.Target=Target,
                   EM.SPR_Btgt.scalar=SPR_Btgt.scalar)%>%
            mutate(MSE.framework=ifelse(OM.Difference%in%Not.tested.in.Rat.Pack,'SSMSE',c('SSMSE & RatPack'))),
          paste0(outs.path,'/Table 1.Scenarios.csv'),row.names = F)

Tab.HCR.scens=fn.create.list(Keep.species)
for(i in 1:N.sp)
{
  Scenarios=SCENARIOS[[i]]
  aaid=which(Scenarios$Difference%in%Not.tested.in.Rat.Pack)
  if(length(aaid)>0) Scenarios=Scenarios[-aaid,]  
  dd=fn.create.list(Scenarios$Scenario)
  for(s in 1:nrow(Scenarios))
  {
    sp_path_scen=paste(out.path.RatPack,Keep.species[i],Scenarios$Scenario[s],sep='/')
    dat <- readLines(paste0(sp_path_scen,'/inputs/input_HSE.txt'), warn = FALSE)
    dd[[s]]=data.frame(Scenario=Scenarios$Scenario[s],
                       Species=Keep.species[i],
                       Limit=substr(dat[[grep('HCR.limit',dat)+1]],5,9),
                       Threshold=substr(dat[[grep('HCR.break',dat)+1]],5,9),
                       Target=substr(dat[[grep('HCR.target',dat)+1]],5,9))
  }
  Tab.HCR.scens[[i]]=do.call(rbind,dd)
}
write.csv(do.call(rbind,Tab.HCR.scens),paste0(outs.path,'/Table 1.Scenarios_HCR_values.csv'),row.names = F)


# Report SSMSE outputs  ----------------------------------------------------------
#note: performance indicators: only latest EM assessment is displayed
if(run.SSMSE)
{
  OVERWRITE=FALSE #set to TRUE if SSMSE is re run, otherwise set to FALSE to speed up reporting
  out.exten='SSMSE'
  output_dis.pi.list=fn.create.list(Keep.species)
  Check.SSMSE.convergence=Get.SSMSE.perf.indic=Get.SSMSE.timeseries.perf.indic=output_boxplot.pi.list=
    output_quilt.list=output_lollipop.list=output_dis.pi.list
  
  #1. Extract quantities
  tic()   #takes 0.5 secs per species-scenario-simulation-proj.year if OVERWRITE==TRUE 
  for(i in 1:N.sp)
  {
    print(paste('SSMSE extract quantities for ',Keep.species[i],'-----------'))
    Scenarios=SCENARIOS[[i]]
    
    summary <- SSMSE_summary_all(paste(out.path.SSMSE,Keep.species[i],'Outputs',sep='/'),overwrite = OVERWRITE)
    
      #1.1 Performance indicators
    BMSY=median(summary$scalar$SSB_MSY,na.rm=T)
    FMSY=median(summary$scalar$F_MSY,na.rm=T)
    temp.d=summary$dq%>%
      rename(Recruits=Value.Recr,
             SSB=Value.SSB,
             Depletion=Value.Bratio,
             Catch=Value.ForeCatch)%>%
      mutate(model_run=sub(".*?_", "", model_run),
             B.over.BMSY=SSB/BMSY,
             F.over.FMSY=Value.F/FMSY,)%>%
      dplyr::select(c(model_run,iteration,scenario,year,F.over.FMSY,B.over.BMSY,any_of(Perform.ind)))%>%
      filter(!model_run=='error_check')
    
    out.dumi=fn.create.list(Scenarios$Scenario)
    for(s in 1:nrow(Scenarios))
    {
      d=temp.d%>%  
        filter(scenario==Scenarios$Scenario[s] & !model_run%in%c('EM_init','OM'))%>%
        mutate(dumi=as.numeric(sub(".*?_", "", model_run)))%>%
        filter(dumi==max(dumi))
     
      f.traj=d%>%group_by(year)%>%summarise(f.traj=median(F.over.FMSY))
      b.traj=d%>%group_by(year)%>%summarise(b.traj=median(B.over.BMSY))
      
      out.dumi[[s]]=list(f.traj=f.traj$f.traj,   
                         b.traj=b.traj$b.traj,
                         Years=f.traj$year,
                         Probs=data.frame(x=d%>%filter(year==max(year))%>%pull(B.over.BMSY),
                                          y=d%>%filter(year==max(year))%>%pull(F.over.FMSY)))
      rm(d)
    }
    Get.SSMSE.perf.indic[[i]]=list(Kobe=out.dumi)
    
    temp.d=temp.d%>%  
            filter(!model_run%in%c('EM_init','OM'))%>%
            mutate(dumi=as.numeric(sub(".*?_", "", model_run)))%>%
            filter(dumi==max(dumi,na.rm=T))
    
    Get.SSMSE.timeseries.perf.indic[[i]]=temp.d
    
      #extract values
    a=temp.d%>%
      filter(year==max(year))%>%
      dplyr::select(c(scenario,any_of(Perform.ind),F.over.FMSY,B.over.BMSY))%>%
      rename(Scenario=scenario)%>%
      gather(Perf.ind,Value,-Scenario)%>%
      filter(!Perf.ind=='Catch')
    
      #calculates total catch and catch variability 
    Katch.tot.perf.ind=Katch.var.perf.ind=fn.create.list(Scenarios$Scenario)
    for(s in 1:nrow(Scenarios))
    {
      katch=temp.d%>%
        filter(scenario==Scenarios$Scenario[s])%>%
        dplyr::select(Catch,iteration,year)%>%
        filter(!is.na(Catch))
      iter=sort(unique(katch$iteration))
      Tot.ktch=fn.create.list(iter)
      AAV.ktch=Tot.ktch
      for(x in 1:length(iter))
      {
        dd=fn.ktch.perf.ind(ktch=katch%>%filter(iteration==iter[x])%>%pull(Catch))
        Tot.ktch[[x]]=data.frame(Value=dd$Total)%>%
                            mutate(Perf.ind='Total catch', Scenario=Scenarios$Scenario[s])%>%
                            relocate(Scenario,Perf.ind,Value)
        AAV.ktch[[x]]=data.frame(Value=dd$AAV)%>%
                      mutate(Perf.ind='Catch variability', Scenario=Scenarios$Scenario[s])%>%
                      relocate(Scenario,Perf.ind,Value)
      }
      Katch.var.perf.ind[[s]]=do.call(rbind,AAV.ktch)
      Katch.tot.perf.ind[[s]]=do.call(rbind,Tot.ktch)
    }
    Katch.var.perf.ind=do.call(rbind,Katch.var.perf.ind)
    Katch.tot.perf.ind=do.call(rbind,Katch.tot.perf.ind)

    Get.SSMSE.perf.indic[[i]]$perf.ind.values=rbind(a,Katch.var.perf.ind,Katch.tot.perf.ind)%>%
                                                      mutate(Perf.ind=ifelse(Perf.ind=='F.over.FMSY','F/FMSY',
                                                                      ifelse(Perf.ind=='B.over.BMSY','B/BMSY',
                                                                             Perf.ind)),
                                                             Perf.ind=factor(Perf.ind,levels=Perform.ind.levels))
    
    
    #1.2 Check convergence
    Check.SSMSE.convergence[[i]] <- check_convergence_SSMSE(summary = summary,
                                                            min_yr = last.year.obs.catch+1,
                                                            max_yr = last.year.obs.catch+Proj.years)
    
    rm(temp.d,a,Katch.var.perf.ind,Katch.tot.perf.ind)
  }
  toc()
  
  #2. Plot figures
  delt=c(-0.175,-0.2,-0.2,-0.2)  
  for(i in 1:N.sp)
  {
    print(paste('SSMSE create figures for ',Keep.species[i],'-----------'))
    Scenarios=SCENARIOS[[i]]
      
    #2.1 Kobe plot
    scen.list_kobe=fn.create.list(Scenarios$Scenario)
    scen.list_kobe.prob.green=scen.list_kobe
    for(s in 1:nrow(Scenarios))
    {
      xx=kobePlot(f.traj=Get.SSMSE.perf.indic[[i]]$Kobe[[s]]$f.traj,
                                   b.traj=Get.SSMSE.perf.indic[[i]]$Kobe[[s]]$b.traj,
                                   Years=Get.SSMSE.perf.indic[[i]]$Kobe[[s]]$Years,
                                   Titl=Scenarios$Scenario[s],
                                   Probs=Get.SSMSE.perf.indic[[i]]$Kobe[[s]]$Probs,
                                   pt.size=2.5,
                                   txt.col='transparent',
                                   line.col='black',
                                   YrSize=4,
                                   YLAB='', XLAB='',
                                   Leg.txt.size=10)
      scen.list_kobe[[s]]=xx$kobe
      scen.list_kobe.prob.green[[s]]=data.frame(Scenario=Scenarios$Scenario[s],Prob=xx$prob.green)
      
    } #end s
    wid=6.5
    NroW=5
    if(nrow(Scenarios)<9)
    {
      wid=7.5
      NroW=4
    }
    ggarrange(plotlist = scen.list_kobe,ncol=2,nrow=NroW, common.legend=FALSE)%>%
          annotate_figure(left = textGrob(expression(F/~F[MSY]), rot = 90, vjust = 1, gp = gpar(cex = 1.7)),
                          bottom = textGrob(expression(B/~B[MSY]), gp = gpar(cex = 1.7)))
    ggsave(paste0(outs.path,'/2_Kobe_',out.exten,'_',Keep.species[i],'.tiff'),width = wid,height = 12,compression = "lzw")
    

    #2.2 Distribution of performance indicators   
    output_dis.pi.list[[i]]=fn.perf.ind.dist(df=Get.SSMSE.perf.indic[[i]]$perf.ind.values,
                                            YLAB='',
                                            Title=Keep.species[i])
    output_boxplot.pi.list[[i]]=fn.perf.ind.boxplot(df=Get.SSMSE.perf.indic[[i]]$perf.ind.values,
                                                   YLAB='',
                                                   Title=Keep.species[i])        
    
    #2.3 Performance indicator polar plots
    Kobe.per.ind=do.call(rbind,scen.list_kobe.prob.green)%>%
      mutate(Perf.ind='Kobe')%>%mutate(value=Prob/100)%>%dplyr::select(-Prob)%>%
      relocate(Scenario,Perf.ind,value)
    Per.ind.scaled=Get.SSMSE.perf.indic[[i]]$perf.ind.values%>%
                        group_by(Scenario,Perf.ind)%>%
                        summarise(value=median(Value))
    
    Per.ind.scaled=rbind(Per.ind.scaled%>%
                           filter(!Perf.ind%in%c('F/FMSY','B/BMSY')),
                         Kobe.per.ind)%>%
                    mutate(Perf.ind=factor(Perf.ind,levels=Perform.ind.levels2))%>%  
                         group_by(Perf.ind)%>%
                    mutate(value.rescaled=value/max(value))%>%
                rename(Indicator=Perf.ind,
                       value.orig=value,
                       value=value.rescaled)%>%
              arrange(Indicator)
    fn.polar.plot(data= Per.ind.scaled, Title= "")
    ggsave(paste0(outs.path,'/3_Polar.plot_',out.exten,'_',Keep.species[i],'.tiff'),width = 6,height = 8,compression = "lzw")
    
    #2.4 Performance indicator lollipop plot
    output_lollipop.list[[i]]=fn.lolipot.plot(data= Per.ind.scaled, Title= Keep.species[i])
    
    
    #2.5 Quilt plot
    output_quilt.list[[i]]=fn.quilt.plot(df=Per.ind.scaled%>%
                                               data.frame()%>%
                                           dplyr::select(-value)%>%
                                           mutate(value=ifelse(Indicator%in%c('Recruits','Total catch','SSB'),
                                                               round(value.orig),round(value.orig,1)))%>%
                                               dplyr::select(-value.orig)%>%
                                               spread(Indicator,value)%>%
                                               `rownames<-`(Scenarios$Scenario)%>%
                                               dplyr::select(-Scenario),
                                        clr.scale=colorRampPalette(c('white','cadetblue2','cornflowerblue')),
                                        col.breaks=50,
                                        Titl=Keep.species[i],
                                        Delta=delt[i]) 
    
    #2.5 Time series of performance indicators 
    fn.perf.ind.time.series(df=Get.SSMSE.timeseries.perf.indic[[i]]%>%
                                    dplyr::select(-c(dumi,model_run))%>%
                                    gather(Perf.ind,Value,-c(iteration,scenario,year))%>%
                              rename(Scenario=scenario),
                            YLAB='Indicator value',
                            Title=Keep.species[i])
    ggsave(paste0(outs.path,'/1_Perf_indicator_time.series_',out.exten,'_',Keep.species[i],'.tiff'),
           width = 10,height = 8,compression = "lzw")
    
    
  } #end i
  
    #combined distribution of performance indicators
  ggarrange(plotlist = output_dis.pi.list,nrow=1,common.legend = FALSE)%>%
    annotate_figure(left = textGrob('Density distribution', rot = 90, vjust = 1, gp = gpar(cex = 1.7)))
  ggsave(paste0(outs.path,'/1_Perf_indicator_distribution.plot_',out.exten,'.tiff'),width = 10.5,height = 8,compression = "lzw")
  
  ggarrange(plotlist = output_boxplot.pi.list,nrow=1)%>%
    annotate_figure(left = textGrob('Indicator value', rot = 90, vjust = 1, gp = gpar(cex = 1.7)))
  ggsave(paste0(outs.path,'/1_Perf_indicator_box.plot_',out.exten,'.tiff'),width = 10,height = 8,compression = "lzw")
  
    #combined lollipop plot
  figure=ggarrange(plotlist = output_lollipop.list,nrow=1)
  annotate_figure(figure,bottom = textGrob("z-score", vjust=-1,gp = gpar(cex = 1.3)))
  ggsave(paste0(outs.path,'/3_Lollipop.plot_',out.exten,'.tiff'),width = 10,height = 10,compression = "lzw")
  
    #combined quilt plot
  ggarrange(plotlist = output_quilt.list,ncol=1)
  ggsave(paste0(outs.path,'/4_Quilt.plot_',out.exten,'.tiff'),width = 7,height = 10,compression = "lzw")
}

# Report Ratpack outputs  ----------------------------------------------------------
if(run.RatPack)
{
  out.exten='RatPack'
  outputs_RatPack=fn.create.list(Keep.species)
  data_fields <- c('Sim','Year','Period','SSB0','SSBcurrent','SSBregion','Depletion',
                   'Recruits','AssessFail','estSSB0','estSSBcurrent','estDepletion',
                   'RawRBC','RBCPostPGMSY','RBC','TACdisc','TACpost','TAC',
                   'RawTotalCatch','TotCatch','TotDiscard','mainCPUE')
  
  #Perform.ind
  Perf.ind=c(SSB='SSBcurrent',Depletion='Depletion',Recruits='Recruits',
             Catch='TotCatch',Catch.RBC='RBC',Catch.TAC='TAC')
  
  #1. Extract quantities   #ACA, update with 'set up this ratpack results_then delete.R'
  tic()
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    aaid=which(Scenarios$Difference%in%Not.tested.in.Rat.Pack)
    if(length(aaid)>0) Scenarios=Scenarios[-aaid,]
    Store=fn.create.list(Scenarios$Scenario)
    for(s in 1:nrow(Scenarios))
    {
      print(paste('RatPack Outputs for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      sp_path_scen=paste(out.path.RatPack,Keep.species[i],Scenarios$Scenario[s],sep='/')
      spi=str_remove(Keep.species[i],' shark')
      xx=fn.get.RatPack.results(spi, PATH=sp_path_scen, last_yr=last.yr.EM)
      OMOut <- read.table(paste(sp_path_scen,'Results',paste0(spi,'_results_1.out'),sep='/'), 
                          skip=1, header=TRUE, fill=TRUE)
      Ufleet<- read.table(paste(sp_path_scen,'Results',paste0(spi,'_totcatch.out'),sep='/'), 
                          skip=1, header=TRUE, fill=TRUE)
      EMOut <- read.table(paste(sp_path_scen,'Debug',paste0(spi,'trace_plot.dat'),sep='/'), 
                          header=TRUE, fill=TRUE)
      
      #Get Kobe time series from EM Report file of final assessment year
      Store.Kobe=fn.create.list(1:niters)
      spi=paste0(str_remove(Keep.species[i],' shark'),'_sim_')
      for(k in 1:niters)
      {
        nn=paste0(spi,k,'_year_',last.yr.EM)
        Report=SS_output(paste0(sp_path_scen,'/Stock_Synthesis/',nn),covar=F,forecast=F,readwt=F)
        Store.Kobe[[k]]=Report$Kobe%>%mutate(Iteration=k)
        rm(Report)
      }
      Store[[s]]=list(d=xx$d,
                      dd1=xx$dd1,
                      d_rel_err=xx$d_rel_err,
                      d_current=xx$d_current,
                      OMOut=OMOut%>%
                            dplyr::select(c(AssessFail,Sim,Year,Period, any_of(Perf.ind))),
                      EMOut=EMOut,
                      Ufleet=Ufleet,
                      Kobe=do.call(rbind,Store.Kobe)%>%filter(Yr<=last.yr.EM))
      
      
      rm(OMOut,EMOut,Ufleet,xx)
    } #end s
    outputs_RatPack[[i]]=Store
  } #end i
  toc()
  
  #2. Plot figures
  #Adapt this....also see mi Ratpack code
  #1. Spawning stock biomass
  OMSSBquant=fn.percentiles(d=OMOut,
                            grouping='Year',
                            var='SSBcurrent')
  EMSSBquant=fn.percentiles(d=EMOut %>%
                              filter(RBCyear%in% Proj.assessment.years) %>%   
                              group_by(RBCyear) %>%
                              pivot_longer(cols=colnames(EMOut[3:ncol(EMOut)]),
                                           names_to="Year", values_to="estSSB", names_prefix="X"),
                            grouping=c('RBCyear','Year'),
                            var='estSSB')
  
  Quantiles=rbind(OMSSBquant%>%
                    data.frame%>%
                    mutate(Assessment='OM')%>%
                    relocate(Assessment),
                  EMSSBquant%>%
                    data.frame)%>%
    mutate(Species=Keep.species[i],
           Scenario=Scenarios$Scenario[s])
  #1. Calculate performance indicators
  Perform.ind
  Perf.ind=c(SSB='SSBcurrent',Depletion='Depletion',Recruits='Recruits',
             Catch='TotCatch',Catch.RBC='RBC')
  Perf.ind.list=fn.create.list(Perf.ind)
  for(p in 1:length(Perf.ind))
  {
    Perf.ind.list[[p]]=fn.percentiles(d=OMOut,grouping='Year',var=Perf.ind[[p]])%>%mutate(PerfInd=Perf.ind[[p]])
  }
  
  
  RBCyears <- sort(unique(EMSSBquant$RBCyear))
  EMSSBquant$Year <- as.numeric(EMSSBquant$Year)
  EMSSBquant$RBCyear <- factor(EMSSBquant$RBCyear, level=RBCyears)
  EMSSBquant <- rename(EMSSBquant, Assessment=RBCyear)
  ggplot(OMSSBquant, aes(x=Year)) +
    geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="gray", alpha=0.50) +
    geom_line(aes(y=middle)) +
    labs(y="SSB") +
    geom_line(data=EMSSBquant, aes(y=middle, color=Assessment, linetype=Assessment)) +
    geom_ribbon(data=EMSSBquant,aes(ymin=lower, ymax=upper, fill=Assessment), alpha=0.20) +
    scale_color_manual(values=rev(c("#08306b","cadetblue")))+ expand_limits(y=0)
  #ggsave(paste(sp_path_scen,'outputs','SSB_OM_EM.tiff',sep='/'),width = 6,height = 6,compression = "lzw")
  ggsave(paste0(outs.path,'/4_Quilt.plot_',out.exten,'.tiff'),width = 7,height = 10,compression = "lzw")
  
}