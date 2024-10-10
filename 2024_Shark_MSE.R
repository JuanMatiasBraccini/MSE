# Header ---------------
# This script uses SSMSE and RatPack to perform Management Strategy Procedures to inform the development of the 
#   Shark resource Harvest Strategy based on the 4 indicator species

#notes: Alternative states of nature are considered thru multiple OM (Operating Model):
#             alternative hypotheses on Steepness, Natural mortality, selectivity and illegal fishing
#       Alternative harvest control rules and reference points are considered thru multiple EM (Estimation Model):
#             alternative values of limit, threshold and target reference points
#       The Alternative OM and EM are defined in 'hndl.mse.comp' below based on the 'Scenarios' spreadsheet
#       Most files are set up automatically by this script but there is some manual tweaking to be done:
#           RatPack: manually update the .OPD, .HSE and .proj files in the 'inputs' folder using values 
#                    from input_HSE.txt and input_OPD.txt. Only do it for S1, then copy from S1 to other Scenarios and
#                    then manually update relevant parts only.
#           SSMSE:  For the OM and EM, Copy '#_Q_setup' and '#_Q_parms(if_any)' from 'control.ss_new' to 'control.ss'
#                   For the OM and EM and time changing parameters (i.e., blocks in Q or Selectivity). Copy, 
#                   'timevary Q parameters' or 'timevary selex parameters' from 'control.ss_new to 'control.ss' (check 'control_timevary_XX.csv')

# SSMSE reference material:
#     https://nmfs-fish-tools.github.io/SSMSE/manual
#     https://github.com/nmfs-fish-tools/SSMSE/blob/main/README.md
#     https://github.com/k-doering-NOAA/ssmse-wfc
#     https://noaa-fisheries-integrated-toolbox.github.io/SSMSE

#HCL
  #DPIRD's:
    # 50-100% effort reduction if performance indicator between limit and threshold
    # 10-50% effort reduction if performance indicator between threshold and target
  #MSC guidelines:
    # Bring back to threshold within 1 generation if between limit and threshold and 
    # within another generation if between threshold and target

rm(list=ls(all=TRUE))
#remotes::install_github("nmfs-fish-tools/SSMSE")
library(SSMSE)
library(r4ss)
library(tictoc)
library(tidyverse)
library(readxl)
library(doParallel)
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

#year latest stock assessment 
assessment.year=2022  

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
harvest_control_rule=read_excel(hndl.mse.comp, sheet = "harvest control rule",skip = 0)
performance_indicators=read_excel(hndl.mse.comp, sheet = "Performance measure",skip = 0)
Future_data_collection=read_excel(hndl.mse.comp, sheet = "Future data collection",skip = 0)

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
First.Run.SSMSE=FALSE                  # set to TRUE to generate OMs, Folders, etc
run.SSMSE=FALSE                        # set to TRUE to run SSMSE
First.Run.RatPack=FALSE                # don't run as it will over right OPD and HSE files
run.RatPack=FALSE                        # set to TRUE to run RatPack
niters <- 2                            # number of simulations per scenario (100)
Proj.years=10                          # number of projected years (25)
Proj.years.obs=seq(1,Proj.years,by=5)  # sampled years in the projected period
Proj.years.between.ass=4               # years between assessments in the projected period
proj.CV=0.2                            # CV in the projected period
Effective.pop.size.future=100          #future length comp sample size
theme_set(theme_light() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(colour = 'black'),
                  text = element_text(family = "Calibri", size = 12)))

Keep.species=sort(unique(Operating_models$Species))
N.sp=length(Keep.species)
species_logistic.selectivity.NSF=sort(unique(Operating_models%>%filter(!is.na(NSF.selectivity))%>%pull(Species)))  
species_IUU_indonesia=sort(unique(Operating_models%>%filter(!is.na(Indo.IUU))%>%pull(Species)))    

subset.scenarios=TRUE  #test scenario grid?? way time consuming

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
Not.tested.in.Rat.Pack='rep.cycle' #cannot test rep cycle in RatPack

# Run SSMSE loop over each species-scenario combination ---------------
Current.fleets=fn.create.list(Keep.species)
Current.fleets$`Gummy shark`=Current.fleets$`Whiskery shark`=c("Other","Southern.shark_2")
Current.fleets$`Dusky shark`=Current.fleets$`Sandbar shark`=c("Other","Southern.shark_2","Survey")

#1. Create SSMSE folders and files
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
      fn.create.SSMSE.files(sp_path_assessment, sp_path_OM, sp_path_EM, Scen=Scenarios[s,], 
                            proj.yrs=Proj.years, block.pattern=BLK.pat[[i]])
    } #end s
    rm(sp_path_assessment,sp_path_OM,sp_path_EM,Scenarios)
  } #end i
  toc()
}


#2. Execute SSMSE
if(run.SSMSE)
{
  SSMSE_outputs=fn.create.list(Keep.species)
  for(i in 1:N.sp)
  {
    dumy.out=fn.create.list(Scenarios$Scenario)
    
    Scenarios=SCENARIOS[[i]]  
    sp_path_assessment=paste(in.path,paste0('1.',Keep.species[i]),assessment.year,'SS3 integrated',sep='/')
    sp_path_OM=paste(out.path.SSMSE,Keep.species[i],'OM',sep='/')
    sp_path_EM=paste(out.path.SSMSE,Keep.species[i],'EM',sep='/')
    sp_path_out=paste(out.path.SSMSE,Keep.species[i],'Outputs',sep='/')
    for(s in 1:nrow(Scenarios))
    {
      print(paste('SSMSE run for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      dumy.out[[s]]=fn.run.SSSMSE(sp_path_assessment,Scen=Scenarios[s,], sp_path_OM, sp_path_EM, sp_path_out,
                                  Nsims=niters, Neff.future=Effective.pop.size.future,
                                  proj.yrs=Proj.years, proj.yrs.with.obs=Proj.years.obs, 
                                  yrs.between.assess=Proj.years.between.ass,
                                  cur.fleets=Current.fleets[[i]], future.cv=proj.CV,
                                  specify.future.OM=FALSE, apply.future.meanbodywt=FALSE) #doesn't work with future meanbodywt
    } #end s
    SSMSE_outputs[[i]]=dumy.out
    rm(sp_path_OM,sp_path_EM,Scenarios,sp_path_out)
  } #end i
}


# Run RatPack loop over each species-scenario combination --------------

#1. Create RatPack folders and files  
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
      
      #manually populate HSE using this info
      dumy=fun.populate.HSE(i,SS.path=paste(in.path,paste0('1.',Keep.species[i]),assessment.year,'SS3 integrated',
                                            Scenarios$Assessment.path[s],sep='/'),
                            scen=Scenarios[s,], proj.yrs=Proj.years, yrs.between.assess=Proj.years.between.ass)
      sink(paste0(sp_path_scen,'/inputs/input_HSE.txt'))
      print(dumy,row.names=F)
      sink()
      rm(dumy)
      
      #manually populate OPD using this info   
      dumy=fun.populate.OPD(i,SS.path=paste(in.path,paste0('1.',Keep.species[i]),assessment.year,'SS3 integrated',
                                            Scenarios$Assessment.path[s],sep='/'),
                            Scen=Scenarios[s,])
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
  tic()
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    aaid=which(Scenarios$Difference%in%Not.tested.in.Rat.Pack)
    if(length(aaid)>0) Scenarios=Scenarios[-aaid,]  
    for(s in 1:nrow(Scenarios))
    {
      print(paste('RatPack run for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      sp_path_scen=paste(out.path.RatPack,Keep.species[i],Scenarios$Scenario[s],sep='/')
      fn.run.RatPack.exe(where.exe=sp_path_scen,exe.name='run.bat')
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
if(run.SSMSE)
{
  tic()
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    for(s in 1:nrow(Scenarios))
    {
      print(paste('SSMSE Outputs for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
      
    } #end s
  } #end i
  toc()
}
# Report Ratpack outputs  ----------------------------------------------------------
if(run.RatPack)
{
  outputs_RatPack=fn.create.list(Keep.species)
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
      
      #Bring in OM and EM results
      OMOut <- read.table(paste(sp_path_scen,'Results',paste0(spi,'_results_1.out'),sep='/'), 
                          skip=1, header=TRUE, fill=TRUE)
      EMOut <- read.table(paste(sp_path_scen,'Debug',paste0(spi,'trace_plot.dat'),sep='/'), 
                          header=TRUE, fill=TRUE)
      
      #1. Calculate performance indicators
      Perf.ind=c(Stock.stat1='SSBcurrent',Stock.stat2='Depletion',Stock.stat3='Recruits',
                 Catch='TotCatch',Catch1='RBC')
      Perf.ind.list=fn.create.list(Perf.ind)
      for(p in 1:length(Perf.ind))
      {
        Perf.ind.list[[p]]=fn.percentiles(d=OMOut,grouping='Year',var=Perf.ind[[p]])%>%mutate(PerfInd=Perf.ind[[p]])
      }
      
      #1. Spawning stock biomass
      OMSSBquant=fn.percentiles(d=OMOut,
                                grouping='Year',
                                var='SSBcurrent')
      EMSSBquant=fn.percentiles(d=EMOut %>%
                                    filter(RBCyear==2022 | RBCyear==2025) %>%   #ACA, why these years??
                                    group_by(RBCyear) %>%
                                    pivot_longer(cols=colnames(EMOut[3:ncol(EMOut)]),
                                                 names_to="Year", values_to="estSSB", names_prefix="X"),
                                grouping=c('RBCyear','Year'),
                                var='estSSB')
      
      #plot trajectories
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
      ggsave(paste(sp_path_scen,'outputs','SSB_OM_EM.tiff',sep='/'),width = 6,height = 6,compression = "lzw")
      
      Store[[s]]=rbind(OMSSBquant%>%
                         data.frame%>%
                         mutate(Assessment='OM')%>%
                         relocate(Assessment),
                       EMSSBquant%>%
                         data.frame)%>%
                  mutate(Species=Keep.species[i],
                         Scenario=Scenarios$Scenario[s])
    } #end s
    outputs_RatPack[[i]]=Store
  } #end i
  toc()
  
  outputs_RatPack=do.call(rbind,outputs_RatPack)
  
}