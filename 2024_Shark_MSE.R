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
niters <- 2                            # number of simulations per scenario (100)
assessment.year=2022                   # year latest stock assessment 
last.year.obs.catch=2021               # last year with observed catch
Proj.years=10                          # number of projected years (25)
Proj.years.between.ass=4               # years between assessments in the projected period
Proj.years.obs=seq(1,Proj.years,by=1)  # sampled years in the projected period
Proj.assessment.years=seq((assessment.year+1),(assessment.year+Proj.years),Proj.years.between.ass)[-1]
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
  tic()   #19 sec per species-scenario-simulation-proj.year (not in parallel)
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
OVERWRITE=FALSE #set to TRUE if SSMSE is re run, otherwise set to FALSE to speed up reporting
if(run.SSMSE)
{
  output_dis.pi.list=fn.create.list(Keep.species)
  Check.SSMSE.convergence=Get.SSMSE.perf.indic=output_boxplot.pi.list=output_quilt.list=output_dis.pi.list
  tic()
  for(i in 1:N.sp)
  {
    print(paste('SSMSE create figures for ',Keep.species[i],'-----------'))
    Scenarios=SCENARIOS[[i]]
    
    #1. Extract quantities
    summary <- SSMSE_summary_all(paste(out.path.SSMSE,Keep.species[i],'Outputs',sep='/'),overwrite = OVERWRITE)
    
      #1.1 Performance indicators
    BMSY=median(summary$scalar$SSB_MSY,na.rm=T)
    FMSY=median(summary$scalar$F_MSY,na.rm=T)
    Get.SSMSE.perf.indic[[i]]=summary$dq%>%
      rename(Recruits=Value.Recr,
             SSB=Value.SSB,
             Depletion=Value.Bratio,
             Catch=Value.ForeCatch)%>%
      mutate(model_run=sub(".*?_", "", model_run),
             B.over.BMSY=SSB/BMSY,
             F.over.FMSY=Value.F/FMSY,)%>%
      dplyr::select(c(model_run,iteration,scenario,year,F.over.FMSY,B.over.BMSY,any_of(Perform.ind)))
    
    
    #fn.ktch.perf.ind(ktch=)  #calculates total catch and variability by simulation. Get the median as PI
    
    
      #1.2 Check convergence
    Check.SSMSE.convergence[[i]] <- check_convergence_SSMSE(summary = summary,
                                                            min_yr = last.year.obs.catch+1,
                                                            max_yr = last.year.obs.catch+Proj.years)
    
    #Save Kobe plot
    scen.list_kobe=fn.create.list(Scenarios$Scenario)
    for(s in 1:nrow(Scenarios))
    {
      f.traj=Get.SSMSE.perf.indic[[i]]%>%  #ACA: extra median series across iters and probs for scenario last EM
                filter(scenario=Scenarios$Scenario[s])
      scen.list_kobe[[s]]=kobePlot(f.traj=c(0,0.1,0.15,0.25,0.6,0.8,1,1.1,1.5,1.1,0.9),   # replace with real x and y outputs
                                   b.traj=c(2,1.8,1.6,1.4,1.2,1.1,1,0.7,0.5,0.8,0.9),
                                   Years=1:11,
                                   Titl=Scenarios$Scenario[s],
                                   Probs=data.frame(x=rnorm(1e3,0.9,0.05),  #ACA: replace with real x and y probs for final year
                                                     y=rnorm(1e3,0.9,0.05)),
                                   pt.size=2.5,
                                   txt.col='transparent',
                                   line.col='black',
                                   YrSize=4,
                                   YLAB='', XLAB='',
                                   Leg.txt.size=10)
      
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
    ggsave(paste0(outs.path,'/2_Kobe_SSMSE_',Keep.species[i],'.tiff'),width = wid,height = 12,compression = "lzw")
    

    #Save distribution of performance indicators   Perform.ind
    #ACA: first must calculate relative value of each indicator for each scenario
    N.dumi=100  #delete
    output_dis.pi.list[[i]]=fn.perf.ind.dist(df=expand.grid(Scenario=rep(Scenarios$Scenario,each=N.dumi),    #aca replace with real density for final year
                                                          Perf.ind=Perform.ind)%>%
                                              mutate(Value=sample(x=seq(0,1,by=0.1),size=length(Scenarios$Scenario)*length(Perform.ind)*N.dumi,replace=T)),
                                            YLAB='',
                                            Title=Keep.species[i])
    output_boxplot.pi.list[[i]]=fn.perf.ind.boxplot(df=expand.grid(Scenario=rep(Scenarios$Scenario,each=N.dumi),    #aca replace with real density for final year
                                                                   Perf.ind=Perform.ind)%>%
                                                      mutate(Value=sample(x=seq(0,1,by=0.1),size=length(Scenarios$Scenario)*length(Perform.ind)*N.dumi,replace=T)),
                                                   YLAB='',
                                                   Title=Keep.species[i])        
    
    #Save performance indicator polar plots
    fn.polar.plot(data= expand.grid(Scenario=Scenarios$Scenario,    #aca replace with real values
                                    Indicator=Perform.ind)%>%
                              mutate(value=sample(x=seq(0.1,1,by=0.1),size=length(Scenarios$Scenario)*length(Perform.ind),replace=T)),
                  Title= "")
    ggsave(paste0(outs.path,'/3_Polar.plot_SSMSE_',Keep.species[i],'.tiff'),width = 6,height = 8,compression = "lzw")
    
    
    #Save quilt plot
    #ACA: first must calculate relative value of each indicator for each scenario
    output_quilt.list[[i]]=fn.quilt.plot(df=expand.grid(Per.ind=Perform.ind,Scenario=Scenarios$Scenario)%>%arrange(Per.ind)%>%
                                           mutate(N=sample(seq(0,1,.1),n(),replace=T))%>%
                                           spread(Per.ind,N)%>%
                                           `rownames<-`(Scenarios$Scenario)%>%
                                           dplyr::select(-Scenario),
                                        clr.scale=colorRampPalette(c('white','cadetblue2','cornflowerblue')),
                                        col.breaks=50,
                                        Titl=Keep.species[i]) 
    

    
  } #end i
  
  #Output combined distribution of performance indicators
  ggarrange(plotlist = output_dis.pi.list,nrow=1,common.legend = FALSE)%>%
    annotate_figure(left = textGrob('Density distribution', rot = 90, vjust = 1, gp = gpar(cex = 1.7)))
  ggsave(paste0(outs.path,'/1_Perf_indicator_distribution.plot_SSMSE.tiff'),width = 10.5,height = 8,compression = "lzw")
  
  ggarrange(plotlist = output_boxplot.pi.list,nrow=1)%>%
    annotate_figure(left = textGrob('Indicator value', rot = 90, vjust = 1, gp = gpar(cex = 1.7)))
  ggsave(paste0(outs.path,'/1_Perf_indicator_box.plot_SSMSE.tiff'),width = 10,height = 8,compression = "lzw")
  
  #Output combined quilt plot
  ggarrange(plotlist = output_quilt.list,ncol=1)
  ggsave(paste0(outs.path,'/4_Quilt.plot_SSMSE.tiff'),width = 6,height = 10,compression = "lzw")

  
  #Missing. Add time series of key performance indicators and display like '1_Perf_indicator_distribution.plot_SSMSE'
  
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
                                    filter(RBCyear%in% Proj.assessment.years) %>%   
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