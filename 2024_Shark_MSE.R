# Header ---------------
# This script uses SSMSE and RatPack to perform Management Strategy Procedures to inform the development of the 
#   Shark resource Harvest Strategy based on the 4 indicator species

#notes: Alternative state of nature are considered thru multiple OM (Operating Model):
#             Alternative hypothesis on Steepness, Natural mortality, selectivity and illegal fishing
#       Alternative harvest control rules and reference points are considered thru multiple EM (Estimation Model):
#             X, Y, Z
#       RatPack .OPD, .HSE and .proj files are set up manually based on exported text file

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


#remotes::install_github("nmfs-fish-tools/SSMSE")
library(SSMSE)
library(r4ss)
library(tictoc)
library(tidyverse)
library(readxl)
#library(Hmisc)

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

#outputs
out.path.SSMSE=handl_OneDrive("Analyses/MSE/Shark harvest strategy/SSMSE")   
out.path.RatPack=handl_OneDrive("Analyses/MSE/Shark harvest strategy/RatPack")   


# Define MSE components ---------------
#Components: 
# 1. Management objectives
# 2. Operating Models
# 3. Estimation models (management scenarios)
# 4. Harvest control rules
# 5. Performance measures
# 6. Future data collection

hndl.mse.comp=handl_OneDrive("Analyses/MSE/Shark harvest strategy/Scenarios.xlsx")
Management_objectives=read_excel(hndl.mse.comp,  sheet = "Management objectives",skip = 0)
Operating_models=read_excel(hndl.mse.comp,       sheet = "Operating model",skip = 0)
Management_scenarios=read_excel(hndl.mse.comp,   sheet = "Estimation model",skip = 0)
harvest_control_rule=read_excel(hndl.mse.comp, sheet = "harvest control rule",skip = 0)
performance_indicators=read_excel(hndl.mse.comp, sheet = "Performance measure",skip = 0)
Future_data_collection=read_excel(hndl.mse.comp, sheet = "Future data collection",skip = 0)

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
First.Run.RatPack=FALSE                  
niters <- 2                            # number of simulations per scenario (100)
Proj.years=10                          # number of projected years (25)
Proj.years.obs=seq(1,Proj.years,by=5)  # sampled years in the projected period
Proj.years.between.ass=2               # years between assessments in the projected period
proj.CV=0.2                            # CV in the projected period

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
  SCENARIOS[[i]]=merge(Management_scenarios,OM_scenarios,by=NULL)%>%
    arrange(row.number,Ref.point)%>%
    mutate(Scenario=paste0('S',row_number()))%>%
    relocate(Ref.point,Scenario)%>%dplyr::select(-row.number)
  
}

# Run SSMSE loop over each species-scenario combination ---------------
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
    print(paste('SSMSE run for ',Keep.species[i],'    Scenario',Scenarios[s,],'-----------'))
    Indoktch=NULL
    if(!is.na(Scenarios$Difference[s]) & Scenarios$Difference[s]=="IUU")
    {
      Indoktch=Catch.species.dataset%>%filter(Name==Keep.species[i] & Data.set=="Indonesia")
    }
    fn.apply.SSMSE(sp_path_assessment, sp_path_OM,sp_path_EM, sp_path_out, Scen=Scenarios[s,], 
                       First.run=First.Run.SSMSE, proj.yrs=Proj.years, proj.yrs.with.obs=Proj.years.obs, 
                       yrs.between.assess=Proj.years.between.ass, future.cv=proj.CV, Neff.future=NULL, 
                       Nsims=niters, cur.fleets=c('Other','Southern.shark_2'))
  } #end s
  
} #end i
toc()



# Run RatPack loop over each species-scenario combination --------------

  #1. Create RatPack folders and files  
rat.pack.folders=c('inputs','outputs','PGMSY','Results','Stock_Synthesis','Stock_Synthesis3.30base')
rat.pack.files=c('run.bat','Whiskery_test.proj','Whiskery.OPD','Whiskery.HSE')

if(First.Run.RatPack)
{
  tic()
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    aaid=which(Scenarios$Difference%in%'rep.cycle')
    if(length(aaid)>0) Scenarios=Scenarios[-aaid,]  #cannot test rep cycle in RatPack
    for(s in 1:nrow(Scenarios))
    {
      print(paste('RatPack run for ',Keep.species[i],'    Scenario',Scenarios$Scenario[s],'-----------'))
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
                            scen=Scenarios[s,])
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
    }
  }
  toc()
}



#2. Execute RatPack #ACA
if(!First.Run.RatPack)
{
  tic()
  for(i in 1:N.sp)
  {
    Scenarios=SCENARIOS[[i]]
    aaid=which(Scenarios$Difference%in%'rep.cycle')
    if(length(aaid)>0) Scenarios=Scenarios[-aaid,]  #cannot test rep cycle in RatPack
    
    for(s in 1:nrow(Scenarios)) fn.run.RatPack.exe(where.exe=sp_path_scen,exe.name='run.bat')
  }
  toc()
}
   


