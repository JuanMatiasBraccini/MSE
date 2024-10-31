# Miscellaneous -----------------------------------------------------------------
fn.remove.subfolder=function(Loc)  shell( glue::glue("rmdir /s /q \"{Loc}\" ") )
fn.rename=function(from, to) file.rename(from, to)
fn.create.folder=function(x) if(!dir.exists(x)) dir.create(x)
fn.copy.file=function(x,from,to)
{
  if(!file.exists(paste0(to,'/',x)))
  {
    invisible(file.copy(from=paste0(from,"/", x), to=to, recursive = TRUE))
  }
}
fn.file.remove=function(file) if(file.exists(file))file.remove(file) 
fn.get=function(parname,from) from[match(parname,rownames(from)),]$INIT
fn.create.list=function(vec) sapply(vec,function(x) NULL)   
fn.right.format=function(x,var)
{
  return(x%>%
           dplyr::select(!!sym(var))%>%
           rename(variable=!!sym(var))%>%
           rownames_to_column()%>%
           mutate(dummy=ifelse(grepl(paste(c('PRet','PDis','PMalOff'),collapse = '|'),rowname),'drop','keep'))%>%
           filter(dummy=='keep')%>%
           dplyr::select(-dummy)%>%
           mutate(rowname=str_remove(rowname,'SizeSel_'),
                  fleet=stringr::str_extract(string = rowname, pattern = "(?<=\\().*(?=\\))"),
                  parameter=sub("^(([^_]*_){1}[^_]*).*", "\\1", rowname))%>%
           dplyr::select(-rowname)%>%
           spread(parameter,variable,fill=0)%>%
           mutate(P_7=0,P_8=0))
}
my_theme=function(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,cap.siz=10,
                  lgT.siz=14,leg.siz=12,axs.t.siz=10,axs.T.siz=14)
{
  #font<-windowsFonts("Arial" = windowsFont("Arial"))
  #font="Arial"  #"TT Courier New"  "TT Arial"  "TT Times New Roman"
  theme_bw()%+replace% 
    theme(
      
      #panel
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, linewidth=1.15),
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.line = element_line(colour = "black"),
      #axis.ticks = element_line(),          #strip axis ticks
      
      # strip background
      strip.background = element_rect(
        fill = "grey90",
        colour = "grey90"),
      
      #text elements
      #title
      plot.title = element_text(             
        # family = font,                           
        size = Ttl.siz,                         
        face = 'bold',                           #bold typeface
        hjust = 0,                               #left align
        vjust = 2),                              #raise slightly
      
      #subtitle
      plot.subtitle = element_text(          
        # family = font,                           
        size = Sbt.siz,
        hjust = 0,                               #left align
        vjust = 2),                         
      
      #strip legend
      strip.text = element_text(
        # family = font,
        size = str.siz),
      strip.text.x = element_text(
        # family = font,
        size = strx.siz,
        margin = margin(.1,0,.1,0, "cm")),
      
      
      #caption
      plot.caption = element_text(          
        # family = font,                           
        size = cap.siz,                          
        hjust = 1),                             #right align
      
      #legend
      legend.title=element_text(
        # family = font,
        size=lgT.siz),
      legend.text=element_text(
        # family = font,
        size=leg.siz),
      
      #axis titles
      axis.title = element_text(             
        # family = font,                          
        size = axs.T.siz),                     
      
      #axis text
      axis.text = element_text(              
        # family = font,                          
        size = axs.t.siz)                       
    )
}

fn.ktch.perf.ind=function(ktch)
{
  ktch=subset(ktch,!is.na(ktch))
  ktch.var=data.frame(C=ktch)%>%
    mutate(C.prev=lag(C),
           C.delta=abs(C-C.prev))%>%
    filter(!is.na(C.prev))
  AAV=100*sum(ktch.var$C.delta)/sum(ktch.var$C)
  return(list(AAV=AAV,Total=sum(ktch)))
}

# SSMSE -----------------------------------------------------------------
fn.create.SSMSE.files=function(sp_path_assessment,sp_path_OM,sp_path_EM,Scen,block.pattern)
{
  #1. Bring in Assessment model
  Assessment.location=paste(sp_path_assessment,Scen$Assessment.path,sep='/')
  list.of.files <-list.files(Assessment.location) %>%
    stringr::str_subset(., paste(c("plots","Diagnostics","sigma 0.4","ss.b0","ss.p0","ss.r0","admodel",
                                   "console.output.txt","ss.cor","ss.eva","suggested_tuning.ss","ss.std",
                                   "A no var adj","estim male offset","MonteCarlo",".tiff",".csv"),
                                 collapse = '|'), negate = TRUE)
  
  
  #2. Create OM and EM based on Assessment model
  scen_path_OM=paste(sp_path_OM,Scen$Scenario,sep='/')
  scen_path_EM=paste(sp_path_EM,Scen$Scenario,sep='/')
  fn.create.folder(scen_path_OM)
  fn.create.folder(scen_path_EM)
  unlink(paste0(scen_path_OM,"/*"))
  unlink(paste0(scen_path_EM,"/*"))
  invisible(lapply(list.of.files, function(x) file.copy(paste(Assessment.location, x, sep = "/"), to = scen_path_OM, recursive = TRUE)))
  invisible(lapply(list.of.files, function(x) file.copy(paste(Assessment.location, x, sep = "/"), to = scen_path_EM, recursive = TRUE)))
 
  
  #3. Bring in OM and update for scenarios not tested during the Assessment 
  Report=SS_output(scen_path_EM,covar=F,forecast=F,readwt=F,verbose = F, printstats=F) 
  
  fore.OM <- r4ss::SS_readforecast(file.path(scen_path_OM, "forecast.ss"),verbose = FALSE)
  
  dat <- r4ss::SS_readdat(file.path(scen_path_OM, "data.dat"),verbose = FALSE)
  dat.new <- r4ss::SS_readdat(file.path(scen_path_OM, "data_echo.ss_new"),verbose = FALSE)
  dat.EM <- r4ss::SS_readdat(file.path(scen_path_EM, "data.dat"),verbose = FALSE)
  
  control <- r4ss::SS_readctl(file.path(scen_path_OM, "control.ctl"),verbose = FALSE,
                              datlist=file.path(scen_path_OM, "data_echo.ss_new"))
  control.new <- r4ss::SS_readctl(file.path(scen_path_OM, "control.ss_new"),verbose = FALSE,
                                  datlist=file.path(scen_path_OM, "data_echo.ss_new"))
  control.EM <- r4ss::SS_readctl(file.path(scen_path_EM, "control.ctl"),verbose = FALSE,
                                 datlist=file.path(scen_path_EM, "data_echo.ss_new"))
  
  par.file <- SS_readRatPack.object(file.path(scen_path_OM,'ss.par'))
  
  if(!is.na(Scen$Difference))
  {
    if(Scen$Difference=='NSF.logis.sel')
    {
      control$size_selex_types[match('Northern.shark',row.names(control$size_selex_types)),c('Pattern','Male')]=c(1,0)
      control.new$size_selex_types=control$size_selex_types
      
      id=match(c("SizeSel_P_3_Northern.shark(1)","SizeSel_P_4_Northern.shark(1)",
                 "SizeSel_P_5_Northern.shark(1)","SizeSel_P_6_Northern.shark(1)",
                 "SizeSel_PMalOff_1_Northern.shark(1)","SizeSel_PMalOff_2_Northern.shark(1)",
                 "SizeSel_PMalOff_3_Northern.shark(1)","SizeSel_PMalOff_4_Northern.shark(1)",
                 "SizeSel_PMalOff_5_Northern.shark(1)"),
               row.names(control$size_selex_parms))
      id=subset(id,!is.na(id))
      control$size_selex_parms=control$size_selex_parms[-id,] 
      control.new$size_selex_parms=control.new$size_selex_parms[-id,]
      
      id=match('SizeSel_P_1_Northern.shark(1)',row.names(control$size_selex_parms))
      aaa=control$MG_parms[match('Mat50%_Fem_GP_1',row.names(control$MG_parms)),'INIT']
      control$size_selex_parms[id,c('LO','HI','INIT','PRIOR')]=c(aaa*.8,aaa*1.2,aaa,aaa)
      control.new$size_selex_parms[id,c('LO','HI','INIT','PRIOR')]=c(aaa*.8,aaa*1.2,aaa,aaa)
      
      id=match('SizeSel_P_2_Northern.shark(1)',row.names(control$size_selex_parms))
      control$size_selex_parms[id,c('LO','HI','INIT','PRIOR')]=c(1,20,10,10)
      control.new$size_selex_parms[id,c('LO','HI','INIT','PRIOR')]=c(1,20,10,10)
      
    }
    if(Scen$Difference=='lower.steepness')
    {
      control$SR_parms[match('SR_BH_steep',row.names(control$SR_parms)),c('LO','INIT')]=c(0.25,Scen$Difference.value)
      control.new$SR_parms[match('SR_BH_steep',row.names(control$SR_parms)),c('LO','INIT')]=c(0.25,Scen$Difference.value)
    }
    if(Scen$Difference=='M.contstant')
    {
      nn=ncol(control$natM)
      control$natM[1,]=rep(Scen$Difference.value,nn)
      control$natM[2,]=rep(Scen$Difference.value,nn)
      control.new$natM[1,]=rep(Scen$Difference.value,nn)
      control.new$natM[2,]=rep(Scen$Difference.value,nn)
    }
    if(Scen$Difference=='rep.cycle')
    {
      control$MG_parms[grep('Eggs',row.names(control$MG_parms)),'INIT']=
              control$MG_parms[grep('Eggs',row.names(control$MG_parms)),'INIT']*Scen$Difference.value
      control.new$MG_parms[grep('Eggs',row.names(control.new$MG_parms)),'INIT']=
        control.new$MG_parms[grep('Eggs',row.names(control.new$MG_parms)),'INIT']*Scen$Difference.value


    }
    if(Scen$Difference=='IUU')
    {
      id=match('Other',dat$fleetinfo$fleetname)
      a=dat$catch%>%filter(fleet==id)
      dat$catch=dat$catch%>%filter(!fleet==id)
      dat.new$catch=dat.new$catch%>%filter(!fleet==id)
      a=left_join(a,Indoktch%>%
                    dplyr::select(Year,catch)%>%rename(catch1=catch),
                  by=c('year'='Year'))%>%
                  mutate(catch1=ifelse(is.na(catch1),0,catch1),
                         catch=catch-catch1,
                         catch=catch+(catch1*Scen$Difference.value))%>%
                  dplyr::select(-catch1)
      dat$catch=rbind(a,dat$catch)%>%arrange(fleet,year,seas)
      dat.new$catch=rbind(a,dat.new$catch)%>%arrange(fleet,year,seas)
    }
  }
  
  #Nathan Vaughan suggestions
  id.F.fleet=which(dat$fleetinfo$fleetname=='F.series_Southern.shark_1')  #remove F series
  if(length(id.F.fleet)>0)
  {
    fore.OM$ForeCatch=fore.OM$ForeCatch%>%filter(!Fleet==id.F.fleet)
    
    dat$Nfleets=dat$Nfleets-1
    dat$Nfleet=dat$Nfleet-1
    dat$fleetinfo=dat$fleetinfo[-id.F.fleet,]
    dat$fleetinfo1=dat$fleetinfo1[,-id.F.fleet]
    dat$fleetinfo2=dat$fleetinfo2[,-id.F.fleet]
    dat$fleetnames=dat$fleetnames[-id.F.fleet]
    dat$surveytiming=dat$surveytiming[-id.F.fleet]
    dat$units_of_catch=dat$units_of_catch[-id.F.fleet]
    dat$areas=dat$areas[-id.F.fleet]
    dat$catch=dat$catch%>%filter(!fleet==id.F.fleet)
    dat$CPUEinfo=dat$CPUEinfo[-id.F.fleet,]
    dat$CPUE=dat$CPUE%>%filter(!index==id.F.fleet)
    dat$len_info=dat$len_info[-id.F.fleet,]
    
    dat.new$Nfleets=dat.new$Nfleets-1
    dat.new$Nfleet=dat.new$Nfleet-1
    dat.new$fleetinfo=dat.new$fleetinfo[-id.F.fleet,]
    dat.new$fleetinfo1=dat.new$fleetinfo1[,-id.F.fleet]
    dat.new$fleetinfo2=dat.new$fleetinfo2[,-id.F.fleet]
    dat.new$fleetnames=dat.new$fleetnames[-id.F.fleet]
    dat.new$surveytiming=dat.new$surveytiming[-id.F.fleet]
    dat.new$units_of_catch=dat.new$units_of_catch[-id.F.fleet]
    dat.new$areas=dat.new$areas[-id.F.fleet]
    dat.new$catch=dat.new$catch%>%filter(!fleet==id.F.fleet)
    dat.new$CPUEinfo=dat.new$CPUEinfo[-id.F.fleet,]
    dat.new$CPUE=dat.new$CPUE%>%filter(!index==id.F.fleet)
    dat.new$len_info=dat.new$len_info[-id.F.fleet,]
    
    dat.EM$Nfleets=dat.EM$Nfleets-1
    dat.EM$Nfleet=dat.EM$Nfleet-1
    dat.EM$fleetinfo=dat.EM$fleetinfo[-id.F.fleet,]
    dat.EM$fleetinfo1=dat.EM$fleetinfo1[,-id.F.fleet]
    dat.EM$fleetinfo2=dat.EM$fleetinfo2[,-id.F.fleet]
    dat.EM$fleetnames=dat.EM$fleetnames[-id.F.fleet]
    dat.EM$surveytiming=dat.EM$surveytiming[-id.F.fleet]
    dat.EM$units_of_catch=dat.EM$units_of_catch[-id.F.fleet]
    dat.EM$areas=dat.EM$areas[-id.F.fleet]
    dat.EM$catch=dat.EM$catch%>%filter(!fleet==id.F.fleet)
    dat.EM$CPUEinfo=dat.EM$CPUEinfo[-id.F.fleet,]
    dat.EM$CPUE=dat.EM$CPUE%>%filter(!index==id.F.fleet)
    dat.EM$len_info=dat.EM$len_info[-id.F.fleet,]
    
    
    control$fleetnames=control$fleetnames[-id.F.fleet]
    control$Nfleets=control$Nfleets-1
    control$Q_options=control$Q_options%>%filter(!fleet==id.F.fleet)
    control$Q_parms=control$Q_parms[-grep('F.series',rownames(control$Q_parms)),]
    control$size_selex_types=control$size_selex_types[-id.F.fleet,]
    control$age_selex_types=control$age_selex_types[-id.F.fleet,]
    control$lambdas=control$lambdas%>%filter(!fleet==id.F.fleet)  
    control$N_lambdas=control$N_lambdas-1  
      
    control.new$fleetnames=control.new$fleetnames[-id.F.fleet]
    control.new$Nfleets=control.new$Nfleets-1
    control.new$Q_options=control.new$Q_options%>%filter(!fleet==id.F.fleet)
    control.new$Q_parms=control.new$Q_parms[-grep('F.series',rownames(control.new$Q_parms)),]
    control.new$size_selex_types=control.new$size_selex_types[-id.F.fleet,]
    control.new$age_selex_types=control.new$age_selex_types[-id.F.fleet,]
    control.new$lambdas=control.new$lambdas%>%filter(!fleet==id.F.fleet)  
    control.new$N_lambdas=control.new$N_lambdas-1 
    
    
    control.EM$fleetnames=control.EM$fleetnames[-id.F.fleet]
    control.EM$Nfleets=control.EM$Nfleets-1
    control.EM$Q_options=control.EM$Q_options%>%filter(!fleet==id.F.fleet)
    control.EM$Q_parms=control.EM$Q_parms[-grep('F.series',rownames(control.EM$Q_parms)),]
    control.EM$size_selex_types=control.EM$size_selex_types[-id.F.fleet,]
    control.EM$age_selex_types=control.EM$age_selex_types[-id.F.fleet,]
    control.EM$lambdas=control.EM$lambdas%>%filter(!fleet==id.F.fleet)  
    control.EM$N_lambdas=control.EM$N_lambdas-1  
    
  }
  
  par.file[[match("# Fcast_recruitments:",par.file)+1]]=" 0.00000000000 0.00000000000"
  control$recdev_early_start=control.new$recdev_early_start=control.EM$recdev_early_start=1922 #dat$styr-dat$Nages 
  control$recdev_early_phase=control.new$recdev_early_phase=control.EM$recdev_early_phase=3
  control$Fcast_recr_phase=control.new$Fcast_recr_phase=control.EM$Fcast_recr_phase=-2
  control$time_vary_auto_generation[control$time_vary_auto_generation<1]=1
  control.new$time_vary_auto_generation[control.new$time_vary_auto_generation<1]=1
  control.EM$time_vary_auto_generation[control.EM$time_vary_auto_generation<1]=1
  if('CPUE'%in%names(dat))
  {
    dat$CPUE$seas=dat.new$CPUE$seas=dat.EM$CPUE$seas=7
    control$Q_options$float=0
    control$Q_parms$PHASE=2
    control.new$Q_options$float=0
    control.new$Q_parms$PHASE=2
    control.EM$Q_options$float=0
    control.EM$Q_parms$PHASE=2
  }
  if(!is.null(block.pattern))
  {
    iid=match(block.pattern,rownames(Report$estimated_non_dev_parameters))
    if(any(grepl('LnQ',block.pattern)))
    {
      dum=Report$estimated_non_dev_parameters[iid,]%>%
        dplyr::select(Value)%>%
        rename(INIT=Value)%>%
        mutate(LO=ifelse(INIT<0,INIT*2,INIT/2),
               HI=ifelse(INIT<0,INIT/2,INIT*2),
               PRIOR=0,
               PR_SD=1,
               PR_type=0,
               PHASE=2)%>%
        relocate(LO,HI,INIT,PRIOR,PR_SD,PR_type,PHASE)
      #control.EM=append(control.EM,values=list('timevary Q parameters'=dum),after=match('Q_parms',names(control.EM)))
      write.csv(dum,file.path(scen_path_EM, 'control_timevary_Q_parameters.csv'),row.names = T)
    }
    if(any(grepl('Size_',block.pattern)))
    {
      dum=Report$estimated_non_dev_parameters[iid,]%>%
                dplyr::select(Value)%>%
                rename(INIT=Value)%>%
                mutate(LO=ifelse(INIT<0,INIT*2,INIT/2),
                       HI=ifelse(INIT<0,INIT/2,INIT*2),
                       PRIOR=0,
                       PR_SD=0.3,
                       PR_type=6,
                       PHASE=4)%>%
                relocate(LO,HI,INIT,PRIOR,PR_SD,PR_type,PHASE)
      write.csv(dum,file.path(scen_path_EM, 'control_timevary_selex_parameters.csv'),row.names = T)
      
    }
  }
  file.remove(file.path(scen_path_OM,'ss.par'))
  zz <- file(file.path(scen_path_OM,'ss.par'), open = "at")
  sink(zz)
  for(x in 1:length(par.file)) writeLines(paste(par.file[[x]],collapse=' '),con = zz) 
  sink()
  close(zz) 
  r4ss::SS_writedat(datlist=dat, outfile=file.path(scen_path_OM, 'data.dat'), verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writectl(ctllist=control, outfile=file.path(scen_path_OM, 'control.ctl'), verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writedat(datlist=dat.new, outfile=file.path(scen_path_OM, 'data_echo.ss_new'), verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writectl(ctllist=control.new, outfile=file.path(scen_path_OM, 'control.ss_new'), verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writedat(datlist=dat.EM, outfile=file.path(scen_path_EM, 'data.dat'), verbose = FALSE, overwrite = TRUE)
  r4ss::SS_writectl(ctllist=control.EM, outfile=file.path(scen_path_EM, 'control.ctl'), verbose = FALSE, overwrite = TRUE)
  
  
  #4. Add changes in OM model structure for future projections
  fore.OM$Nforecastyrs=1
  fore.OM=fore.OM[-match('ForeCatch',names(fore.OM))]
  fore.OM$ControlRuleMethod=0 # 0, don't use a ramp HCR at all; 1, catch as function of SSB; 2, F as function of SSB 
  r4ss::SS_writeforecast(fore.OM, scen_path_OM, verbose = FALSE, overwrite = TRUE)
  
  fore_new.OM <- r4ss::SS_readforecast(file.path(scen_path_OM, "forecast.ss_new"),verbose = FALSE)
  fore_new.OM$Nforecastyrs=1
  fore_new.OM=fore_new.OM[-match('ForeCatch',names(fore_new.OM))]
  fore_new.OM$ControlRuleMethod=0 # 0, don't use a ramp HCR at all; 1, catch as function of SSB; 2, F as function of SSB 
  r4ss::SS_writeforecast(fore_new.OM, scen_path_OM, verbose = FALSE,file = "forecast.ss_new", overwrite = TRUE)

  
  #5. Set up Management procedure (EM)
    #get quantities of interest
  dum=Report$derived_quants
  SPR_Btgt=round(dum[grep("SPR_Btgt",dum$Label),'Value'],2)  
  SSB_Virgin=dum[grep("SSB_Virgin",dum$Label),'Value']
  SSB_MSY=dum[grep("SSB_MSY",dum$Label),'Value']
  SSB_MSY.over.SSB_Virgin=SSB_MSY/SSB_Virgin
  Min.bio=0.2
  if(Scen$Limit==0.4) Min.bio=0.15
  if(Scen$Limit==0.6) Min.bio=0.22
  BLimit=round(max(Min.bio,Scen$Limit*SSB_MSY.over.SSB_Virgin),2)   
  BThreshold=round(Scen$Threshold*SSB_MSY.over.SSB_Virgin,2)
  BTarget=round(Scen$Target*SSB_MSY.over.SSB_Virgin,2)
  rm(Report,dum,SSB_MSY.over.SSB_Virgin)
  
    #bring in forecast file
  fore <- r4ss::SS_readforecast(file.path(scen_path_EM, "forecast.ss"),verbose = FALSE) 
  if(length(id.F.fleet)>0) fore$ForeCatch=fore$ForeCatch%>%filter(!Fleet==id.F.fleet)
  
  
  fore$MSY <- 2 # 1, FSPR; 2 FMSY; 3, FBtarget
  fore$Forecast <- 2 # 1, use FSPR; 2, use FMSY; 3, F
  fore$Btarget <- BTarget  #value of relative biomass aimed to be achieved during forecast
  fore$SPRtarget <- SPR_Btgt
  fore$ControlRuleMethod <- 0 # 0, don't use a ramp HCR at all; 1, catch as function of SSB; 2, F as function of SSB 
  fore$BforconstantF <- BTarget
  fore$BfornoF <- BLimit
  fore=fore[-match('ForeCatch',names(fore))]
  fore$Nforecastyrs=1
  r4ss::SS_writeforecast(fore, scen_path_EM, verbose = FALSE, overwrite = TRUE)
  
  fore_new.EM <- r4ss::SS_readforecast(file.path(scen_path_EM, "forecast.ss_new"),verbose = FALSE)
  if(length(id.F.fleet)>0) fore_new.EM$ForeCatch=fore_new.EM$ForeCatch%>%filter(!Fleet==id.F.fleet)
  fore_new.EM$MSY <- 2 # 1, FSPR; 2 FMSY; 3, FBtarget
  fore_new.EM$Forecast <- 2 # 1, use FSPR; 2, use FMSY; 3, F
  fore_new.EM$Btarget <- BTarget  #value of relative biomass aimed to be achieved during forecast
  fore_new.EM$SPRtarget <- SPR_Btgt
  fore_new.EM$ControlRuleMethod <- 0 # 0, don't use a ramp HCR at all; 1, catch as function of SSB; 2, F as function of SSB 
  fore_new.EM$BforconstantF <- BTarget
  fore_new.EM$BfornoF <- BLimit
  fore_new.EM$Nforecastyrs=1
  fore_new.EM=fore_new.OM[-match('ForeCatch',names(fore_new.EM))]
  r4ss::SS_writeforecast(fore_new.EM, scen_path_EM, verbose = FALSE,file = "forecast.ss_new", overwrite = TRUE)
  
  
  #6. Remove certain files to make sure they are not used.
  fn.file.remove(file=file.path(scen_path_EM, "forecast.ss_new"))
  fn.file.remove(file=file.path(scen_path_EM, "control.ss_new"))
  fn.file.remove(file=file.path(scen_path_EM, "data.ss_new"))
  fn.file.remove(file=file.path(scen_path_EM, "starter.ss_new"))

  
  #7. Rename some files (doesn't run otherwise)
   fn.rename(paste(scen_path_OM,'data_echo.ss_new',sep='/'), paste(scen_path_OM,'data.ss_new',sep='/'))
   fn.rename(paste(scen_path_EM,'data_echo.ss_new',sep='/'), paste(scen_path_EM,'data.ss_new',sep='/'))
   fn.rename(paste(scen_path_OM,'ss_win.log',sep='/'), paste(scen_path_OM,'ss.log',sep='/'))
   fn.rename(paste(scen_path_EM,'ss_win.log',sep='/'), paste(scen_path_EM,'ss.log',sep='/'))
}

fn.run.SS=function(where.inputs,where.exe,args=FALSE)
{
  wd_orig=getwd()
  setwd(where.inputs)
  if(!isFALSE(args)) system(paste(shQuote(where.exe),args))else
  {
    system(paste(shQuote(where.exe)))
  }
  setwd(wd_orig)
}

fn.run.SSSMSE=function(Scen,sp_path_OM,sp_path_EM,sp_path_out,Nsims,proj.yrs,
                       Neff.future,proj.yrs.with.obs,yrs.between.assess,cur.fleets,future.cv,specify.future.OM,
                       apply.future.meanbodywt)
{
  scen_path_OM=paste(sp_path_OM,Scen$Scenario,sep='/')
  scen_path_EM=paste(sp_path_EM,Scen$Scenario,sep='/')
  dat <- r4ss::SS_readdat(file.path(scen_path_OM, "data.dat"),verbose = FALSE)
  control <- r4ss::SS_readctl(file=file.path(scen_path_OM, "control.ctl"),
                              verbose = FALSE,
                              datlist=file.path(scen_path_OM, "data.ss_new"))
#  control <- r4ss::SS_readctl(file=file.path(paste(sp_path_assessment,Scen$Assessment.path,sep='/'), "control.ctl"),
#                              verbose = FALSE,
#                              datlist=file.path(paste(sp_path_assessment,Scen$Assessment.path,sep='/'), "data_echo.ss_new"))
  
  current.fleets=grep(paste(cur.fleets,collapse='|'),dat$fleetinfo$fleetname)
  datfile_path=file.path(scen_path_OM, "data.dat")
  
  #1. Create sampling scheme for projections
  myfleets=sort(unique(dat$catch$fleet))
  mysurveys=which(dat$fleetinfo$type==3)  
    #Catch
  future.ktch.yrs=(dat$endyr+1):(dat$endyr+proj.yrs)
  future.ktch.fleets=length(myfleets)
  future.ktch=data.frame(Yr=rep(future.ktch.yrs,future.ktch.fleets),
              Seas=1,
              FltSvy=rep(myfleets,each=proj.yrs),
              SE=0.01) #   filter(FltSvy%in%current.fleets)   #doesn't work if excluding fleets
    #CPUE
  future.CPUE=NA
  future.obs.yrs=sort(unique(future.ktch$Yr))   
  future.obs.yrs=future.obs.yrs[proj.yrs.with.obs]
  if('index'%in%colnames(dat$CPUE))
  {
    future.CPUE <- expand.grid(Yr = future.obs.yrs,  
                               Seas = unique(dat$CPUE$seas),
                               FltSvy = unique(dat$CPUE$index), 
                               SE=future.cv)%>%
                    filter(FltSvy%in%current.fleets)%>%
                    arrange(Yr,FltSvy)
  }
    
    #length comps
  future.lengcomp=NA
  future.Neff=round(mean(dat$lencomp$Nsamp))
  if(nrow(dat$lencomp)>0)
  {
    future.lengcomp <- expand.grid(Yr = future.obs.yrs,  
                                   Seas = unique(dat$lencomp$Seas),
                                   FltSvy = unique(dat$lencomp$FltSvy), 
                                   Sex = 1:2, 
                                   Part = unique(dat$lencomp$Part),
                                   Nsamp = Neff.future)%>%
                        filter(FltSvy%in%current.fleets)%>%
      arrange(Yr,FltSvy,Sex)
  }
  
    #mean body weight
  future.meanbodywt=NA
  if(apply.future.meanbodywt & !is.null(dat$meanbodywt))
  {
    future.meanbodywt <- expand.grid(Yr = future.obs.yrs,
                                     Seas = unique(dat$meanbodywt$Seas),
                                     FltSvy = unique(dat$meanbodywt$Fleet),
                                     Part = unique(dat$meanbodywt$Part),
                                     Type = unique(dat$meanbodywt$Type),
                                     Std_in = future.cv)%>%
                            filter(FltSvy%in%current.fleets)
  }  
  
  samp_struct_list=list(list(catch=future.ktch,
                              CPUE=future.CPUE,
                              lencomp=future.lengcomp,
                              agecomp=NA,
                              meanbodywt=future.meanbodywt,
                              MeanSize_at_Age_obs=NA))
  names(samp_struct_list)=Scen$Scenario
  
  
  #2. Create future OM
  first.future.yr=dat$endyr+1
  template_mod_change <- SSMSE::create_future_om_list(example_type = "model_change")  
  
   #Random fluctuations in rec devs
  rec_dev_specify <- template_mod_change[[1]]
  rec_dev_specify$pars <- "rec_devs"
  rec_dev_specify$scen <- c("replicate", "all") # use c("random", "all") if did not want to replicate the same recdevs across scenarios
  rec_dev_specify$input$first_yr_averaging <- dat$styr # use same sd as from the orig model.
  rec_dev_specify$input$last_yr_averaging <- dat$endyr  #missing: update accordingly with my years
  rec_dev_specify$input$last_yr_orig_val <- dat$endyr
  rec_dev_specify$input$first_yr_final_val <- first.future.yr
  rec_dev_specify$input$ts_param <- "sd"
  rec_dev_specify$input$value <- NA
  
    #Random fluctuations in gear selectivity
  sel.pars=rownames(control$size_selex_parms)
  mod_change_sel <- template_mod_change[[1]]
  mod_change_sel$pars <- sel.pars[grep('SizeSel_P_1_Southern.shark_2',sel.pars)]
  mod_change_sel$scen[2] <- "all"
  mod_change_sel$input$last_yr_orig_val <- dat$endyr      
  mod_change_sel$input$first_yr_final_val <- first.future.yr
  mod_change_sel$input$ts_param <- "sd"
  mod_change_sel$input$value <- 0.2
  
  future_om_list <- list(mod_change_sel, rec_dev_specify)
  future_OM=NULL
  if(specify.future.OM)future_OM=future_om_list

  
  #3. run SSMSE
  if(set.lib.path) .libPaths("C:/Users/myb/AppData/Local/R/win-library/4.4")
  out <- SSMSE::run_SSMSE(scen_name_vec = Scen$Scenario,      # name of the scenario
                          out_dir_scen_vec = sp_path_out,     # directory in which to run the scenario 
                          iter_vec = Nsims,
                          OM_name_vec = NULL,                 # specify directory instead
                          OM_in_dir_vec = scen_path_OM,       # OM directory
                          EM_name_vec = NULL,                 # specify directory instead
                          EM_in_dir_vec = scen_path_EM,       # EM directory
                          run_EM_last_yr = FALSE,
                          MS_vec = "EM",                      # The management strategy is specified in the EM
                          use_SS_boot_vec = TRUE,
                          nyrs_vec = proj.yrs,                # Years to project OM forward
                          nyrs_assess_vec = yrs.between.assess,
                          sample_struct_list = samp_struct_list,
                          future_om_list = future_OM,
                          verbose = TRUE,
                          seed = 666, # changing each time a chunk of runs is done will help ensure there is stochacisity 
                          run_parallel = TRUE,
                          n_cores = parallelly::availableCores()-1)
  return(out[[1]])
}

check_convergence_SSMSE <- function(summary, min_yr, max_yr)
{
  #require(dplyr) # note: not the best way to do this
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

# RatPack -----------------------------------------------------------------
SS_readRatPack.object=function (file) 
{
  dat <- readLines(file, warn = FALSE)
  if(any(grepl('::',dat))) dat <- dat[-grep('::',dat)]
  return(as.list(dat))
}   
fn.copy.RatPack.files=function(inpath,sp,outpath,file.name)
{
  inpath1=inpath
  if(grepl(paste(c('.proj','.OPD','HSE'),collapse='|'),file.name)) inpath1=paste0(inpath,'/inputs')
  mylist=SS_readRatPack.object(file = file.path(inpath1, file.name))
  
  #run.bat file
  if(file.name=="run.bat")
  {
    id=grep('ratpackMSE.exe',mylist)
    mylist[[id]]=str_replace(mylist[[id]],'Whiskery',str_remove(sp, " shark"))
  }

  outpath1=outpath
  if(grepl(paste(c('.proj','.OPD','HSE'),collapse='|'),file.name)) outpath1=paste(outpath1,'inputs',sep='/')
  file.name1=str_replace(file.name,'Whiskery',str_remove(sp, " shark"))
  outfile <- paste(outpath1, file.name1, sep = "/")
  if(file.exists(outfile)) file.remove(outfile)
  zz <- file(outfile, open = "at")
  sink(zz)
  for(x in 1:length(mylist)) writeLines(paste(mylist[[x]],collapse=' '),con = zz) 
  sink()
  close(zz)
}  
fun.populate.HSE=function(i,SS.path,scen,proj.yrs,yrs.between.assess)
{
  dat = r4ss::SS_readdat(file.path(SS.path,"data.dat"),verbose = FALSE)
  cntrl = r4ss::SS_readctl(file.path(SS.path,"control.ctl"),verbose = FALSE,datlist=file.path(SS.path, "data_echo.ss_new"))
  Report=SS_output(SS.path,covar=F,forecast=F,readwt=F,verbose = F, printstats=F)
  
  LISTA=list()
  id=which(dat$fleetinfo$fleetname=='F.series_Southern.shark_1')
  if(length(id>0))
  {
    dat$fleetinfo=dat$fleetinfo%>%filter(!fleetname=='F.series_Southern.shark_1')
    dat$catch=dat$catch%>%filter(!fleet==id)
    dat$CPUEinfo=dat$CPUEinfo%>%filter(!Fleet==id)
    dat$CPUE=dat$CPUE%>%filter(!index==id)
    dat$len_info=dat$len_info[-id,]
    cntrl$size_selex_types=cntrl$size_selex_types[-id,]
  }
  a=dat$catch%>%filter(year>0)%>%select(-seas,-catch_se)%>%spread(fleet,catch)
  a$Tot=rowSums(a[,-1],na.rm=T)
  LISTA$Number.of.future.years=proj.yrs
  LISTA$Frequency.of.RBC.update=yrs.between.assess
  LISTA$Final.year.TAC=round(mean(a$Tot[(nrow(a)-4):nrow(a)])) 
  LISTA$Recommended.catch.ranges=Target.commercial.catch%>%filter(Species==Keep.species[i])
  LISTA$SSsteepness=fn.get(parname='SR_BH_steep',from=cntrl$SR_parms)  
  LISTA$Mortality=c(round(mean(unlist(cntrl$natM['natM1',])),2),round(mean(unlist(cntrl$natM['natM2',])),2))
  LISTA$Growth.age.L2=dat$Nages
  LISTA$Maturity=c(fn.get(parname='Mat50%_Fem_GP_1',from=cntrl$MG_parms), fn.get(parname='Mat_slope_Fem_GP_1',from=cntrl$MG_parms))
  LISTA$fleets= dat$fleetinfo
  n.fleets=nrow(dat$fleetinfo%>%filter(!grepl('Survey',fleetname))) 
  LISTA$Number.of.fleets=n.fleets
  n.surveys=nrow(dat$fleetinfo%>%filter(grepl('Survey',fleetname)))
  LISTA$Number.of.surveys=n.surveys 
  LISTA$Selectivity.pattern=cntrl$size_selex_types$Pattern
  LISTA$Selectivity.pattern=ifelse(LISTA$Selectivity.pattern==15,5,LISTA$Selectivity.pattern)   #doesn't work with 15    
  LISTA$Mirrored.fleet=cntrl$size_selex_types$Special
  LISTA$Selectivity.pars=fn.right.format(x=cntrl$size_selex_parms,var='INIT')%>%dplyr::select(-fleet)
  LISTA$Selectivity.phase=fn.right.format(x=cntrl$size_selex_parms,var='PHASE')%>%dplyr::select(-fleet)
  LISTA$Retention.inflection_Retention.slope=rep(0,n.fleets)
  LISTA$variance.adjustment.CPUE_variance.adjustment.discard=rep(0,n.fleets+n.surveys) 
  LISTA$Variance.adjustment.length_variance.adjustment.age=rep(1,n.fleets+n.surveys) 
  LISTA$Ageing.error=rep(0,dat$Nages+1)
  LISTA$SSR0init=exp(fn.get(parname='SR_LN(R0)',from=cntrl$SR_parms))
  #LISTA$SSR0init=exp(Report$estimated_non_dev_parameters[match('SR_LN(R0)',rownames(Report$estimated_non_dev_parameters)),]$Value)
  #reference points
  dum=Report$derived_quants
  SPR_Btgt=round(dum[grep("SPR_Btgt",dum$Label),'Value'],2)  
  SSB_Virgin=dum[grep("SSB_Virgin",dum$Label),'Value']
  SSB_MSY=dum[grep("SSB_MSY",dum$Label),'Value']
  SSB_MSY.over.SSB_Virgin=SSB_MSY/SSB_Virgin
  LISTA$HCR.target=round(scen$Target*SSB_MSY.over.SSB_Virgin,2)
  LISTA$HCR.break=round(scen$Threshold*SSB_MSY.over.SSB_Virgin,2)
  Min.bio=0.2
  if(scen$Limit==0.4) Min.bio=0.15
  if(scen$Limit==0.6) Min.bio=0.22
  LISTA$HCR.limit=round(max(Min.bio,scen$Limit*SSB_MSY.over.SSB_Virgin),2)  
  LISTA$allocation.of.fleets.to.regions=as.data.frame(matrix(rep(1,n.fleets+n.surveys),ncol=1))  
  return(LISTA)
}
fun.populate.OPD=function(i,SS.path,Scen,Neff.future,Nregions)
{
  dat = r4ss::SS_readdat(file.path(SS.path,"data.dat"),verbose = FALSE)
  cntrl = r4ss::SS_readctl(file.path(SS.path,"control.ctl"),verbose = FALSE, datlist=file.path(SS.path, "data_echo.ss_new"))
  Report=SS_output(SS.path,covar=F,forecast=F,readwt=F,verbose = F, printstats=F)
  
  #Update OM when Scenario info for scenarios not tested during the Assessment 
  if(!is.na(Scen$Difference))
  {
    if(Scen$Difference=='lower.steepness')    
    {
      cntrl$SR_parms[match('SR_BH_steep',row.names(cntrl$SR_parms)),c('LO','INIT')]=c(0.25,Scen$Difference.value)
    }
    if(Scen$Difference=='M.contstant')
    {
      nn=ncol(cntrl$natM)
      cntrl$natM[1,]=rep(Scen$Difference.value,nn)
      cntrl$natM[2,]=rep(Scen$Difference.value,nn)
    }
    
    if(Scen$Difference=='IUU') 
    {
      id=match('Other',dat$fleetinfo$fleetname)
      a=dat$catch%>%filter(fleet==id)
      dat$catch=dat$catch%>%filter(!fleet==id)
      a=left_join(a,Indoktch%>%
                    dplyr::select(Year,catch)%>%rename(catch1=catch),
                  by=c('year'='Year'))%>%
        mutate(catch1=ifelse(is.na(catch1),0,catch1),
               catch=catch-catch1,
               catch=catch+(catch1*Scen$Difference.value))%>%
        dplyr::select(-catch1)
      dat$catch=rbind(a,dat$catch)%>%arrange(fleet,year,seas) 
    }
  }
  
  LISTA=list()
  
  LISTA$Species=str_remove(Keep.species[i],' shark')
  id=which(dat$fleetinfo$fleetname=='F.series_Southern.shark_1')
  if(length(id>0))
  {
    dat$fleetinfo=dat$fleetinfo%>%filter(!fleetname=='F.series_Southern.shark_1')
    dat$catch=dat$catch%>%filter(!fleet==id)
    dat$CPUEinfo=dat$CPUEinfo%>%filter(!Fleet==id)
    dat$CPUE=dat$CPUE%>%filter(!index==id)
    dat$len_info=dat$len_info[-id,]
  }
  Fleet.info=dat$fleetinfo
  LISTA$fleets= Fleet.info
  n.fleets=nrow(Fleet.info%>%filter(!grepl('Survey',fleetname)))
  LISTA$Number.of.metiers=n.fleets
  LISTA$Number.of.fleets=n.fleets  
  n.surveys=nrow(Fleet.info%>%filter(grepl('Survey',fleetname)))
  LISTA$Number.of.surveys=n.surveys 
  LISTA$Maximum.age=dat$Nages
  a=dat$catch%>%filter(year>0)%>%select(-seas,-catch_se)%>%spread(fleet,catch)
  colnames(a)[-match('year',colnames(a))]=paste('fleet',colnames(a)[-match('year',colnames(a))])
  LISTA$First.and.last.year.of.historic.data=c(min(a$year),max(a$year))
  LISTA$Number.of.length.bins=(length(dat$lbin_vector_pop)-1)
  LISTA$lower.length.of.length.bins=dat$lbin_vector_pop
  
  LISTA$natural.mortality_by.stock.sex.age=c(1,2,length(seq(0,(dat$Nages))))
  LISTA$natural.mortality_age.groups=seq(0,(dat$Nages))
  LISTA$natural.mortality_female=round(unname(unlist(cntrl$natM['natM1',])),3)
  LISTA$natural.mortality_male=round(unname(unlist(cntrl$natM['natM2',])),3)
  
  LISTA$Growth.parameters_max.age.stock=rep(dat$Nages,2)
  LISTA$Growth.parameters_L_at_Amin=c(fn.get(parname='L_at_Amin_Fem_GP_1',from=cntrl$MG_parms),fn.get(parname='L_at_Amin_Mal_GP_1',from=cntrl$MG_parms))
  LISTA$Growth.parameters_L_at_Amax=c(fn.get(parname='L_at_Amax_Fem_GP_1',from=cntrl$MG_parms),fn.get(parname='L_at_Amax_Mal_GP_1',from=cntrl$MG_parms))
  LISTA$Growth.parameters_K=c(fn.get(parname='VonBert_K_Fem_GP_1',from=cntrl$MG_parms),
                              fn.get(parname='VonBert_K_Mal_GP_1',from=cntrl$MG_parms))
  LISTA$Growth.parameters_CV.age.0=c(fn.get(parname='CV_young_Fem_GP_1',from=cntrl$MG_parms),
                                     fn.get(parname='CV_young_Mal_GP_1',from=cntrl$MG_parms))
  LISTA$Length.weight_a=c(fn.get(parname='Wtlen_1_Fem_GP_1',from=cntrl$MG_parms),
                          fn.get(parname='Wtlen_1_Mal_GP_1',from=cntrl$MG_parms))
  LISTA$Length.weight_b=c(fn.get(parname='Wtlen_2_Fem_GP_1',from=cntrl$MG_parms),
                          fn.get(parname='Wtlen_2_Mal_GP_1',from=cntrl$MG_parms))
  LISTA$maturity_a50=fn.get(parname='Mat50%_Fem_GP_1',from=cntrl$MG_parms)
  LISTA$maturity_slope=fn.get(parname='Mat_slope_Fem_GP_1',from=cntrl$MG_parms)
  
  LISTA$Recruitment_steepness=fn.get(parname='SR_BH_steep',from=cntrl$SR_parms)
  LISTA$Recruitment_stdev.recruitment=round(fn.get(parname='SR_sigmaR',from=cntrl$SR_parms),2)
  LISTA$Recruitment_rec.devs.min.year=dat$styr
  LISTA$Recruitment_rec.devs.max.year_null=dat$styr
  LISTA$Recruitment_rec.devs.max.year=max(Report$recruitpars%>%filter(type=='Main_RecrDev')%>%pull(Yr))
  LISTA$rec.devs.over.time_null=0
  LISTA$rec.devs.over.time=round(Report$recruitpars%>%filter(type=='Main_RecrDev')%>%pull(Value),3)
  
  LISTA$Recruitment_unfished.recruitment_init=exp(fn.get(parname='SR_LN(R0)',from=cntrl$SR_parms))
  LISTA$Recruitment_unfished.recruitment_estimated=exp(Report$estimated_non_dev_parameters[match('SR_LN(R0)',rownames(Report$estimated_non_dev_parameters)),]$Value)
  
  LISTA$Catches.by.fleet.and.years.and.regions=c(ncol(a)-1,nrow(a),Nregions)
  b=a%>%gather(Fleet,Catch,-year)
  LISTA$Catch=lapply(split( b , f = b$Fleet ),function(x) x=x%>%dplyr::select(-Fleet)%>%mutate_if(is.numeric, round, 2))
  
  Sels=Report$sizeselex%>%
    filter(Factor=='Lsel' & Yr==dat$endyr & Sex==1)%>%
    distinct(Fleet,.keep_all = T)%>%
    arrange(Fleet)
  if(length(id)>0) Sels=Sels%>%filter(!Fleet==id)
  Sels=Sels%>%dplyr::select(-c(Factor,Fleet,Yr,Sex,Label))
  LISTA$Number.of.extra.selectivity.time.blocks.by.fleet=rep(0,n.fleets+n.surveys)
   if(!is.na(Scen$Difference) & Scen$Difference=='NSF.logis.sel')        
  {
    id=match('Northern.shark',row.names(cntrl$size_selex_types))
    L50=cntrl$MG_parms[match('Mat50%_Fem_GP_1',row.names(cntrl$MG_parms)),'INIT']
    Len.cls=as.numeric(colnames(Sels))
    Sels[id,]=1/(1+exp(-log(19)*(Len.cls-L50)/10))
  }
  
  LISTA$Initial.selectivity.at.length.by.fleet=round(Sels,3)  
  LISTA$stdev.for.selectivity_length.bin.correlation=rep(0,nrow(Sels))  
  Sels.ret=Sels
  Sels.ret[,]=1
  LISTA$Initial.retention.at.length.by.fleet=Sels.ret
  LISTA$type.of.discards=rep(1,n.fleets)
  LISTA$Initial.gammas.for.retention=as.data.frame(matrix(1,ncol=3,nrow=n.fleets))
  
  CPUE=dat$CPUE
  cpue.future.flag=rep(0,nrow(Fleet.info))
  cpue.future.flag[match('Southern.shark_2',Fleet.info$fleetname)]=1
  if(n.surveys>0) cpue.future.flag[match('Survey',Fleet.info$fleetname)]=1
  LISTA$CPUE_future.flag=cpue.future.flag
  cv=cpue.future.flag
  cv[cv>0]=0.15
  LISTA$CPUE_cv=cv
  hist.cv=cpue.future.flag
  hist.cv[unique(dat$CPUE$index)]=0.15 
  LISTA$CPUE_hist.cv=hist.cv
  avrg.q.est=exp(Report$parameters[grep('LnQ_base',rownames(Report$parameters)),c('Label','Value')]%>%pull(Value)) 
  avrg.q=hist.cv
  if(length(avrg.q.est)==0) avrg.q.est=NA
  avrg.q[avrg.q>0]=avrg.q.est
  LISTA$CPUE_average.catchability_outputs.NA.if.not.estimated=avrg.q
  LISTA$CPUE_annual.increase.q=rep(0,length(hist.cv))
  cpue.power=hist.cv
  cpue.power[cpue.power>0]=1
  LISTA$CPUE_power=cpue.power
  LISTA$CPUE_variance=rep(0,length(hist.cv))
  LISTA$CPUE_correlation=rep(1,length(hist.cv))
  LISTA$CPUE_history.flag=c(Nregions,nrow(a),n.fleets+n.surveys)
  CPUE.Region.1=a
  if(n.surveys>0)
  {
    dis.surveys=Fleet.info%>%filter(grepl('Survey',fleetname))%>%pull(fleetname)
    dd=data.frame(CPUE.Region.1[,1:n.surveys])
    colnames(dd)=paste('fleet',seq(n.fleets+1,n.fleets+length(dis.surveys)))
    CPUE.Region.1=cbind(CPUE.Region.1,dd)  
  }
  CPUE.Region.1[,-1]=0
  fleets.cpue=sort(unique(CPUE$index))
  if(!is.null(fleets.cpue))
  {
    for(pp in 1:length(fleets.cpue))
    {
      CPUE.Region.1[match(CPUE%>%filter(index==fleets.cpue[pp])%>%pull(year),CPUE.Region.1$year),
                    match(paste("fleet",fleets.cpue[pp]),colnames(CPUE.Region.1))]=1
    }
  }
  LISTA$CPUE_Region_1=CPUE.Region.1  
 
  LISTA$Discards_future.flag_cv=rep(0,n.fleets)  
  LISTA$Discards_history.flag=c(nrow(a),n.fleets)
  Discards=CPUE.Region.1
  if(n.surveys>0) Discards=Discards%>%select(colnames(a))
  Discards[,-1]=0
  LISTA$Discards=Discards
  
  Length.comps_future=as.data.frame(matrix(0,ncol=3,nrow=nrow(Sels)))
  Length.comps_future[match('Southern.shark_2',Fleet.info$fleetname),1]=1
  if(n.surveys>0) Length.comps_future[match('Survey',Fleet.info$fleetname),1]=1
  LISTA$Length.comps_future.flag=Length.comps_future
  Length.comps_historical.samp=as.data.frame(matrix(0,ncol=3,nrow=nrow(Sels)))
  Average.samp.size=dat$lencomp%>%group_by(FltSvy)%>%summarise(Nsamp=mean(Nsamp))%>%arrange(FltSvy)
  Length.comps_historical.samp[Average.samp.size$FltSvy,1]=round(Average.samp.size$Nsamp)
  LISTA$Length.comps_historical.sample.size=Length.comps_historical.samp
  Length.comps_projected.samp=Length.comps_historical.samp
  Length.comps_projected.samp[-which(Fleet.info$fleetname%in%c('Southern.shark_2','Survey')),]=0
  Length.comps_projected.samp[Length.comps_projected.samp>0]=Neff.future
  LISTA$Length.comps_projected.sample.size=Length.comps_projected.samp  
  LISTA$Length.comps_retained=c(nrow(a),n.fleets+n.surveys)
  Length.comps.ret_mat=as.data.frame(matrix(0,nrow=nrow(a),ncol=nrow(dat$len_info)+1))
  colnames(Length.comps.ret_mat)=names(CPUE.Region.1)
  Length.comps.ret_mat$year=a$year
  dis.flits=colnames(Length.comps.ret_mat)[-1]
  for(pp in 1:length(dis.flits))
  {
    dd=dat$lencomp%>%filter(FltSvy==str_remove(dis.flits[pp], "fleet "))
    if(nrow(dd)>0)
    {
      Length.comps.ret_mat[match(sort(unique(dd$Yr)),Length.comps.ret_mat$year),dis.flits[pp]]=1
    }
    
  }
  LISTA$Length.comps_retained_matrix=Length.comps.ret_mat
  LISTA$Length.comps_discarded=c(nrow(a),n.fleets+n.surveys)
  Length.comps.disc_mat=Length.comps.ret_mat
  Length.comps.disc_mat[,-1]=0
  LISTA$Length.comps_discarded_matrix=Length.comps.disc_mat
  
  LISTA$Age.comps_future.flag=as.data.frame(matrix(0,ncol=3,nrow=nrow(Sels)))
  LISTA$Age.comps_historical.sample.size=LISTA$Age.comps_future.flag
  LISTA$Age.comps_projected.sample.size=LISTA$Age.comps_future.flag
  LISTA$Age.comps_retained=c(nrow(a),n.fleets+n.surveys)
  LISTA$Age.comps_retained_matrix=Length.comps.disc_mat

  LISTA$Conditional.Age.length_future.flag=as.data.frame(matrix(0,ncol=3,nrow=nrow(Sels)))
  LISTA$Conditional.Age.length_historical.sample.size=LISTA$Conditional.Age.length_future.flag
  LISTA$Conditional.Age.length_projected.sample.size=LISTA$Conditional.Age.length_future.flag
  LISTA$Conditional.Age.length_retained=c(nrow(a),n.fleets+n.surveys)
  LISTA$Conditional.Age.length_retained_matrix=Length.comps.disc_mat

  return(LISTA)
}

fn.run.RatPack.exe=function(where.exe,exe.name)
{
  setwd(where.exe)
  system(paste(shQuote(exe.name)))
}
fn.re.run.SS=function(WD,prev.ass)
{
  if(!file.exists(paste0(WD,'/ss.par')))
  {
    invisible(file.copy(from=paste0(prev.ass,'/ss.par'),to=paste0(WD,'/ss.par')))
  }
  fn.run.SS(where.inputs=WD,
            where.exe=handl_OneDrive('SS3/ss_win.exe'),
            args="-nohess")
}
fn.get.RatPack.results=function(spi,PATH,last_yr)
{
  res_1 <- combine_results(proj=read_proj(proj_file_name=paste0(spi,'_test.proj'),
                                          file_path=PATH,
                                          in_dir="Inputs"),
                           file_path=PATH)
  res_1[[1]] <- filter(res_1[[1]], Year<=last_yr) # Remove the final year from the results from the new version of ratpack (it adds
  # an extra year with no assessment).
  
  #separate the data for the simulation period
  dd1 <- filter(res[[1]][[1]], Period == "Sim")    
  
  d <- res[[1]][[1]] %>%
    filter(Year <=last.yr.EM) %>%
    rename(estDepletion=estDepletion_PreviousYr) %>%
    dplyr::select(any_of(data_fields))%>%
    mutate(Scenario=Scenarios$Scenario[s],
           estSSB0_1 = lead(estSSB0), estSSBcurrent_1 = lead(estSSBcurrent), #re-arrange years to fix misspecification
           estDepletion_1 = lead(estDepletion))
  
  # remove EM results when assessment does not converge
  d <- d %>%
    mutate(estSSB0_1 = ifelse(AssessFail == 1, as.integer(NA), estSSB0_1),
           estSSBcurrent_1 = ifelse(AssessFail == 1, as.integer(NA), estSSBcurrent_1),
           estDepletion_1 = ifelse(AssessFail == 1, as.integer(NA), estDepletion_1))
  
  #change -999 values to NAs
  d <- mutate(d, across(where(is.numeric), ~na_if(., -999)))
  
  ## select the simulation period, remove the last year because est depletion -1 year
  d_current <- filter(d, Period == "Sim", Year<=last_yr-1)
  
  ## relative error calculation
  d_rel_err <- d_current
  d_rel_err$SSB0_rel_error <- (d_rel_err$estSSB0 - d_rel_err$SSB0) / d_rel_err$SSB0
  d_rel_err$SSB_rel_error <- (d_rel_err$estSSBcurrent_1 - d_rel_err$SSBcurrent) / d_rel_err$SSBcurrent
  d_rel_err$Depletion_rel_error <- (d_rel_err$estDepletion_1 - d_rel_err$Depletion) / d_rel_err$Depletion
  
  return(list(d=d, dd1=dd1, d_rel_err=d_rel_err, d_current=d_current))
}


# Report outputs -----------------------------------------------------------------
fn.percentiles=function(d,grouping,var)
{
  return(d%>%
           group_by_at(grouping)%>%
           summarise(per_5=quantile(!!as.name(var), probs=0.05, na.rm=TRUE),
                     per_95=quantile(!!as.name(var), probs=0.95, na.rm=TRUE),
                     
                     
                     ymin=quantile(!!as.name(var), probs=0.10, na.rm=TRUE),
                     lower=quantile(!!as.name(var), probs=0.25, na.rm=TRUE),
                     middle=quantile(!!as.name(var), probs=0.5, na.rm=TRUE),
                     upper=quantile(!!as.name(var), probs=0.75, na.rm=TRUE),
                     ymax=quantile(!!as.name(var), probs=0.9, na.rm=TRUE)))
}

fn.perf.ind.time.series=function(df,YLAB='Indicator distribution',Title)
{
  df%>%
    mutate(Scenario=factor(Scenario,levels=paste0('S',1:length(unique(Scenario)))),
           Perf.ind=ifelse(Perf.ind=='F.over.FMSY','F/FMSY',
                    ifelse(Perf.ind=='B.over.BMSY','B/BMSY',
                    ifelse(Perf.ind=='Catch','Total catch',
                    Perf.ind))),
           Perf.ind=factor(Perf.ind,levels=Perform.ind.levels),
           iteration=as.character(iteration))%>%
    filter(!is.na(Value))%>%
    group_by(year,Perf.ind,Scenario)%>%mutate(Median=median(Value,na.rm=T))%>%
    ggplot(aes(year,Value,color=iteration))+    
    geom_line(alpha=0.4)+
    geom_line(aes(year,Median),color='chocolate3',size=.95)+
    ggh4x::facet_grid2(Perf.ind~Scenario,scales = TRUE, independent = "x",drop=TRUE)+
    ylab(YLAB)+xlab('')+
    my_theme()%+replace% 
    theme(panel.grid.minor = element_blank(),
          strip.text.x = element_text(size=11),
          axis.text = element_text(size=8),
          plot.title = element_text(size=15,hjust=0),
          axis.title = element_text(size=14),
          legend.spacing.x = unit(0.05, 'cm'),
          legend.text = element_text(size=8),
          legend.key.width = unit(0.5, "cm"),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.box.margin=margin(-25,0,-10,-10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ggtitle(Title)+
    scale_color_manual(values=rep("grey70", length(unique(df$iteration))))+
    expand_limits(y=0)
}

fn.perf.ind.dist=function(df,YLAB='Density distribution',Title)
{
  df%>%
    mutate(Scenario=factor(Scenario,levels=paste0('S',1:length(unique(Scenario)))))%>%
    ggplot(aes(Value,fill=Scenario))+
    geom_density(adjust=2,alpha=0.4)+
    facet_wrap(~Perf.ind,ncol=1,scales = 'free')+
    ylab(YLAB)+xlab('')+
    my_theme()%+replace% 
    theme(panel.grid.minor = element_blank(),
          strip.text.x = element_text(size=11),
          axis.text = element_text(size=9),
          plot.title = element_text(size=15,hjust=0),
          axis.title = element_text(size=14),
          legend.spacing.x = unit(0.05, 'cm'),
          legend.text = element_text(size=8),
          legend.key.width = unit(0.5, "cm"),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.box.margin=margin(-25,0,-10,-10))+
    ggtitle(Title)
}

fn.perf.ind.boxplot=function(df,YLAB='Indicator value',Title)
{
  df%>%
    mutate(Scenario=factor(Scenario,levels=paste0('S',1:length(unique(Scenario)))))%>%
    ggviolin(x = "Scenario", y = "Value", fill = "Scenario",
             add = c("jitter","boxplot"), add.params = list(fill = "white",alpha=0.5))+
    facet_wrap(~Perf.ind,ncol=1,scales = 'free_y')+
    ylab(YLAB)+xlab('')+
    my_theme()%+replace% 
    theme(panel.grid.minor = element_blank(),
          strip.text.x = element_text(size=11),
          axis.text = element_text(size=9),
          axis.title = element_text(size=14),
          plot.title = element_text(size=15,hjust=0),
          legend.position = 'none',
          legend.margin=margin(0,0,0,0))+
    ggtitle(Title)
}

kobePlot <- function(f.traj,b.traj,Years,Titl,Probs=NULL,pt.size,txt.col,line.col,YrSize,
                     ALPHA=1,show.marginal.density=FALSE,show.probs.contour.legend=FALSE,
                     YLAB=expression(F/~F[MSY]),XLAB=expression(B/~B[MSY]), Leg.txt.size=14)
{
  dta=data.frame(x=b.traj,
                 y=f.traj,
                 yr=Years)%>%
    arrange(yr)
  Mx.F=max(2,max(dta$y,na.rm=T))
  Mx.B=max(2,max(dta$x,na.rm=T))
  Mn.F=0 
  Mn.B=0
  
  kobe <-dta%>%
    ggplot(aes(x, y))+
    geom_rect(xmin = 1, xmax = Mx.B, ymin = Mn.F, ymax = 1, fill = RiskColors['Low'], alpha = ALPHA) +
    geom_rect(xmin = Mn.B, xmax = 1, ymin = 1, ymax = Mx.F, fill = RiskColors['Severe'], alpha = ALPHA) +
    geom_rect(xmin = 1, xmax = Mx.B, ymin = 1, ymax = Mx.F, fill = RiskColors['High'], alpha = ALPHA) +
    geom_rect(xmin = Mn.B, xmax = 1, ymin = Mn.F, ymax = 1, fill = RiskColors['Medium'], alpha = ALPHA)
  if(!is.null(Probs))
  {
    Probs=Probs%>%filter(x>=0 & y>=0)
    kernelF <- gplots::ci2d(Probs$x, Probs$y, nbins = 50, factor = 1.5, 
                            ci.levels = c(0.5, 0.8, 0.95), show = "none")
    KernelD=rbind(kernelF$contours$"0.95"%>%mutate(CI='1',col='cadetblue4'), #'grey30'
                  kernelF$contours$"0.8"%>%mutate(CI='2',col='cadetblue3'), #'grey60'
                  kernelF$contours$"0.5"%>%mutate(CI='3',col='cadetblue1')) #'grey85'
    kernels=KernelD%>%distinct(CI,col)%>%pull(col)
    names(kernels)=KernelD%>%distinct(CI,col)%>%pull(CI)
    
    Pr.d=data.frame(
      Prob=c(sum(ifelse(Probs$x >= 1 & Probs$y <= 1, 1, 0))/length(Probs$x)*100,
             sum(ifelse(Probs$x < 1 & Probs$y <= 1, 1, 0))/length(Probs$x)*100,
             sum(ifelse(Probs$x >= 1 & Probs$y > 1, 1, 0))/length(Probs$x)*100,
             sum(ifelse(Probs$x < 1 & Probs$y > 1, 1, 0))/length(Probs$x) * 100),
      col=RiskColors[c('Low','Medium','High','Severe')],
      x=rep(-10,4),  #dummy
      y=rep(-10,4))
    pr.ds=Pr.d%>%pull(col)
    Nms.probs=round(Pr.d%>%pull(Prob),1)
    Nms.probs=ifelse(Nms.probs>0 & Nms.probs<0.1,'<0.1',Nms.probs)
    names(pr.ds)=paste(Nms.probs,'%',sep='')
    #pr.ds=pr.ds[which(!names(pr.ds)=='0%')]
    kobe <-kobe +
      geom_polygon(data=KernelD,aes(x, y,fill=CI),size=1.25,alpha=0.5)+
      scale_fill_manual(labels=c("95%","80%","50%"),values = kernels)+
      geom_point(data=Pr.d,aes(x, y,color=col),alpha = 1,size=5)+
      scale_color_manual(labels=names(pr.ds),values =Pr.d$col)+
      labs(CI="", col=dta[nrow(dta),'yr'])
    if(!show.probs.contour.legend) kobe=kobe+guides(color=guide_legend("CI"), fill = "none")
  }
  kobe <-kobe + 
    coord_cartesian(xlim = c(Mn.B,Mx.B), ylim=c(Mn.F,Mx.F))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))+
    geom_path(linetype = 2, size = 0.5,color=line.col)+
    geom_point(size=pt.size,color=line.col,alpha=seq(0.2,1,length.out=length(dta$yr)))+
   # geom_point(aes(x=dta[1,'x'],y=dta[1,'y']),size=4,shape=22,fill='white',alpha=.3)+
  #  geom_point(aes(x=dta[nrow(dta),'x'],y=dta[nrow(dta),'y']),size=4,shape=25,fill='white',alpha=.3)+      
   # geom_text_repel(data=dta[1,],aes(x=x,y=y,label=yr),size=YrSize,color=txt.col)+
    geom_text_repel(data=dta[nrow(dta),],aes(x=x,y=y,label=yr),size=YrSize,color=txt.col)+
    xlab(XLAB)+
    ylab(YLAB)+
    theme_bw()%+replace% 
    theme(panel.grid.minor = element_blank(),
          axis.text = element_text(size=9),
          axis.title = element_text(size=20),
          plot.title = element_text(size=20,hjust=0),
          legend.text = element_text(size=Leg.txt.size),
          legend.position = 'bottom',
          legend.title = element_text(size=17),
          legend.spacing.x = unit(0.05, 'cm'),
          legend.margin=margin(-10,0,-10,0),
          legend.box.margin=margin(-20,10,10,-10))
  if(!is.null(Probs)) kobe=kobe+theme(legend.title = element_blank(),legend.text.align = 1)
  
  if(show.marginal.density)
  {
    xplot <- ggdensity(data=Probs, x='x', fill = "steelblue",color = 'transparent') +
      xlim(Mn.B,Mx.B)+
      theme_void()+
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
      labs(title = Titl)
    yplot <- ggdensity(data=Probs, x='y', fill = "steelblue",color = 'transparent') + 
      rotate()+
      xlim(Mn.F,Mx.F)+ 
      theme_void()+
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
    kobe=plot_grid(xplot, NULL, kobe, yplot, ncol = 2, align = "hv",
                   rel_widths = c(4, 1), rel_heights = c(1, 4))
    
  }
  if(!show.marginal.density) kobe=kobe+labs(title = Titl)
  
  
  return(list(kobe=kobe,prob.green=Pr.d[rownames(Pr.d)=='Low','Prob'] ))
}

fn.polar.plot=function(data,Title='',Subtitle='',Caption='')
{
  p=data%>%
    mutate(Scenario=factor(Scenario,levels=paste0('S',1:length(unique(Scenario)))))%>%
    ggplot(aes(x = Scenario, y = value,fill = Scenario)) +
    geom_col(width = 1, color = "white") + 
    facet_wrap(~Indicator)+
    coord_curvedpolar()+
    labs(x = "", y = "",  title = Title, subtitle = Subtitle, caption = Caption) + 
    my_theme()%+replace% 
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.subtitle = element_text(size = 12),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size=16),
          axis.text = element_text(size=12),
          axis.title = element_text(size=16),
          plot.title = element_text(size=20,hjust=0),
          legend.text = element_text(size=15),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,0,-10,-10))
  return(p)
}

fn.lolipot.plot=function(data,Title='')
{
  p=data%>%
    group_by(Indicator)%>%
    mutate(z=(value.orig-mean(value.orig))/sd(value.orig),
           z.group=factor(ifelse((z < 0 & Indicator%in%Perf.ind_pos)|
                                   (z > 0 & Indicator%in%Perf.ind_neg), "low",
                                 "high")))%>%
    ungroup()%>%
    mutate(Scenario=factor(Scenario,levels=paste0('S',1:length(unique(Scenario)))))%>%
    ggdotchart(x = "Scenario", y = "z", color = "z.group",  sorting = "none",   
               palette = c("chartreuse4", "brown"),
               add = "segments",  add.params = list(color = "z.group", size = 1), 
               group = "z.group",  dot.size = 3, rotate = TRUE)+               
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray")+
    facet_wrap(~Indicator,ncol=1)+    
    labs(x = "", y = "",  title = Title) + 
    my_theme()%+replace% 
    theme(legend.title = element_blank(),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size=15),
          axis.text = element_text(size=10),
          axis.title = element_text(size=16),
          plot.title = element_text(size=18,hjust=0),
          legend.text = element_text(size=15),plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,0,-10,-10))
  return(p)
}

fn.quilt.plot=function(df,clr.scale,col.breaks,Titl,Delta)
{
  color_df=df
  for(x in 1:ncol(color_df)) color_df[,x]=clr.scale(col.breaks)[as.numeric(cut(color_df[,x],breaks = col.breaks))]
  
  df=t(df)
  color_df=t(color_df)
  
  my_table_theme <- ttheme_default(core=list(fg_params=list(col='grey20'),bg_params = list(fill = unlist(color_df), col=NA)))
  G <- ggplot() +
    geom_point(aes(x=0:1,y=0:1),color='transparent')+
    theme_void() +
    annotation_custom(gridExtra::tableGrob(df, theme = my_table_theme),
                      xmin = Delta,#-Inf,
                      xmax = 1, #Inf,
                      ymin = 0, #-Inf,
                      ymax = 1)+ #Inf)
    ggtitle(Titl)+
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  return(G)
}
