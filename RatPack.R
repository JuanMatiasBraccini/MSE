library(r4ss)
library(tictoc)
library(tidyverse)
#library(Hmisc)  stuffs up group_by

Usr=Sys.getenv("USERNAME")
handl_OneDrive=function(x)paste('C:/Users',Usr,'OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')
hndl='Analyses/MSE/RatPack/'
hndl.out=paste0(hndl,'outputs')

theme_set(theme_light() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(colour = 'black'),
                  text = element_text(family = "Calibri", size = 12)))


# Run RatPack-------------------------------------------------------------------------
fn.run=function(where.exe,exe.name)
{
  setwd(where.exe)
  system(paste(shQuote(exe.name)))
}
tic()
fn.run(where.exe=handl_OneDrive(paste0(hndl)),exe.name='run.bat')
toc()

# Bring in OM and EM outputs-------------------------------------------------------------------------
OMOut <- read.table(handl_OneDrive(paste0(hndl,'Results/Whiskery_results_1.out')), 
                    skip=1, header=TRUE, fill=TRUE)

EMOut <- read.table(handl_OneDrive(paste0(hndl,'Debug/Whiskerytrace_plot.dat')), 
                    header=TRUE, fill=TRUE)

#Calculate percentiles
fn.percentiles=function(d,grouping,var)
{
  return(d%>%
           group_by_at(grouping)%>%
           summarise(ymin=quantile(!!as.name(var), probs=0.10, na.rm=TRUE),
                     lower=quantile(!!as.name(var), probs=0.25, na.rm=TRUE),
                     middle=quantile(!!as.name(var), probs=0.5, na.rm=TRUE),
                     upper=quantile(!!as.name(var), probs=0.75, na.rm=TRUE),
                     ymax=quantile(!!as.name(var), probs=0.9, na.rm=TRUE)))
}
OMSSBquant=fn.percentiles(d=OMOut,grouping='Year',var='SSBcurrent')
EMSSBquant=fn.percentiles(d=EMOut %>%
                            filter(RBCyear==2022 | RBCyear==2025) %>%
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


ggsave(handl_OneDrive(paste0(hndl.out,'/length_cpue_only years with catch_Neff50_CV_0.05_95CI_200 sims.tiff')),width = 6,height = 6,compression = "lzw")

# Bring in SS Report file-------------------------------------------------------------------------
N.sims=2
Assess.years=c(2022)

Report.list=vector('list',length=length(Assess.years))
names(Report.list)=paste0('Assess_',Assess.years)

for(i in 1:length(Report.list))
{
  dumi=vector('list',length=N.sims)
  names(dumi)=paste0('Sim_',1:N.sims)
  for(d in 1:N.sims)
  {
    x=paste0('Whiskery_sim_',d,'_year_',Assess.years[i])
    dumi[[d]]=SS_output(dir=handl_OneDrive(paste0(hndl,'Stock_Synthesis/',x)))
  }
  Report.list[[i]]=dumi
}
#SS_plots(Report,plot=1:4,  png=T)
SS_plots(Report.list$Assess_2022$Sim_1,plot=c(1:6,8:20),  png=T)



# Bring in contents from Results folder-------------------------------------------------------------------------
Results=data.table::fread(handl_OneDrive(paste0(hndl,'Results/Whiskery_results_1.out')),
                    skip=9)
Results=Results%>%
  mutate(Sim=factor(Sim,levels=1:max(unique(Results$Sim))),
         Version='OM.1')  #OM scenario



## specify the final projection year
last_yr <- max(Results$Year)



#Summary of Assesment Fail
summary(Results$AssessFail)

#Summary of the annual RBCs
summary(Results$RBC)

#Summary of OM depletion and Current SSB
summary(Results$Depletion)
summary(Results$SSBcurrent)

#Summary EM depletion and Current SSB
summary(Results$estDepletion)
summary(estSSBcurrent$estDepletion)


#Trends - Each simulation run
  #depletion
Results%>%
  ggplot(aes(Year,Depletion,color=Sim))+
  geom_point(aes(shape=Period))+
  geom_line()+
  ylim(0,1)+theme(legend.position = 'top')

Results_350%>%
  ggplot(aes(Year,Depletion,color=Sim))+
  geom_point(aes(shape=Period))+
  geom_line()+
  ylim(0,1)+theme(legend.position = 'top')

Results_600%>%
  ggplot(aes(Year,Depletion,color=Sim))+
  geom_point(aes(shape=Period))+
  geom_line()+
  ylim(0,1)+theme(legend.position = 'top')


p=Results%>%
  ggplot(aes(Year,SSBcurrent,color=Sim))+
  geom_point(aes(shape=Period))+
  geom_line()+expand_limits(y = 0)+
  theme(legend.position = 'top')
p+
  geom_point(aes(Year,estSSBcurrent,color=Sim),shape=3,size=3)


## Trends OM SSB
fn.plot.trend=function(d,ylab)
{
  p=d%>%
    ggplot(aes(x = Year, y = Mean, colour=Version, fill = Version))+
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.4, colour = NA)+
    geom_line()+
    labs(x = "Year", y = ylab)+
    expand_limits(y = 0)
  print(p)
  return(d)
}
OM_SSB=fn.plot.trend(d=Results %>%
                group_by(Year,Version) %>%
                do(data.frame(rbind(Hmisc::smean.cl.boot(.$SSBcurrent)))),
              ylab="SSB (t)")


## Trends EM SSB
EM_SSB=fn.plot.trend(d=Results %>%
                group_by(Year,Version) %>%
                do(data.frame(rbind(Hmisc::smean.cl.boot(.$estSSBcurrent))))%>% 
                drop_na(),
              ylab="SSB (t)")

#Combine EM and OM
OM_SSB$Analysis = "Operating Model"
EM_SSB$Analysis = "Estimation Method"
SSB_plot_data = bind_rows(OM_SSB, EM_SSB)
SSB_plot_data%>%
ggplot(aes(x = Year, y = Mean, colour = Analysis, fill = Analysis, legend.position="none"))+
  facet_wrap(~Version, scales = "free")+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, colour = NA)+
  geom_line()+
  geom_point()+
  labs(x = "Year", y = "SSB (t)")+
  expand_limits(y = 0)

#Depletion
  #OM
fn.plot.trend(d=Results %>%
                group_by(Year,Version) %>%
                do(data.frame(rbind(Hmisc::smean.cl.boot(.$Depletion)))),
              ylab="Depletion")+xlim(2022,2026)

  #EM 
fn.plot.trend(d=Results %>%
                group_by(Year,Version) %>%
                do(data.frame(rbind(Hmisc::smean.cl.boot(.$estDepletion))))%>% 
                drop_na(),
              ylab="Depletion")+xlim(2022,2026)


#Catch
OM_catch <- Results %>% 
  group_by(Year, Version) %>%
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$TotCatch))))

OM_RBC <- Results %>% 
  group_by(Year, Version) %>%
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$RBC))))%>% 
  drop_na()

OM_TAC <- Results %>% 
  group_by(Year, Version) %>%
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$TAC))))%>% 
  drop_na()


ggplot(data = OM_catch, 
                        aes(x = Year, y = Mean, colour=Version, fill = Version))+
  # geom_hline(yintercept = 0.4, colour = "forestgreen", linetype = "dashed")+
  # geom_hline(yintercept = 0.2, colour = "red", linetype = "dashed")+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.4, colour = NA)+
  geom_line()+
  labs(x = "Year", y = "Catch (t)")+
  ylim(0, 400) + xlim(2010, 2026)+
  geom_ribbon(data=OM_RBC,
              aes(ymin = Lower, ymax = Upper), alpha = 0.4, colour = 'forestgreen')+
  geom_ribbon(data=OM_TAC,
              aes(ymin = Lower, ymax = Upper), alpha = 0.4, colour = 'steelblue')



#Relative error

d_current <- filter(Results, Period == "Sim", Year<=last_yr-1)
## relative error calculation
d_rel_err <- d_current
d_rel_err$SSB0_rel_error <- (d_rel_err$estSSB0 - d_rel_err$SSB0) / d_rel_err$SSB0
d_rel_err$SSB_rel_error <- (d_rel_err$estSSBcurrent_1 - d_rel_err$SSBcurrent) / d_rel_err$SSBcurrent
d_rel_err$Depletion_rel_error <- (d_rel_err$estDepletion_1 - d_rel_err$Depletion) / d_rel_err$Depletion


SSB0_rel_error_dat <- d_rel_err %>% 
  group_by(Year, Version) %>%
  reframe(SSB0_rel_error = quantile(SSB0_rel_error, 
                                    c(0.1, 0.25, 0.50, 0.75,  0.90), na.rm = T), 
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = SSB0_rel_error)

SSB_rel_error_dat <- d_rel_err %>% 
  group_by(Year, Version) %>%
  reframe(SSB_rel_error = quantile(SSB_rel_error, 
                                   c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm = T), 
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = SSB_rel_error)

Dep_rel_error_dat <- d_rel_err %>% 
  group_by(Year, Version) %>%
  reframe(Depletion_rel_error = quantile(Depletion_rel_error, 
                                         c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm = T),
          q = c(0.1, 0.25, 0.50, 0.75, 0.90)) %>%
  pivot_wider(names_from = q, values_from = Depletion_rel_error)
## add some labels
SSB0_rel_error_dat$Var <- "SSB0"
SSB_rel_error_dat$Var <- "SSB"
Dep_rel_error_dat$Var <- "Depletion"
rel_error_dat <- bind_rows(SSB0_rel_error_dat, SSB_rel_error_dat, Dep_rel_error_dat)
rel_error_dat$Var <- factor(rel_error_dat$Var, levels = c("SSB0", "SSB", "Depletion"))
colnames(rel_error_dat) <- c("Year", "Version", "RE_10", "RE_25", "RE_50", "RE_75", "RE_90", "Var")
#rel_error_dat = rel_error_dat %>% filter(Analysis == "T1")
## create the plot
rel_error_dat%>%
  ggplot(aes(x = Year, y = RE_50))+ #, colour = HCR, fill = HCR
  facet_grid(Var~Version)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin = RE_25, ymax = RE_75), alpha = 0.3, colour = NA)+
  geom_ribbon(aes(ymin = RE_10, ymax = RE_90), alpha = 0.4, colour = NA)+
  labs(y = "Relative error", x = "Year")+
  theme(legend.position = "none")
