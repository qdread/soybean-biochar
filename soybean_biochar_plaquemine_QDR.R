#Load Libraries and Data (AUDPC, yield and severity (X28.Sep))
library(ggplot2); library(nlme); library(gmodels); library(emmeans); library(gdata); library(multcomp)

# Set path based on username
if (Sys.info()['user'] == 'jean.beacorn') file_path <- 'C:/Users/jean.beacorn/Desktop'
if (Sys.info()['user'] == 'qdread') file_path <- 'C:/Users/qdread/onedrive_usda/ars_projects/lima'

Plaquemine.R <- read.csv(file.path(file_path, "Plaquemine.csv"))
View(Plaquemine.R)

#Look at data in raw form first
# 

str(Plaquemine.R)
Plaquemine.R$Replication <- as.factor(Plaquemine.R$Replication)
Plaquemine.R$Treatment <- as.factor(Plaquemine.R$Treatment)
Plaquemine.R$Block <- as.factor(Plaquemine.R$Block)
Plaquemine.R$Row <- as.factor(Plaquemine.R$Row)
Plaquemine.R$Date <- as.factor(Plaquemine.R$Date)
Plaquemine.R$Location <- as.factor(Plaquemine.R$Location)
Plaquemine.R$Ca...ppa. <- as.numeric(Plaquemine.R$Ca...ppa.)
str(Plaquemine.R)

#Visualize data
# 
# #Dot plot


qplot(Treatment, VM.db, data=Plaquemine.R)+ theme_bw()+
  xlab("Biochar x Block")+
  ylab("Volatiles (dry base)")


######Linear models 
#
#Volatile Matter (dry base)
#
lmVM.db <- lm(VM.db ~ Treatment, data = Plaquemine.R)
Plaquemine.R$lmVM.db.resids <- residuals(lmVM.db, type="pearson")
Plaquemine.R$lmVM.db.fitted <- fitted(lmVM.db)
qplot(lmVM.db.fitted, lmVM.db.resids, color=Treatment, data=Plaquemine.R)
qplot(Treatment, lmVM.db.resids, data=Plaquemine.R)
qqnorm(Plaquemine.R$lmVM.db.resids); qqline(Plaquemine.R$lmVM.db.resids)
hist(Plaquemine.R$lmVM.db.resids)
summary(lmVM.db)
anova(lmVM.db) 

(VM.db.lm.emmeans <- (emmeans(lmVM.db, ~ Treatment)))
CLD(VM.db.lm.emmeans)
VM.db.lm.emmeans.sum <- summary(VM.db.lm.emmeans)

ggplot(VM.db.lm.emmeans.sum, aes(Treatment, emmean))+
  geom_point(aes(Treatment, emmean))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))+
  ylab("Volatile Matter (dry base) Percent Difference")+
  xlab("Treatment")
#
#

######Nonlinear models:

#
#volatile Matter (dry base)
#
#Effect of Field Blocking
lmeVM.db <- lme(VM.db ~ Treatment*Block, random=(~1|Replication), weights=varIdent(form = ~1|Replication), method = "REML", data = Plaquemine.R)
Plaquemine.R$lmeVM.db.resids <- residuals(lmeVM.db, type="pearson")
Plaquemine.R$lmeVM.db.fitted <- fitted(lmeVM.db)
qplot(lmeVM.db.fitted, lmeVM.db.resids, color=Replication, data=Plaquemine.R)
qplot(Treatment, lmeVM.db.resids, data=Plaquemine.R)
qqnorm(Plaquemine.R$lmeVM.db.resids); qqline(Plaquemine.R$lmeVM.db.resids)
hist(Plaquemine.R$lmeVM.db.resids)
summary(lmeVM.db)
anova(lmeVM.db) 


(VM.db.lme.emmeans <- (emmeans(lmeVM.db, ~ Treatment)))
CLD(VM.db.lme.emmeans)
VM.db.lme.emmeans.sum <- summary(VM.db.lme.emmeans)

ggplot(VM.db.lme.emmeans.sum, aes(Treatment, emmean))+
  geom_col(aes(Treatment, emmean, fill=Treatment), width = 0.5, alpha = 0.67, position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, color=Block), position = position_dodge(width = 0.7), width=0.2)+
  theme_bw()+
  scale_x_discrete(
    limits=c(
      "Control:Sep-Jun", "Biochar:Sep-Jun" 
    )
  )+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))+
  ylab("Volatile Matter (dry base) Percent Difference")+
  xlab("Treatment")

ggplot(VM.db.lme.emmeans.sum, aes(Block, emmean))+
  facet_wrap(~Treatment)+
  geom_col(aes(Treatment, emmean, fill=Block), width = 0.5, alpha = 0.67, position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, color=Treatment), position = position_dodge(width = 0.7), width=0.2)+
  theme_bw()+
  scale_x_discrete(
    limits=c(
      "Biochar", "Control"
    )
  )+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))+
  ylab("Volatile Matter (dry base) Percent Difference")+
  xlab("Treatment")



