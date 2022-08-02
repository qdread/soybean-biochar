#Load Libraries and Data (AUDPC, yield and severity (X28.Sep))
library(ggplot2); library(nlme); library(gmodels); library(emmeans); library(gdata); library(multcomp)
library(lme4)
library(performance)

# Set path based on username
if (Sys.info()['user'] == 'jean.beacorn') file_path <- 'C:/Users/jean.beacorn/Desktop'
if (Sys.info()['user'] == 'qdread') file_path <- 'C:/Users/qdread/onedrive_usda/ars_projects/lima'

Plaquemine.R <- read.csv(file.path(file_path, "Plaquemine.csv"))
Plaquemine.R$Row <- factor(Plaquemine.R$Row)
Plaquemine.R$Col <- as.numeric(factor(Plaquemine.R$Block))

lmm_vmdb <- lmer(VM.db ~ Treatment + (1|Block) + (1|Row), data = Plaquemine.R)

# Built in diagnostics. Nothing appears to be problematic.
# Also see https://datascienceplus.com/spatial-regression-in-r-part-1-spamm-vs-glmmtmb/
check_model(lmm_vmdb)

(emm_vmdb <- emmeans(lmm_vmdb, ~ Treatment))
cld(emm_vmdb, Letters = letters)

# Plots with treatments' estimated marginal means.
ggplot(as.data.frame(emm_vmdb), aes(Treatment, emmean))+
  geom_col(aes(Treatment, emmean, fill=Treatment), width = 0.5, alpha = 0.67, position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), position = position_dodge(width = 0.7), width=0.2)+
  theme_bw()+
  scale_x_discrete(
    labels=c(
      "Control:Sep-Jun", "Biochar:Sep-Jun" 
    )
  )+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))+
  ylab("Volatile Matter (dry base) Percent Difference")+
  xlab("Treatment")

# Block level random effect estimates.
testSpatialAutocorrelation(lmm_vmdb, x = Plaquemine.R$Col, y = as.numeric(Plaquemine.R$Row))
