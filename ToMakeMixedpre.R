rm(list=ls())
setwd("/Users/jvila/Dropbox/euit/Doctorants/RafaLlorens/SHARE/RafaSHARE/")
library(haven)
library(Epi)
library(Hmisc)
library(gdata)
library(compareGroups)
library(ggplot2)
library(lme4)

RutinesLocals <- "/Users/jvila/Dropbox/rutines"
source(file.path(RutinesLocals,"intervals.r"))

load("../data/dat.rda")
resu2 <- lmer(casp.12 ~ C(as.factor(visnumber), base= 1) + 
                relevel(country, ref = "Spain") +
                ageRecru + sex + PartiType +
                (1 | Identificadorpersona), 
            data = dat)
CI2 <- confint(resu2, maxpts = 8)
resuMixed <- data.frame(cbind(summary(resu2)$coefficients, CI2[3:20, ]))[, c(1,4, 5)]

save(resuMixed, file= "../data/ResuMixed.rda")








