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
################################################################################
# checking linearity
################################################################################
if (!require(gam)) install.packages("gam")
xdat <- subset(dat, visnumber==3)
xdat$temp <- xdat[, "chronicNum"]

fit <- gam(formula = casp.12 ~ s(temp), family = "gaussian", 
           data = xdat, na.action = na.exclude)
summary(fit)
par(xpd = FALSE, las = 1)
plot(fit, terms = "s(temp)", se = TRUE, axes = FALSE)
axis(1, pretty(par()$usr[1:2]))
axis(2, pretty(par()$usr[3:4]))
abline(v = median(dat$numerohijos), lty = 2, col = "red")
abline(h = 0, lty = 2, col = "red")
################################################################################
################################################################################


################################################################################
################################################################################
xvaris <- c("Identificadorpersona", "visnumber", "country", "ageRecru", "sex", "PartiType", "arearesidencia", 
            "nivelestudios", "saludautopercibida", "numhijosGR", 
            "numnietosGR", "llegafindemes", "saludinfancia", 
            "civilStat", "chronicCat", "limitacionAVD", "saludautopercibida", 
            "visitasmedico12meses", "Fumaadiario", "bebeadiario", 
            "activfisica", "escaladepresion") 
sum(complete.cases(dat[, xvaris]))

set.seed(03032022)
x <- lmer(casp.12 ~ C(as.factor(visnumber), base= 1) + 
            relevel(country, ref = "Spain") +
            ageRecru + sex + PartiType +
            arearesidencia + nivelestudios + saludautopercibida + 
            numhijosGR + numnietosGR + llegafindemes + saludinfancia +
            civilStat + chronicCat + limitacionAVD + saludautopercibida + 
            visitasmedico12meses + Fumaadiario + bebeadiario +
            activfisica + escaladepresion +
            (1 | Identificadorpersona), 
            data = dat)
summary(x)
CI2 <- confint(x, maxpts = 8)
resuMixed <- data.frame(cbind(summary(x)$coefficients, CI2[3:nrow(CI2), ]))[, c(1,4, 5)]

for (i in 3:length(xvaris)){
rownames(resuMixed)[grep(xvaris[i], rownames(resuMixed))] <- gsub(xvaris[i], paste(xvaris[i], ": ", sep=""), rownames(resuMixed)[grep(xvaris[i], rownames(resuMixed))])
}
colnames(resuMixed) <- c("Estimate", "Lower95CI", "Upper95CI")

resuMixed$Signif <- with(resuMixed, ifelse(Lower95CI<0 & Upper95CI>0, "NS", "" ))
################################################################################
# checking variables to be removed
################################################################################
resu <- NULL
for (i in 3:length(xvaris)){
    TheVari <- xvaris[i]
    x1 <- update(x, formula = paste(". ~ . - ", TheVari, sep=""))
    resu <- rbind(resu, c(TheVari, anova(x, x1)$`Pr(>Chisq)`[2]))
}
resu1 <- data.frame(resu)[1:20, ]
resu1$X2 <- round(as.numeric(resu1$X2), 4)
resu1[order(resu1$X2, decreasing = TRUE), ]

################################################################################
# removing: Fumaadiario, chronicCat, arearesidencia
################################################################################
set.seed(03032022)
xx <- lmer(casp.12 ~ C(as.factor(visnumber), base= 1) + 
                relevel(country, ref = "Spain") +
                ageRecru + sex + PartiType +
                nivelestudios + saludautopercibida +
                numhijosGR + numnietosGR + llegafindemes + saludinfancia +
            civilStat  + limitacionAVD + saludautopercibida + 
            visitasmedico12meses + bebeadiario +
            activfisica + escaladepresion +
                (1 | Identificadorpersona), 
            data = dat)
summary(xx)                   
CIxx <- confint(xx, maxpts = 8)
resuMixedxx <- data.frame(cbind(summary(xx)$coefficients, CIxx[3:(nrow(CIxx)), ]))[, c(1,4, 5)]

xvarisxx <- xvaris[xvaris%nin%c("Fumaadiario", "chronicCat", "arearesidencia")]
for (i in 3:length(xvarisxx)){
rownames(resuMixedxx)[grep(xvarisxx[i], rownames(resuMixedxx))] <- gsub(xvarisxx[i], paste(xvarisxx[i], ": ", sep=""), rownames(resuMixedxx)[grep(xvarisxx[i], rownames(resuMixedxx))])
}
colnames(resuMixedxx) <- c("Estimate", "Lower95CI", "Upper95CI")

resuMixedxx$Signif <- with(resuMixedxx, ifelse(Lower95CI<0 & Upper95CI>0, "NS", "" ))

################################################################################
# checking variables to be removed
################################################################################
resuxx <- NULL
for (i in 3:length(xvarisxx)){
    TheVari <- xvarisxx[i]
    x1xx <- update(xx, formula = paste(". ~ . - ", TheVari, sep=""))
    resuxx <- rbind(resuxx, c(TheVari, anova(xx, x1xx)$`Pr(>Chisq)`[2]))
}
resu1xx <- data.frame(resuxx)[2:nrow(resuxx), ]
resu1xx$X2 <- round(as.numeric(resu1xx$X2), 4)
resu1xx[order(resu1xx$X2, decreasing = TRUE), ]

# numhijosGR (la deixo per confusor) 


mod1 <- data.frame(xname= rownames(resuMixed), betaMod1= resuMixed$Estimate)
modend <- data.frame(xname= rownames(resuMixedxx), betaModEnd= resuMixedxx$Estimate)

bothmodels <- merge(mod1, modend, by= "xname", all.x=T)

bothmodels$change <- round(with(bothmodels, (betaMod1 - betaModEnd)/betaMod1*100 ), 2)

save.image(file = "../data/Mixed.rda")

################################################################################
################################################################################
################################################################################
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
load(file = "../data/Mixed.rda")

resuMixed$xname <- rownames(resuMixed)
resuMixed$order <- seq(1, nrow(resuMixed))
resuMixedxx$xname <- rownames(resuMixedxx)

ResuModels <- merge(resuMixed, resuMixedxx, by = "xname", all.x = T)
ResuModels <- ResuModels[order(ResuModels$order), ]
ResuModels <- remove.vars(ResuModels, "order")
ResuModels <- rename.vars(ResuModels, c("Estimate.x", "Lower95CI.x", "Upper95CI.x",
            "Signif.x", "Estimate.y", "Lower95CI.y", "Upper95CI.y", "Signif.y"),
            c("Mod1Estimate", "Mod1Lower95CI", "Mod1Upper95CI",
            "Mod1Signif", "Mod2Estimate", "Mod2Lower95CI", "Mod2Upper95CI", "Mod2Signif"))
xvaris <- c("Mod1Estimate", "Mod1Lower95CI", "Mod1Upper95CI",
            "Mod2Estimate", "Mod2Lower95CI", "Mod2Upper95CI")
for (i in 1:length(xvaris)){
    ResuModels[, xvaris[i]] <- round(ResuModels[, xvaris[i]], 4)
}
ResuModels <- ResuModels[-1,]

save(ResuModels, file = "../data/ResuModels.rda")




