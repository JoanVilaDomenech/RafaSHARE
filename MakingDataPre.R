rm(list=ls())
setwd("/Users/jvila/Dropbox/euit/Doctorants/RafaLlorens/")

library(haven)
library(Epi)
library(Hmisc)
library(gdata)
library(labelled)

datpre <- as.data.frame(read_sav("data/Easy567depurada.sav"))

head(datpre)
str(datpre)

stat.table(list(Country=País, QoL=CASP12), 
           list(N=count(),'%'=percent(CASP12)), 
           data=datpre,margins=T)

stat.table(list(Country=País, Missing=is.na(CASP12)), 
           list(N=count(),'%'=percent(is.na(CASP12))), 
           data=datpre,margins=T)


# ID register
length(unique(datpre$Identificadorpersona))
length(unique(toupper(datpre$Identificadorpersona)))

datpre$ID <- with(datpre, paste(Identificadorpersona, "-", Ola, sep=""))
length(unique(datpre$ID))
Hmisc::label(datpre$ID) <- "Register ID"

# visit number
datpre <- datpre[order(datpre$ID), ]
x2<-sort(datpre$Identificadorpersona)
tt<-table(x2)
ordre <- sapply(1:length(tt), function(i) 1:tt[i])
visnumber <- c(unlist(ordre))
datpre <- cbind(datpre,visnumber)
Hmisc::label(datpre$visnumber) <- "Visit number"

# Identificadorpersona, always 3??
length((repes <- with(datpre,table(Identificadorpersona)))[repes==3])

# couple
tmp <- subset(datpre,visnumber==1)[, c("ID", "Identificadorpersona", "Identificadorhogar", "Identificadorpareja", "Genero", "entrevistapareja")]
tmp <- tmp[order(tmp$Identificadorpersona, tmp$Genero), ]
tmp$coupleID <- substr(tmp$Identificadorpersona,1, 9)
Hmisc::label(tmp$coupleID) <- "Couple/single ID"

x2<- tmp$coupleID
tt<-table(x2)
ordre <- sapply(1:length(tt), function(i) 1:tt[i])
xxx <- c(unlist(ordre))
tmp <- cbind(tmp,xxx)
selec <- subset(tmp, xxx==2)[, "coupleID"]
tmp$couple <- factor(with(tmp, ifelse(coupleID%in%selec, "Y", "N")))
Hmisc::label(tmp$couple) <- "Couple data"

# hetero?
xxx <- subset(tmp, couple=="Y")
xxx <- xxx[order(xxx$coupleID, xxx$Genero), ]
xxx$xxx <- rep(c(0,1), nrow(xxx)/2)
yyy <- xxx[(xxx$xxx!= xxx$Genero), "coupleID"]
tmp$hetero <- with(tmp, ifelse(coupleID==yyy, "Homo", "Hetero"))
Hmisc::label(tmp$hetero) <- "Homo/Hetero relationship"

# merging
datpre <- merge(datpre, tmp[, c("Identificadorpersona", "coupleID", "couple","hetero")], by="Identificadorpersona", all.x=T)

# Sex
datpre$sex <- factor(with(datpre, ifelse(Genero==0, "Male", ifelse(Genero==1, "Female", NA))))
Hmisc::label(datpre$sex) <- "Sex"

# country 
tmp <- val_labels(datpre$País)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "País")
tmp$country <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "País", all.x=T)
datpre$country <- factor(datpre$country)
Hmisc::label(datpre$country) <- "Country"

# versionentrevista
label(datpre$versionentrevista)
datpre$versionentrevista <- as.factor(datpre$versionentrevista )
Hmisc::label(datpre$versionentrevista) <- "Vers. entrevista"

# nacidopaisentrevist
label(datpre$nacidopaisentrevist)
datpre$nacidopaisentrevist <- as.factor(with(datpre, ifelse(nacidopaisentrevist==1, "Y", 
                                                    ifelse(nacidopaisentrevist==2, "N", NA))))
Hmisc::label(datpre$nacidopaisentrevist) <- "Born country Interv."

# age
datpre$age <- floor(datpre$edad)
Hmisc::label(datpre$age) <- "Age"

# yearGroup
tmp <- val_labels(datpre$grupoedad)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "grupoedad")
tmp$yearGroup <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "grupoedad", all.x=T)
datpre$yearGroup <- factor(datpre$yearGroup)
Hmisc::label(datpre$yearGroup) <- "Born year Group"

# ageGroup
tmp <- val_labels(datpre$gruoedad)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "gruoedad")
tmp$ageGroup <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "gruoedad", all.x=T)
datpre$ageGroup <- factor(datpre$ageGroup)
Hmisc::label(datpre$ageGroup) <- "Age Group"

# estadocivil
tmp <- val_labels(datpre$estadocivil)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "estadocivil")
tmp$civilStat <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "estadocivil", all.x=T)
datpre$civilStat <- factor(datpre$civilStat)
Hmisc::label(datpre$civilStat) <- "Civil Status"

# edadpareja
datpre$edadpareja <- floor(datpre$edadpareja)
datpre$edadpareja <- with(datpre, ifelse(edadpareja==0, NA, edadpareja))
Hmisc::label(datpre$edadpareja) <- "Couple Age"

# vivirconyugue
label(datpre$vivirconyugue)
datpre$vivirconyugue <- as.factor(with(datpre, ifelse(vivirconyugue==1, "Y", 
                                                    ifelse(vivirconyugue==2, "N", NA))))
Hmisc::label(datpre$vivirconyugue) <- "Living with a Couple"

# "hijovivecerca",  "vivemadre", "vivepadre", "hermanosvivos", "nivelestudios", "situacionlaboral, llegarfindemes
xvaris <- c("hijovivecerca",  "vivemadre", "vivepadre",  "nivelestudios",
            "situacionlaboral", "llegarfindemes", "zonaresidencia",
            "ayudaeconomica", "saludinfantil", "vacunasinfantil",
            "enfermcronic", "limitacionAVD", "dificultadmovilidad",
            "saludautopercibida", "visitasmedico12meses", "ingresohospital12meses",
            "categoriaIMC", "Fumaadiario", "bebeadiario",
            "activfisica", "escaladepresion", "CASP12")

for (i in 1:length(xvaris)){
    tmp <- val_labels(datpre[, xvaris[i]])
    tmp <- data.frame(tmp)
    tmp <- rename.vars(tmp, "tmp", xvaris[i])
    tmp$xxx <- rownames(tmp)
    
    datpre <- merge(datpre, tmp, by= xvaris[i], all.x=T)
    x <- label(datpre[,xvaris[i]])
    datpre[,xvaris[i]] <- factor(datpre$xxx)
    datpre <- remove.vars(datpre, "xxx")
    Hmisc::label(datpre[,xvaris[i]]) <- x
}

# hermanosvivos
Hmisc::label(datpre$hermanosvivos) <- "Alive brothers"

# limitacionAVD
Hmisc::label(datpre$limitacionAVD) <- "Acti.Vida Diaria Indice W & H"

# saving
dat <- datpre
save(dat, file = "./data/dat.rda")




str(tmp)

