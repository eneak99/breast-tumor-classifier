cl = read.csv("ClinicalData_82patients.csv", header=T, row.name = 1)

##association age et status de la tumeur

stripchart(cl$Age.at.diagnosis, method='jitter', xlab="Age.at.diagnosis")
table(cl$Status)
boxplot(cl$Age.at.diagnosis ~ as.factor(cl$Status),xlab="Status",ylab="Age.at.diagnosis")
points(cl$Age.at.diagnosis ~ as.factor(cl$Status))
pairwise.t.test( cl$Age.at.diagnosis, cl$Status)

##association entre le grade et le status de la tumeur

#table(cl$Status)
#table(cl$Grade)
table(Grade=cl$Grade,Status=cl$Status)
barplot(table(cl$Grade,cl$Status),legend=T, xlab="Status")
chisq.test(table(cl$Grade,cl$Status))
fisher.test(table(cl$Grade,cl$Status))


##association entre la survie globale et l'histologie

stripchart(cl$Overall.survival, method='jitter', xlab="Overall.survival")
table(cl$Histology)
boxplot(cl$Overall.survival ~ as.factor(cl$Histology),xlab="Histology",ylab="Overall.survival")
points(cl$Overall.survival ~ as.factor(cl$Histology))
#on ne prend pas en compte les effectifs <=2 (catŽgories Pleomorph, Papillary, Mucinous, DCIS de la variable Histology) ni les donnŽes manquantes (NA pour la variable $Overall.survival)
pairwise.t.test( cl$Overall.survival[ cl$Histology!="Pleomorph" & cl$Histology!="Papillary" & cl$Histology!="Mucinous" & cl$Histology!="DCIS" & is.na(cl$Overall.survival) == F ], droplevels(cl$Histology[ cl$Histology!="Pleomorph" & cl$Histology!="Papillary" & cl$Histology!="Mucinous" & cl$Histology!="DCIS" & is.na(cl$Overall.survival) == F ]))

##association entre le sous-type et survie sans rechute

stripchart(cl$Relapse.free.survival, method='jitter', xlab="Relapse.free.survival")
table(cl$subtype)
boxplot(cl$Relapse.free.survival ~ cl$subtype, xlab="subtype", ylab="Relapse.free.survival")
points(cl$Relapse.free.survival ~ cl$subtype)
pairwise.t.test(cl$Relapse.free.survival, cl$subtype)

##association entre le status des ERr et l'échantillon de reference

#table(cl$ER.status)
table(cl$ER.status,cl$reference.sample.batch.ID)
barplot(table(cl$ER.status,cl$reference.sample.batch.ID), legend=T, xlab="reference.sample.batch.ID")
fisher.test(table( droplevels(as.factor(cl$ER.status[ cl$reference.sample.batch.ID != "CDB" ])), droplevels(cl$reference.sample.batch.ID[ cl$reference.sample.batch.ID != "CDB" ])))

##association entre la taille de la tumeur et le status de la p53

#table(cl$tumor.size.)
#table(cl$p53.status)
table(cl$tumor.size.,cl$p53.status)
barplot(table(cl$tumor.size.,cl$p53.status), legend=T, xlab="p53.status")
fisher.test(table(cl$tumor.size.,cl$p53.status))
