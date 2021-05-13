clinical = read.csv("ClinicalData_82patients.csv", header=T, row.name=1)

###Status vs ER Status
table(Status=clinical$Status, ER.status=clinical$ER.status)
barplot(table(clinical$Status, clinical$ER.status), legend=T, xlab="ER Status", ylab="effectif")
fisher.test(table(clinical$Status, clinical$ER.status))

###Age at diagnosis vs Relapse free survival
plot(clinical$Age.at.diagnosis, clinical$Relapse.free.survival)
#### -> On procède au test du coefficient de corrélation : H0: rho=0 (indépendance des 2 variables)
cor.test(clinical$Age.at.diagnosis, clinical$Relapse.free.survival)
#### -> p>0.05, on ne rejette pas H0. Les 2 variables ne sont donc pas significativement corrélées. On ne contruit donc pas de modèle linéaire.
#model=lm(clinical$Age.at.diagnosis ~ clinical$Relapse.free.survival)
#abline(model)

###Tumor size vs Histology -- Les 2 variables sont qualitatives
table(Tumor.size=clinical$tumor.size., Histo=clinical$Histology)
barplot(table(clinical$tumor.size., clinical$Histology), legend=T, xlab="Histology")
#### --> on ne prend pas en compte les catégories histologiques pour lesquelles il n'y a pas de données de taille de tumeur (Fibroadenoma et Normal breast)
#barplot( table(clinical$tumor.size.[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ], droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])), legend=T, xlab="Histology" )
chisq.test( table(clinical$tumor.size.[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ], droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])) )
fisher.test( table(clinical$tumor.size.[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ], droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])) )

###ER status vs Grade
barplot(table(clinical$ER.status, clinical$Grade), legend=T, xlab="Grade")
fisher.test(clinical$ER.status, clinical$Grade)

###Overall survival vs Tumor size
boxplot(clinical$Overall.survival ~ as.factor(clinical$tumor.size.), legend=T, xlab="tumor.size.", ylab="Overall.survival")
points(clinical$Overall.survival ~ as.factor(clinical$tumor.size.))
pairwise.t.test(clinical$Overall.survival, clinical$tumor.size.)

###Relapse free survival vs Node status
boxplot(clinical$Relapse.free.survival ~ as.factor(clinical$node.status), legend=T, xlab="node.status", ylab="Relapse.free.survival")
points(clinical$Relapse.free.survival ~ as.factor(clinical$node.status))
pairwise.t.test(clinical$Relapse.free.survival,  clinical$node.status)
