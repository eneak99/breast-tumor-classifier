cl = read.csv("ClinicalData_82patients.csv", header=T, row.name = 1)
clinical = cl[,-c(13)]

### Age.at.diagnosis vs Histology
boxplot(clinical$Age.at.diagnosis ~ clinical$Histology, xlab="Histology", ylab="Age.at.diagnosis")
points(clinical$Age.at.diagnosis ~ clinical$Histology)

#on ne prend pas en compte les catégories de la variable Histology pour lesquelles il y a moins de 2 patients avec une valeur d'age :
boxplot( clinical$Age.at.diagnosis[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ] ~ droplevels(clinical$Histology[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ]), xlab="Histology", ylab="Age.at.diagnosis")
points( clinical$Age.at.diagnosis[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ] ~ droplevels(clinical$Histology[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ]))

pairwise.t.test(clinical$Age.at.diagnosis[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ] , droplevels(clinical$Histology[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ]))

### Overall.survival vs Grade
boxplot( clinical$Overall.survival ~ as.factor(clinical$Grade), xlab="Grade", ylab="Overall.survival" )
points( clinical$Overall.survival ~ as.factor(clinical$Grade) )
pairwise.t.test( clinical$Overall.survival, clinical$Grade)

### Relapse.free.survival vs Histology
boxplot( clinical$Relapse.free.survival ~ clinical$Histology, xlab="Histology", ylab="Relapse.free.survival")
points( clinical$Relapse.free.survival ~ clinical$Histology)
#on ne prend pas en compte les catégories de la variable Histology pour lesquelles il y a moins de 2 patients avec une valeur de survie:
pairwise.t.test (clinical$Relapse.free.survival[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ], droplevels(clinical$Histology[ clinical$Histology != "Mucinous" & clinical$Histology != "Normal breast" & clinical$Histology != "Papillary" & clinical$Histology != "Pleomorph" ]))

### node.status vs reference.sample.batch.ID
table(Node_status=clinical$node.status, Ref=clinical$reference.sample.batch.ID)
barplot( table(clinical$node.status, clinical$reference.sample.batch.ID), legend=T, xlab="reference.sample.batch.ID")
#on ne prend pas en compte la madalité CDB de la référence (pas de données de node.status, voir colonne table avec 0)
fisher.test( clinical$node.status[ clinical$reference.sample.batch.ID!="CDB" ], droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID!="CDB" ]))

### Grade vs p53.status
table(Grade=clinical$Grade, p53=clinical$p53.status)
barplot( table(clinical$Grade, clinical$p53.status), legend=T, xlab="p53.status")
fisher.test( clinical$Grade, clinical$p53.status )
