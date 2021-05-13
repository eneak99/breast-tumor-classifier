cl = read.csv("ClinicalData_82patients.csv", header=T, row.name = 1)
### elimine la dernière colonne (Comments) --> 12 variables cliniques
clinical = cl[,-c(13)]

#clinical [ , c("Relapse.free.survival", "Overall.survival") ]
plot( clinical$Relapse.free.survival, clinical$Overall.survival )
cor.test( clinical$Relapse.free.survival, clinical$Overall.survival )

#clinical [ , c("Status", "Overall.survival") ]
boxplot(clinical$Overall.survival ~ as.factor(clinical$Status), xlab="Status", ylab="Overall.survival")
points(clinical$Overall.survival ~ as.factor(clinical$Status))
pairwise.t.test(clinical$Overall.survival, clinical$Status)

#clinical [ , c("Status", "Age.at.diagnosis") ]
boxplot(clinical$Age.at.diagnosis ~ as.factor(clinical$Status), xlab="Status", ylab="Age.at.diagnosis")
points(clinical$Age.at.diagnosis ~ as.factor(clinical$Status))
pairwise.t.test(clinical$Age.at.diagnosis, clinical$Status)

#table(clinical$tumor.size)
#table(clinical$node.status)
table(Tumor_size=clinical$tumor.size, Node=clinical$node.status)
barplot( table(clinical$tumor.size, clinical$node.status), legend=T, xlab="node.status", ylab="Effectif")
fisher.test( table(clinical$tumor.size, clinical$node.status) )

#table(clinical$tumor.size)
#table(clinical$ER.status)
table(Tumor_size=clinical$tumor.size, ER=clinical$ER.status)
barplot( t(table(clinical$tumor.size, clinical$ER.status)), legend=T, xlab="tumor.size", ylab="Effectif")
fisher.test( table(clinical$tumor.size, clinical$ER.status) )

#clinical [ , c("ER.status", "Relapse.free.survival") ]
boxplot(clinical$Relapse.free.survival ~ as.factor(clinical$ER.status), xlab="ER.status", ylab="Relapse.free.survival")
points(clinical$Relapse.free.survival ~ as.factor(clinical$ER.status))
pairwise.t.test(clinical$Relapse.free.survival, clinical$ER.status)
