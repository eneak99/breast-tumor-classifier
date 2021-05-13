clinical= read.csv("ClinicalData_82patients.csv",header=T,row.names = 1)

### subtype vs Age.at.diagnosis
boxplot( clinical$Age.at.diagnosis ~ clinical$subtype, xlab="subtype", ylab="Age.at.diagnosis")
points( clinical$Age.at.diagnosis ~ clinical$subtype)
pairwise.t.test( clinical$Age.at.diagnosis, clinical$subtype )

### Status vs reference.sample.batch.ID
#table(clinical$Status)
#table(clinical$reference.sample.batch.ID)
table(Status=clinical$Status, Ref=clinical$reference.sample.batch.ID)
barplot( table(clinical$Status, clinical$reference.sample.batch.ID), legend=T)
# on ne prend pas en compte la référence CDB, sous-représentée
chisq.test( table(droplevels(as.factor(clinical$Status[clinical$reference.sample.batch.ID!="CDB"])), droplevels(clinical$reference.sample.batch.ID[clinical$reference.sample.batch.ID!="CDB"])) )
# on procède au test de Fisher car les conditions de validité du test de Chi2 ne sont pas réunies (effectifs faibles, n<5 pour certaines combinaisons status/ref)
fisher.test( table(droplevels(as.factor(clinical$Status[clinical$reference.sample.batch.ID!="CDB"])), droplevels(clinical$reference.sample.batch.ID[clinical$reference.sample.batch.ID!="CDB"])) )

### Overall.survival vs p53.status
# les catégories de la variable p53.status sont des valeurs numériques. Il faut indiquer explicitement que ce sont des catégories (labels) et non des nombres. Pour cela, on utilise la fonction as.factor()
boxplot( clinical$Overall.survival ~ as.factor(clinical$p53.status), xlab="p53.status", ylab="Overall.survival")
points( clinical$Overall.survival ~ as.factor(clinical$p53.status))
pairwise.t.test( clinical$Overall.survival, clinical$p53.status)

### Relapse.free.survival vs Grade
# les catégories de la variable Grade sont des valeurs numériques. Il faut indiquer explicitement que ce sont des catégories (labels) et non des nombres. Pour cela, on utilise la fonction as.factor()
boxplot( clinical$Relapse.free.survival ~ as.factor(clinical$Grade), xlab="Grade", ylab="Relapse.free.survival")
points( clinical$Relapse.free.survival ~ as.factor(clinical$Grade))
pairwise.t.test( clinical$Relapse.free.survival, clinical$Grade)

### node.status vs Grade
#table(clinical$node.status)
#table(clinical$Grade)
table(node_status=clinical$node.status, grade=clinical$Grade)
barplot( table(clinical$node.status, clinical$Grade), legend=T, xlab="Grade")
#chisq.test( table(clinical$node.status, clinical$Grade) )
fisher.test( table(clinical$node.status, clinical$Grade) )
