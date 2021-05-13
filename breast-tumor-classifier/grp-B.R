clinical = read.csv("ClinicalData_82patients.csv", header = T, row.name = 1)

### Relapse.free.survival vs Status
boxplot( clinical$Relapse.free.survival ~ as.factor(clinical$Status), xlab="Status", ylab="Relapse.free.survival")
points( clinical$Relapse.free.survival ~ as.factor(clinical$Status))
pairwise.t.test( clinical$Relapse.free.survival, clinical$Status)

### Relapse.free.survival vs tumor.size
boxplot(clinical$Relapse.free.survival ~ as.factor(clinical$tumor.size), xlab="tumor.size", ylab="Relapse.free.survival")
points(clinical$Relapse.free.survival ~ clinical$tumor.size)
pairwise.t.test(clinical$Relapse.free.survival, clinical$tumor.size)

### ER.status vs Overall.survival
#clinical[,c("ER.status", "Overall.survival")]
boxplot(clinical$Overall.survival ~ as.factor(clinical$ER.status), xlab="ER.status",ylab="Overall.survival")
points(clinical$Overall.survival ~ as.factor(clinical$ER.status))
pairwise.t.test(clinical$Overall.survival, clinical$ER.status)

### ER.status vs node.status
#table(clinical$ER.status)
#table(clinical$node.status)
table(ER=clinical$ER.status,node=clinical$node.status)
barplot(table(clinical$ER.status, clinical$node.status), legend=T, xlab="node.status", ylab="Effectif" )
fisher.test(table(clinical$ER.status, clinical$node.status))

### Grade vs tumor.size.
#table(clinical$Grade)
#table(clinical$tumor.size.)
table(Grade=clinical$Grade, tumor_size=clinical$tumor.size.)
barplot( table(clinical$Grade, clinical$tumor.size.), legend=T, xlab="tumor.size.", ylab="Effectif" )
chisq.test( table(clinical$Grade, clinical$subtype) )
fisher.test( table(clinical$Grade, clinical$subtype) )
