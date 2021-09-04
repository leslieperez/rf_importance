source("artificial_landscape.R")
source("importance.R")

le <- Landscape$new("landscapes/parameters-conditional.txt", "landscapes/simple-conditional-landscape1.txt")
#le <- Landscape$new("landscapes/cases/param1.txt", "landscapes/cases/case1.txt")
le$print()


# create configuration
x <- c(1,2,1,0.3,1)
names(x) <- le$pnames
x<- le$deactivateConfig(x)

cat("Configuration: ", x, "\n")

cat("Partial contribution to eval:\n")
print(le$getPartialEval(x))

cat("Total eval: ",le$getEval(x), "\n")

cat("Local importance: \n")
for(pname in le$pnames) {
  cat("  parameter ", pname, ": ", isLocallyImportant(pname, x, le),"\n")
}
