source("artificial_landscape.R")
source("importance.R")

# Case 1: interaction of two parameters
#le <- Landscape$new("landscapes/cases/param1.txt", "landscapes/cases/case1.txt")
#le <- Landscape$new("landscapes/cases/param2.0.txt", "landscapes/cases/case2.0.txt")
#le <- Landscape$new("landscapes/cases/param2.1.txt", "landscapes/cases/case2.1.txt")
#le <- Landscape$new("landscapes/cases/param2.2.txt", "landscapes/cases/case2.2.txt")
#le <- Landscape$new("landscapes/cases/param2.3.txt", "landscapes/cases/case2.3.txt")
#le <- Landscape$new("landscapes/cases/param3.txt", "landscapes/cases/case3.txt")
#le <- Landscape$new("landscapes/cases/param4.txt", "landscapes/cases/case4.txt")
#le <- Landscape$new("landscapes/cases/param5.txt", "landscapes/cases/case5.txt")
le <- Landscape$new("landscapes/cases/param6.txt", "landscapes/cases/case6.txt")
#le$print()

# create configuration

 if(FALSE){
   x <- le$getConfiguration(c(2,1))
   cat("Configuration: ", x, "\n")
   cat("Partial contribution to eval:\n")
   print(le$getPartialEval(x))
   cat("Weights:\n")
   print(le$getWeights(x))
   cat("Total eval: ",le$getEval(x), "\n")
   cat("Local importance: \n")
   for(pname in le$pnames) {
     cat("  parameter ", pname, ": ", isLocallyImportant(pname, x, le),"\n")
   }
 }

le$listAll()
