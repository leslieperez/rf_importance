library(MASS)
library(RColorBrewer)
library(GGally)
library(viridis)
library(hrbrthemes)
library(gridExtra)

prefix <- "model_data/acotsp1000-4500-"
#prefix <- "model_data/acotsp2000-"
repetitions <- c("01-","02-", "03-", "04-", "05-")
model_types <- c("perf", "norm", "quan", "irank", "nrank", "qrank")
trepetitions <- c("-r01", "-r02", "-r03", "-r04", "-r05")

getMeasureData <- function(model_type, measure, variable_order=NULL) {
  
  data <- cdata <- NULL
  
  for(i in 1:length(repetitions)) {
    files <- paste0(prefix, repetitions[i], model_type, trepetitions, ".Rdata")
    for (j in 1:length(files)) {
      load(files[j])
      imp <- model$importance_frame
      rownames(imp) <- as.character(imp$variable)
      if (!is.null(variable_order)) {
        imp <- imp[variable_order,]
      }
      if (is.null(data)) {
        if (is.null(variable_order))
          cdata <- as.character(imp$variable[order(imp[,measure])])
        else
          cdata <- rownames(imp)
        data <- as.data.frame(matrix(NA, ncol=length(cdata)+2, nrow=0))
        colnames(data) <- c("rep", "trep", cdata)
      }
      data <- rbind(data, c(i, j, imp[cdata, measure]))
      colnames(data) <- c("rep", "trep", cdata)
    }
  }
  return(data)
}

add_ref <- function(data, measure) {
  cdata <- colnames(data)
  if (measure == "mean_min_depth") {
    data <- rbind(data, rep(0, ncol(data)))
    data <- rbind(data, c(0,0, rep(6, ncol(data)-2)))
  }
  return(data)
}

plotMeasureData <- function(model_type, measure, variable_order=NULL) {
  #cat("Plot", model_type, "\n")
  if (is.null(variable_order))
    variable_order <- c("instance", "algorithm", "localsearch", "ants", "alpha", "beta", "rho", "q0", "nnls", "dlb", "rasrank", "elitistants", "dummy")
  
  datos <- getMeasureData(model_type, measure, variable_order = variable_order)
  #datos <- add_ref(datos, "mean_min_depth")
  my_colors <- colors()[as.numeric(datos$rep) *11]
  datos[,"rep"] <- as.factor(datos[,"rep"])
  #datos <- datos[,-c(1,2)]
  #parcoord(datos, col= my_colors, var.label = TRUE, main ="mean_min_depth")
  
  p <- ggparcoord(datos, 
             columns = 3:ncol(datos),
             scale="globalminmax", 
             groupColumn = 1, 
             showPoints = TRUE, 
             title = paste(measure, "-", model_type)) +
    scale_color_viridis(discrete=TRUE) + xlab("") + ylab("") + theme_ipsum() +
    theme(axis.text.x = element_text(angle = 60,  hjust=1))
  
  ret <- list(data=datos, p=p)
  return(ret)
}

plotAllMeasureData <- function(measure) {
  model_types <- c("perf", "norm", "quan", "irank", "rank", "qrank")
  params <- c("instance", "algorithm", "localsearch", "ants", "alpha", "beta", "rho", "q0", "nnls", "dlb", "rasrank", "elitistants", "dummy")
  
  plot_list <- list()
  for (i in 1:length(model_types)) {
    plot_list[[i]] <- plotMeasureData(model_types[i], measure, params)$p
  }
  
  #wp <- do.call("grid.arrange", c(plot_list))
  wp <- do.call("marrangeGrob", list(grobs=plot_list, ncol = 1, nrow = 6, as.table=FALSE))
  return(wp)
}

allCorrelation <- function(data) {
  params <- colnames(data)
  params <- params[!(params %in% c("rep", "trep"))]
  data <- data[,params]
  all.cor <- c()
  for (i in 1:(nrow(data)-1)) {
    for (j in (i+1):nrow(data)) {
      r <- suppressWarnings(cor.test(x=as.numeric(data[i,]), y=as.numeric(data[j,]), method = "spearman"))
      all.cor <- c(all.cor, r$estimate)
    }
  }
  return(all.cor)
}

correlationBySet <- function(data, do.mean=FALSE) {
  params <- colnames(data)
  params <- params[!(params %in% c("rep", "trep"))]
  reps   <- as.numeric(unique(data[,"rep"]))
  
  if (do.mean)
    correlations <- c()
  else
    correlations <- list()
  for (r in reps) {
    rdata <- data[data[,"rep"]==r,]
    if (do.mean)
      correlations <- c(correlations, mean(allCorrelation(rdata)))
    else
      correlations[[r]] <- allCorrelation(rdata)
  }
  
  return(correlations)
}

plotRanks <- function(data, measure, model_type, decreasing=FALSE) {
  toRank <- function(x, decreasing=FALSE) {
    x <- abs(x)
    if (!decreasing)
      return(rank(x))
    else
      return(rank(-x))
  }
  index  <- c("rep", "trep")
  params <- colnames(data)
  params <- params[!(params %in% index)]
  idata <- data[,index]
  pdata <- data[,params]
  
  ndata <- t(apply(pdata, 1, toRank, decreasing=decreasing))
  data <- cbind(idata, ndata)
  
  p <- ggparcoord(data, 
                  columns = 3:ncol(data),
                  scale="globalminmax", 
                  groupColumn = 1, 
                  showPoints = TRUE, 
                  title = paste(measure, "-", model_type)) +
    scale_color_viridis(discrete=TRUE) + xlab("") + ylab("") + theme_ipsum() +
    theme(axis.text.x = element_text(angle = 60,  hjust=1))
  
  ret <- list(data=data, p=p)
  return(ret)
}

#model_name <- "norm"
#plotMeasureData(model_name, "mean_min_depth")
#plotMeasureData(model_name, "mse_increase")
#plotMeasureData(model_name, "node_purity_increase")
#plotMeasureData(model_name, "times_a_root")
#plotMeasureData(model_name, "no_of_nodes")
