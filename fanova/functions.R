## all performance measures
measures=c("perf","norm","quan","rank","irank","qrank")

## wrap strings `x` into at most `len` characters per line
wrap.it <- function(x, len) { 
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

## return canonical file name for base name `dataset` and replication `rep`
resultFile <- function(dataset,mname,rep=1) {
    reps=paste("_r",rep,sep="")
    paste(dataset,"-",mname,"-importance",reps,".dat",sep="")
}

## read importance data from `fname`, sort it, add a tag if requested
readImportance <- function(fname,tag="") {
    data=read.table(fname,h=T) %>% select(parameter,ind_imp,ind_std) %>% arrange(-ind_imp)
    if (tag!="") {
        data$measure=tag
    }
    colnames(data)[1:3]=c("variable","importance","std_dev")
    return(data)
}

## produce a formatted table with parameters in order of importance
importanceTable <- function(dataset,mname) {
  data=readImportance(resultFile(dataset,mname))
  kable(data,caption=paste("Importance for measure ``",mname,"'' (single run)",sep=""), digits=c(0,3,3)) %>% kable_styling(latex_options=c("striped","hold_position"))
}

## read importance data for all measures
readAll <- function(dataset) {
  data=data.frame()
  for (mname in measures) {
    data = data %>% rbind(readImportance(resultFile(dataset,mname),tag=mname))
  }
  return(data)
}

## read importance data for five replications
readReplications <- function(dataset,mname) {
  data=data.frame()
  for (r in c(1,2,3,4,5)) {
      data = data %>% rbind(readImportance(resultFile(dataset,mname,rep=r),tag=r))
  }
  return(data)
}

eps=0.000001

## a bump chart for the first replication of a dataset over all measures
bumpChartMeasures <- function(dataset,do.rank=TRUE) {
    data=readAll(dataset) %>% group_by(measure) %>% mutate(measure=factor(measure,levels=measures,ordered=T))
    if (do.rank)
        data=data %>% mutate(rank=rank(importance))
    else
        data=data %>% mutate(rank=importance+eps)
    
    first=data %>% filter(measure=="perf")
    plot=ggplot(data=data,aes(x=measure,y=rank,group=variable,color=variable))+geom_line()+geom_text(data=first,aes(x=1,y=first$rank,label=wrap.it(first$variable,10)),hjust=1.1,size=4)+labs(title="Ranking under different measures",x="Measure",y="Rank")+theme(legend.position="none")+geom_blank(aes(x=0, y=1))+geom_point()#+geom_jitter(width=0.05,height=0)
    if (!do.rank)
        plot=plot+geom_linerange(aes(ymin=pmax(rank-std_dev,eps),ymax=pmin(rank+std_dev,1)))+scale_y_log10()
    plot
}

## a bump chart for all (5) replications of a data set over a given measure
bumpChartReplications <- function(dataset,mname,do.rank=TRUE) {
    data=readReplications(dataset,mname) %>% group_by(measure)
    if (do.rank)
        data=data %>% mutate(rank=rank(importance))
    else
        data=data %>% mutate(rank=importance+eps)
    
    first=data %>% filter(measure==1)
    plot=ggplot(data=data,aes(x=measure,y=rank,group=variable,color=variable))+geom_line()+geom_text(data=first,aes(x=1,y=first$rank,label=wrap.it(first$variable,10)),hjust=1.1,size=4)+labs(title=paste("Ranking over different replications for measure ``",mname,"''",sep=""),x="Replication",y="Rank")+theme(legend.position="none")+geom_blank(aes(x=0, y=1))+geom_point()#+geom_jitter(width=0.05,height=0)

    if (!do.rank)
        plot=plot+geom_linerange(aes(ymin=pmax(rank-std_dev,eps),ymax=pmin(rank+std_dev,1)))+scale_y_log10()
    plot
}
