############################################################################
## function for printing a dataframe to pdf;
## Usage:
# pdf(file="Filename.pdf", width=11, height=8.5, paper="special")
# 
# grid.draw(PrintTable(Tbl=DataFrame1,
#                      Title="TableTitle",
#                      TitleFont=14))
# grid.newpage()
# grid.draw(PrintTable(Tbl=DataFrame,
#                      Title="TableTitle",
#                      TitleFont=14))
# dev.off()


PrintTable=function(Tbl, Title, TitleFont, BaseFont=10) {
  TB <- tableGrob(Tbl, theme=ttheme_default(base_size = BaseFont, base_colour = "black", 
                                            parse = FALSE, padding = unit(c(4, 4), "mm")))
  title <- textGrob(Title,gp=gpar(fontsize=TitleFont))
  padding <- unit(0.5,"line")
  TB <- gtable_add_rows(TB, heights = grobHeight(title) + padding, pos = 0)
  TB <- gtable_add_grob(x=TB, grobs=list(title), t=c(1), l=c(1), r=ncol(TB) )
}


############################################################################
##NOT-Idiot-proof function to unstack a long dataframe into a long format, conserving the test strings in the factor columns as the new column names
# input variables: df - dataframe to be manipulated
#                   fName - name for the column that contains the new column headings
#                   vName - name for the column that contains the data being unstacked
# See example below code.

# Function works only if all subsets have the same identification matrix.

Long2Wide=function(df, fName, vName, NullVal=NA, Prefix="") {
  #  if(nargs()==3){Prefix=""}
  
  fCol=which(names(df)==fName);
  vCol=which(names(df)==vName);
  
  Names=names(df[-c(fCol, vCol)]);
  
  UfN=as.character(sort(unique(df[ ,fCol])))
  Key=data.frame(unique(df[ ,Names]));
  names(Key)=Names
  for(i in 1:ncol(Key)){
    if(i==1) {strKey=Key[ ,i]} else {strKey=paste(strKey, Key[ ,i], sep="_")}
  }
  Wide=data.frame(matrix(data=NullVal, ncol=ncol(Key)+length(UfN), nrow=nrow(Key)))
  names(Wide)=c(names(Key), paste(Prefix, UfN, sep=""))
  Wide[ ,1:ncol(Key)]=Key
  
  for (i in 1:length(UfN)){
    tempKey=data.frame(df[which(df[ ,fCol]==UfN[i]),Names]);
    names(tempKey)=Names
    for(j in 1:ncol(tempKey)){
      if(j==1) {strTempKey=tempKey[ ,j]} else {strTempKey=paste(strTempKey, tempKey[ ,j], sep="_")}
    }
    tempData=df[which(df[ ,fCol]==UfN[i]),vCol];
    
    Wide[ which(match(strKey, strTempKey)>0), i+ncol(Key)]=
      tempData[na.omit(match(strKey, strTempKey))]
  }
  
  
  return(Wide)
}

# example taken from reshape:
#  dfLong <- data.frame(id=rep(1:4,3), age=c(40,50,60,50,40,50,60,50,40,50,60,50), Dose=rep(c("dose1", "dose2", "dose4"), each=4), Dosage=c(1,2,1,2,2,1,2,1,3,3,3,3)); dfLong
#         id age Dose   Dosage
#      1   1  40 dose1     1
#      2   2  50 dose1     2
#      3   3  60 dose1     1
#      4   4  50 dose1     2
#      5   1  40 dose2     2
#      6   2  50 dose2     1
#      7   3  60 dose2     2
#      8   4  50 dose2     1
#      9   1  40 dose4     3
#      10  2  50 dose4     3
#      11  3  60 dose4     3
#      12  4  50 dose4     3

#  dfWide=Long2Wide(df, "Dose", "Dosage"); dfWide
#       id age dose1 dose2 dose4
#    1  1  40     1     2     3
#    2  2  50     2     1     3
#    3  3  60     1     2     3
#    4  4  50     2     1     3


############################################################################
##Idiot-proof function to stack a wide dataframe into a long format, conserving the column names as a text string in a new column
# input variables: df - dataframe to be manipulated
#                   cols - the column numbers to be stacked
#                   fName - name for the column that will contain the old column headings
#                   vName - name for the column that will contain the data being stacked
# See example below code.

Wide2Long=function(df, cols, fName, vName, verbose=FALSE) {
  IndexCol=1:ncol(df); IndexCol=IndexCol[-cols]; # define index columns of df
  IndexNames=c(names(df)[IndexCol])
  Index=data.frame(df[ ,IndexCol]); names(Index)[]=IndexNames; # extract and name index columns
  IndexRows=nrow(Index); # calculate number of rows in Index
  
  Output=data.frame(matrix(data=NA, nrow=length(cols)*IndexRows, ncol=length(IndexNames)+2))
  names(Output)=c(IndexNames, fName, vName)
  
  col.fName=length(IndexNames)+1
  col.vName=col.fName+1
  
  for(i in 1:length(IndexNames) ) {
    Output[ ,i] = rep(Index[ ,i], length(cols))
  }
  
  for (i in 1:length(cols)) {
    if(verbose==TRUE) {print(paste(i, "/", length(cols)))}
    fI=rep(names(df)[(cols[i])],IndexRows)
    vI=df[ ,cols[i]];
    Output[((i-1)*IndexRows+1) : (i*IndexRows), col.fName]=fI;
    Output[((i-1)*IndexRows+1) : (i*IndexRows), col.vName]=vI;
  }
  
  return(Output)
}

# example taken from reshape:
#  df <- data.frame(id=1:4, age=c(40,50,60,50), dose1=c(1,2,1,2), dose2=c(2,1,2,1), dose4=c(3,3,3,3)); df
#         id age dose1 dose2 dose4
#      1  1  40     1     2     3
#      2  2  50     2     1     3
#      3  3  60     1     2     3
#      4  4  50     2     1     3

#  Wide2Long(df, c(3:5), "Dose", "Dosage")
#         id age Dose   Dosage
#      1   1  40 dose1     1
#      2   2  50 dose1     2
#      3   3  60 dose1     1
#      4   4  50 dose1     2
#      5   1  40 dose2     2
#      6   2  50 dose2     1
#      7   3  60 dose2     2
#      8   4  50 dose2     1
#      9   1  40 dose4     3
#      10  2  50 dose4     3
#      11  3  60 dose4     3
#      12  4  50 dose4     3


############################################################################
## function for reformatting output tables
TableFormat=function(DF, Digits){ 
  rownames(DF)=1:nrow(DF)
  DF$Default=formatC(DF$Default, big.mark=",", format="f", digits=Digits)
  DF$Scenario=formatC(DF$Scenario, big.mark=",", format="f", digits=Digits)
  {DF$Reduction[!is.na(DF$Reduction)]=paste(round(DF$Reduction[!is.na(DF$Reduction)],3)*100, " %", sep="");}
  return(DF)
}  


############################################################################
## function to building output maps
OutputMap=function(
  InputDF, ## data frame of values to be mapped
  VarName, ## string; name of variable to plot
  DefaultVal=0,
  AggregateFunction="sum",
  UpdateIndex,  ##New for V3.0.3
  Plot1_Title,
  PlotDF, ## data frame with all combinations of index and month, automatically passed
  MapRef, ## spatial pixels data frame with spatial extent; must be specified.
  Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
  PlotLog=FALSE,
  LogOffset,
  Plot2_Title
){
  
  MonthString=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  MonthOrder=c("Sep", "Oct", "Nov", "Dec",
               "May", "Jun", "Jul", "Aug", 
               "Jan", "Feb", "Mar", "Apr")
  
  MonthDF=data.frame(
    Month=1:12,
    MonthName=MonthString
  )
  
  ##V3.0.3 updates Index name from Index LR to Index if this column needs to be updated for the run
  if (UpdateIndex){
    InputDF=InputDF[ ,c("Index_LR", "Month", VarName)]; #summary(InputDF)
  }else{
    InputDF=InputDF[ ,c("Index", "Month", VarName)]; #summary(InputDF) 
  }##ends rename loop
  names(InputDF)=c("Index", "Month", "Values")
  ExpandedDF=merge(InputDF, PlotDF, all.y=TRUE); #summary(ExpandedDF)
  ExpandedDF$Values[is.na(ExpandedDF$Values)]=DefaultVal
  
  #########################################################--
  MapRefPx=try(as(MapRef, "SpatialPixelsDataFrame"))
  if(attributes(MapRefPx)$class=="try-error") {
    MapRefPx=MapRef
  }
  
  Plot1_Agg=aggregate(Values~Index+Month, ExpandedDF, AggregateFunction); ###summary(Plot1_Agg)
  Plot1_Wide=Long2Wide(Plot1_Agg, fName="Month", vName="Values", Prefix="m_"); ###summary(Plot1_Wide)
  names(Plot1_Wide)=c("Index", MonthString); ###summary(Plot1_Wide)
  Plot1_Px=merge(MapRefPx[ ,c("Index")], Plot1_Wide); ###summary(Plot1_Px)
  
  map1=
    spplot(Plot1_Px[MonthOrder],
           Cuts=100,
           sp.layout=Sp.Layout,
           main=Plot1_Title,
           layout=c(4,3)
    ); ## map1
  
  #######################################################--
  if(PlotLog){
    Plot2_Agg=Plot1_Agg;
    Plot2_Agg$Values=Plot2_Agg$Values+LogOffset
    Plot2_Agg$LogValues=log10(Plot2_Agg$Values);
    
    Ymax=floor(max(Plot2_Agg$LogValues)); Ymax; Yseq=0:Ymax; Yval=10^Yseq;
    
    Plot2Log_Wide=Long2Wide(Plot2_Agg[ ,c("Index", "Month", "LogValues")],
                            fName="Month", vName="LogValues", Prefix="m_"); ###summary(Plot2Log_Wide)
    names(Plot2Log_Wide)=c("Index", MonthString); ###summary(Plot2Log_Wide)
    Plot2_Px=merge(MapRefPx[ ,c("Index")], Plot2Log_Wide); ###summary(Plot2Log_Px)
    
    map2=
      spplot(Plot2_Px[MonthOrder],
             Cuts=100,
             sp.layout=Sp.Layout,
             colorkey=list(labels=list(labels=Yval, at=Yseq)),
             main=Plot2_Title,
             layout=c(4,3)
      ); ###
  } ## end PlotLog
  
  if(PlotLog){
    Results=list(Plot1_Px=Plot1_Px, Map1=map1, Plot2_Px=Plot2_Px, Map2=map2)
  } else {
    Results=list(Plot1_Px=Plot1_Px, Map1=map1)
  }
  return(Results)
} ## end function


############################################################################
## function for removing unneeded objects
CleanUp=function(InputList, Verbose=FALSE){
  for(i in 1:length(InputList)){
    ObjName=InputList[i]; 
    if(Verbose){print(ObjName); print(exists(ObjName))};
    if(exists(ObjName)){
      rm(list=c(ObjName), envir = globalenv())
    }
  }
}

# X=1; Y=1;
# CleanUp(
#   InputList=c("X", "Y")
# )
# X


############################################################################


