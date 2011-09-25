#this script requires the R package "gdata" and a working perl installation

#in many cases perl will be found automatically
perl<-gdata:::findPerl("perl")

#if this line gives an error on your system, remove it and set your perl location manually:
#perl<-"C:\\Perl\\bin\\perl.exe"
#perl<-"G:\\Perl\\bin\\perl.exe"

#install gdata if not done already:
#install.packages("gdata")

#load gdata
library(gdata)

xcaliburxls2R <- function(dir)
{

#list all files in the chosen directory
files<-list.files(path=dir)

#select only files with .XLS or .xls extension
files<-files[grepl( ".XLS", files)|grepl( ".xls", files)]

#for each file...
for (i in 1:length(files))
{

  #count the number of sheets in the file
  sheetnames<-sheetNames(files[i], perl=perl)

  #for each sheet...  
  for (j in 1:length(sheetnames))
  {

    #read the sheet
    tmp<-data.frame(read.xls(files[i], sheet=j, perl=perl))
    
    #read out the substance's name
    tmp.name<-tmp[1,1]
    
    #set limits for data in spreadsheet
    lim<-4:nrow(tmp)-3
    
    #read names of samples
    samplenames<-tmp[lim,1]
    
    #read peak areas 
    tmp.area<-data.frame(tmp[4:lim,5])
    
    #set sample names as rownames and the substance's name as colname
    rownames(tmp.area)<-samplenames
    colnames(tmp.area)<-tmp.name
    
    #repeat the same with retention times
    tmp.rt<-data.frame(tmp[4:lim,15])
    rownames(tmp.rt)<-samplenames
    colnames(tmp.rt)<-tmp.name
    
    #if this is the first run, create new variables
    if(i==1 & j==1)
    {
      area<-tmp.area
      rt<-tmp.rt
    } else
    {
#to be implemented: check if sample names are identical. if not, create a list of dataframes with identical sample names.
      
      #check if sample names are identical
#      if (is.list(area)==F) {
#        if (colnames(area)==colnames(tmp.area))
#          area<-cbind(area, tmp.area)
#      } #else {
#        if (length(which(colnames(area)==colnames(tmp.area))))
#      }
      
      #until then we just run...
      area<-cbind(area, tmp.area)
      rt<-cbind(rt, tmp.rt)
    }
  }  
}
return(list(rt, area))
}