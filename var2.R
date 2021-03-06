args <- commandArgs(trailingOnly = TRUE)

load("/var/www/html/rscript/knowork/data.RData")
invisible(library("plyr",quietly = T,warn.conflicts = F))
invisible(library("textreadr",quietly = T,warn.conflicts = F))
invisible(library("stopwords",quietly = T,warn.conflicts = F))
invisible(library("stringr",quietly = T,warn.conflicts = F))
invisible(library("jsonlite",quietly = T,warn.conflicts = F))

skills<-names(notc)

# API calls will begin at the algorithm() method, with the request body passed as 'input'
# For more details, see algorithmia.com/developers/algorithm-development/languages
input <-args
 
  
  if( length(input) > 1){
    top_lst<-list()
    j<-1
    for(i in input){
      if(length(skills[tolower(skills)==tolower(i)]->>rest)==0)
        if(length(skills[grep(tolower(i),tolower(skills),fixed=T)]->>rest)==0){
          rest<-"NULL"
        }
      
      top_lst[[j]]<-rest
      j<-j+1
    }
    
    top_lst<-unlist(top_lst)
  }else{
    if(length(skills[tolower(skills)==tolower(input)]->>rest)==0)
      if(length(skills[grep(tolower(input),tolower(skills),fixed=T)]->>rest)==0){
        rest<-"NULL"
      }
    
    top_lst<-unlist(rest)
  }
  



  if(!is.null(top_lst)){
    # s1<-skills[skills$topic %in% top_lst,]
    # s2<-topskills[,colnames(topskills)[colnames(topskills) %in% top_lst]]
    s1<-llply(notc[top_lst],data.frame)
    
    cat(sprintf("%s",toJSON(s1)))
    
    }else{
      cat(sprintf("%s","no data found"))
      
    }
    
  
  
  
  
  
  
  
  
  
  
  
