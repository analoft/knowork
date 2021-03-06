#install.packages("tm") # only need to do once
args <- commandArgs(trailingOnly = TRUE)
# list.of.packages <- c("tm","pdftools","data.table","plyr","textreadr","stopwords")
# 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages))
#   install.packages(new.packages)
# invisible(sapply(list.of.packages, require, character.only=TRUE))

invisible(library("tm",quietly = T,warn.conflicts = F))
invisible(library("pdftools",quietly = T,warn.conflicts = F))
invisible(library("data.table",quietly = T,warn.conflicts = F))
invisible(library("plyr",quietly = T,warn.conflicts = F))
invisible(library("textreadr",quietly = T,warn.conflicts = F))
invisible(library("stopwords",quietly = T,warn.conflicts = F))
invisible(library("stringr",quietly = T,warn.conflicts = F))
invisible(library("jsonlite",quietly = T,warn.conflicts = F))
# files <- list.files(path = "Resume-Parser/",pattern = "pdf$")
# files_doc <- list.files(path = "Resume-Parser/",pattern = "doc$")
# files_docx <- list.files(path = "Resume-Parser/",pattern = "docx$")


#########################################################################
#########################################################################
#finder function
# 
# ""
load("/var/www/html/rscript/knowork/data.RData")
stp<-stopwords::stopwords(language = "en", source = "smart")
stp<-stp[! stp %in% letters ]
tp<-names(notc)
#print(args)
if(!is.null(args)){
  if(grepl(pattern = "pdf$",x = args[1])){
    files<-args[1]
    Rpdf <- readPDF(control = list(text = "-layout"))
    
    read_pdf<-function(file_n){
      txt <- pdf_text(file_n)
      txt<-paste(unlist(txt), sep="", collapse="\n")
      txt
    }  
    
    x<-read_pdf(files)
  }else{
    x<-textreadr::read_document(args[1])
    x<-paste0(x,collapse = ",")
  }
  
  # paste(paste0(col_n,"<-","unlist(lapply(file_content,function(x){tx<-mark_corpus(x)
  #     txx<-strsplit(x = tx,split = '\\*\\*\\*')
  #              txx<-unlist(txx)
  #              ",col_n,"_finder", "}))"),collapse = ";")
  mark_content<-mark_corpus(x)
  txx<-strsplit(x = mark_content,split = '\\*\\*\\*')
  txx<-unlist(txx)
  
  work_exp<-  work_exp_finder(txx)
  summary<- summary_finder(txx)
  skills<-skills_finder(txx)
  education<-education_finder(txx)
  accomplishments<-accomplishments_finder(txx)
  
  text1<- paste(education,work_exp,summary,accomplishments)
  if(!(is.null(skills)|skills=="")){
    text<-removeWords(tolower(skills),stp)
  }else {text<-removeWords(tolower(text1),stp)}
  
  
  top_lst<-list()
  j<-1
  
  for(i in tp){
    if(grepl(paste0(" ",tolower(i)," "),(text),fixed = T) |grepl(paste0(" ",tolower(i),","),(text),fixed = T)){
      
      top_lst[[j]]<-i
      j<-j+1
    }
    
    top_lst<-unlist(top_lst)  
  }
  
  top_lst<-unlist(top_lst) 
  
  if(is.null(top_lst)){
    input<-"dummy"
  }else{ input<-top_lst
  }
  
  
skills<-names(notc)

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



}









