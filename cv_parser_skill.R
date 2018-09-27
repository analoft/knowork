#install.packages("tm") # only need to do once
args <- commandArgs(trailingOnly = TRUE)
list.of.packages <- c("tm","pdftools","data.table","plyr","textreadr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))
  install.packages(new.packages)
invisible(sapply(list.of.packages, require, character.only=TRUE))



# files <- list.files(path = "Resume-Parser/",pattern = "pdf$")
# files_doc <- list.files(path = "Resume-Parser/",pattern = "doc$")
# files_docx <- list.files(path = "Resume-Parser/",pattern = "docx$")


#########################################################################
#########################################################################
#finder function



email_finder<-function(corpous){
  emails = unlist(regmatches(corpous, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", corpous)))
  tmp<-unique(unlist( emails))
  
  if(identical( character(0),tmp)){
    tmp<-"not available"
  } else {
    tmp<-paste(tmp,collapse = ",")
    
  }
  
  tmp[1]
}


phone_finder<-function(corpous){
  strings.cleaned = gsub("[- .)(+]|[a-zA-Z]*:?","", corpous)
  ph_num<-str_extract_all(strings.cleaned,"(?:(?:\\+|0{0,2})91(\\s*[\ -]\\s*)?|[0]?)?[789]\\d{9}|(\\d[ -]?){10}\\d$")
  ph_num<-unlist(ph_num)
  
  if(identical( character(0),(ph_num))){
    ph_num<-"not available"
  } else {
    ph_num<-paste(ph_num,collapse = ",")
    
  }
  #strings.cleaned = strings.cleaned[-grep("/", strings.cleaned)]
  
  
  #grep("^(?:(?:\\+|0{0,2})91(\\s*[\ -]\\s*)?|[0]?)?[789]\\d{9}|(\\d[ -]?){10}\\d$",typ, value = TRUE)
  # Split each phone number with a "/" into two phone numbers
  # special.cases = unlist(lapply(strsplit(special.cases, "/"), 
  #                               function(x) {
  #                                 c(x[1], 
  #                                   paste0(substr(x[1], 1, nchar(x[1]) - nchar(x[2])), x[2]))
  #                               }))
  # strings.cleaned = c(strings.cleaned, special.cases)
  # 
  # # Select last 8 digits from each phone number
  # phone.nums = as.numeric(substr(strings.cleaned, nchar(strings.cleaned) - 7, 
  #                                nchar(strings.cleaned)))
  # 
  # 
  ph_num[1]
}

mark_corpus<-function(txt){
  dt<-read.csv("/var/www/html/rscript/knowork/rules/summary.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO Summary found")
  }else{
    for(i in ind){txt<-sub(dt[,1][i],"***summary",txt)
    }}
  
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/accomplishments.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO accomplishment found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***accomplishments",txt)
    }}
  
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/awards.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO awards found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***awards",txt)
    }}
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/credibility.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO credibility found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***credibility",txt)
    }
  }
  dt<-read.csv("/var/www/html/rscript/knowork/rules/education.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO education found")
  } else {
    for(i in ind){txt<-sub(dt[,1][i],"***education",txt)
    }}
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/extracurricular.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO extra curricular found")
  } else {
    for(i in ind){txt<-sub(dt[,1][i],"***extracurricular",txt)
    }}
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/misc.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO misc found")
  } else {
    for(i in ind){txt<-sub(dt[,1][i],"***misc",txt)
    }}
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/skills.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO Skills found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***skills",txt)
    }}
  
  dt<-read.csv("/var/www/html/rscript/knowork/rules/work_exp.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO experience found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***work_exp",txt)
    }}
  
  txt
}

col_n<-c("work_exp","summary","skills","misc","extracurricular","education","credibility","awards","accomplishments")





summary_finder<-function(txx){
  ind<-grep("^summary",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out)
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}


education_finder<-function(txx){
  ind<-grep("^education",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out) 
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}


accomplishments_finder<-function(txx){
  ind<-grep("^accomplishments",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out) 
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
  
  
}


awards_finder<-function(txx){
  ind<-grep("^awards",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out) 
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}


credibility_finder<-function(txx){
  ind<-grep("^credibility",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out) 
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}

skills_finder<-function(txx){
  ind<-grep("^skills",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out)
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}

work_exp_finder<-function(txx){
  ind<-grep("^work_exp",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out) 
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}

misc_finder<-function(txx){
  ind<-grep("^misc",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out)
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}



extracurricular_finder<-function(txx){
  ind<-grep("^extracurricular",txx)
  out<-list()
  for(i in ind){
    tmp<-txx[i]
    tp<- unlist(strsplit(tmp,"\r\n"))
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out)
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}

load("/var/www/html/rscript/knowork/data.RData")
stp<-stopwords::stopwords(language = "en", source = "smart")
stp<-stp[! stp %in% letters ]
tp<-names(notc)

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
  if(!is.null(skills)){
    text<-removeWords(tolower(skills),stp)
  }else {text<-removeWords(tolower(text1),stp)}
  
  
  top_lst<-list()
  j<-1
  
  for(i in tp){
    if(grepl(paste0(" ",tolower(i)),(text),fixed = T)){
      
      top_lst[[j]]<-i
      j<-j+1
    }
    
    top_lst<-unlist(top_lst)  
  }
  
  top_lst<-unlist(top_lst) 

if(is.null(top_lst)){
  rtn<-"dummy"
  cat(sprintf("%s",jsonlite::toJSON(rtn)))
}else{ rtn<-top_lst
cat(sprintf("%s",jsonlite::toJSON(rtn)))}
}