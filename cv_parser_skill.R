#install.packages("tm") # only need to do once
args <- commandArgs(trailingOnly = TRUE)
list.of.packages <- c("tm","pdftools","data.table","plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))
  install.packages(new.packages)
sapply(list.of.packages, require, character.only=TRUE)



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
  dt<-read.csv("rules/summary.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO Summary found")
  }else{
    for(i in ind){txt<-sub(dt[,1][i],"***summary",txt)
    }}
  
  
  dt<-read.csv("rules/accomplishments.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO accomplishment found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***accomplishments",txt)
    }}
  
  
  dt<-read.csv("rules/awards.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO awards found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***awards",txt)
    }}
  
  dt<-read.csv("rules/credibility.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO credibility found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***credibility",txt)
    }
  }
  dt<-read.csv("rules/education.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO education found")
  } else {
    for(i in ind){txt<-sub(dt[,1][i],"***education",txt)
    }}
  
  dt<-read.csv("rules/extracurricular.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO extra curricular found")
  } else {
    for(i in ind){txt<-sub(dt[,1][i],"***extracurricular",txt)
    }}
  
  dt<-read.csv("rules/misc.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO misc found")
  } else {
    for(i in ind){txt<-sub(dt[,1][i],"***misc",txt)
    }}
  
  dt<-read.csv("rules/skills.csv")
  sapply(dt[,1],function(x){grep(x,txt)})->chk
  which(unlist(lapply(chk,function(x){identical(x, integer(0))}))!=T)->ind
  if(identical(ind,integer(0))==T){
    print("NO Skills found")
  } else {
    for(i in ind){
      txt<-sub(dt[,1][i],"***skills",txt)
    }}
  
  dt<-read.csv("rules/work_exp.csv")
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
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
    tp<- unlist(strsplit(tmp,"\r\n"))[-1]
    out<-c(out,sub("\\s",":",paste(gsub("^\\s+|\\s+$", "", tp),collapse=",")))
  }
  
  out<- unlist(out)
  out<-out[!duplicated(out)]
  paste(out,collapse = ",")
}

load("data.RData")
tp<-names(notc)
if(grep(pattern = "pdf$",x = args[1])>0){
  files<-args[1]
  Rpdf <- readPDF(control = list(text = "-layout"))
  
  read_pdf<-function(file_n){
    txt <- pdf_text(file_n)
    txt<-paste(unlist(txt), sep="", collapse="\n")
    txt
  }  
  
  file_content<-read_pdf(files)
  x<-file_content
  
  # paste(paste0(col_n,"<-","unlist(lapply(file_content,function(x){tx<-mark_corpus(x)
  #     txx<-strsplit(x = tx,split = '\\*\\*\\*')
  #              txx<-unlist(txx)
  #              ",col_n,"_finder", "}))"),collapse = ";")
  
  work_exp<-{tx<-mark_corpus(x)
             txx<-strsplit(x = tx,split = '\\*\\*\\*')
             txx<-unlist(txx)
             work_exp_finder(txx)};
  
  summary<-{tx<-mark_corpus(x)
  txx<-strsplit(x = tx,split = '\\*\\*\\*')
  txx<-unlist(txx)
  summary_finder(txx)};
  
  skills<-{tx<-mark_corpus(x)
  txx<-strsplit(x = tx,split = '\\*\\*\\*')
  txx<-unlist(txx)
  skills_finder(txx)};
  
  
  education<-{tx<-mark_corpus(x)
  txx<-strsplit(x = tx,split = '\\*\\*\\*')
  txx<-unlist(txx)
  education_finder(txx)};
  
  accomplishments<-{tx<-mark_corpus(x)
  txx<-strsplit(x = tx,split = '\\*\\*\\*')
  txx<-unlist(txx)
  accomplishments_finder(txx)}
  
  text<- paste(education,work_exp,summary,skills,accomplishments)
  
  top_lst<-list()
  j<-1
  for(i in tp){
    if(tolower(i) %in% tolower(text)){
      
      top_lst[[j]]<-i
      j<-j+1
    }
    
    top_lst<-unlist(top_lst)  
  }
  
  if(is.null(top_lst)){
    rtn<-"Can not find skills"
    cat(sprintf("%s",rtn))
  }else{ rtn<-top_lst
  cat(sprintf("%s",paste0(rtn,collapse = ",")))}
  
  
}
