#wallet
########################################################
########################################################
wall$CLass<-"Digital wallet"
wall$SubCategory<-unlist(lapply(wall$sms,subcategoryFinderWall))

wall$DebitCredit<-as.numeric(unlist(lapply( wall$sms,Wmoney1Finder)))
wall$Balance<-as.numeric(unlist(lapply( wall$sms,Wmoney2Finder)))
wall$Type_Transaction<-ifelse(wall$SubCategory %in% c("Payment","Merchant payment"),"Debit","Credit")
wall$Type_Transaction[wall$SubCategory=="other"]<-"Not Available"
wall$wallName<-unlist(lapply(wall$sms,walletName))
wall$DebitCredit[!wall$SubCategory %in% c("Payment","Wallet Topup","Merchant payment","cashback","Wallet Recharge")]<-NA
wall$Balance[!wall$SubCategory %in% c("Payment","Wallet Topup","Merchant payment","cashback","Wallet Recharge")]<-NA


wall$Type_Transaction[wall$wallName=="Paytm"][unlist(lapply(wall$sms[wall$wallName=="Paytm"],pytm_ck))]<-"Credit"
write.csv(wall,"output/walletClass.csv")
########################################################
########################################################
#balance sheet 

stm<-wall[wall$SubCategory %in% c("Payment","Wallet Topup","Merchant payment","cashback","Wallet Recharge"),c("full_name","mobile_no","time","CLass","SubCategory","wallName", "DebitCredit","Balance","Type_Transaction" )]

nam<-unique(stm$full_name)
fol<-paste0(dirName,"/balanceSheet")
dir.create(fol)
fol<-paste0(dirName,"/balanceSheet/wallet")
dir.create(fol)
#dir("balanceSheet/combined")
#fol<-"balanceSheet/combined"
#library(r2excel)
biglist<-list()
runner1<-1

for(k in nam){
  dt<-stm[stm$full_name == k,]
  dt$wallName[is.na(dt$wallName)]<-"Unknown"
  #dt$wallName<-unlist(lapply(dt$wallName,clean_wallName))
  
  fol1<-paste0(fol,"/",gsub(" ","_",dt$full_name[1]))
  dir.create(fol1)
  dt<-dt[!(is.na(dt$Balance)& is.na(dt$DebitCredit)),]
  #dt<-dt[!(is.na(dt$DebitCredit)&dt$SubCategory %in% c("Debit","Credit","Debit Card")),]
  
  wallName<-unique(dt$wallName)
  semibig<-list()
  runner2<-1
  for(l in wallName){
    dtt<-dt[dt$wallName==l,c("time","SubCategory","DebitCredit","Balance","Type_Transaction")]
    filename<-paste0(fol1,"/",l,".csv")
    header<-paste0("wallName Holder     ",k,":--",l)
    dtt<- dtt[ order(as.POSIXct(dtt$time),decreasing = F),]
    
    dtt$DebitCredit<-as.numeric(dtt$DebitCredit)
    dtt$Balance<-as.numeric(dtt$Balance)
    
    
    if(nrow(dtt)!=1){
      pop<-1
      if(nrow(dtt)>100){
        1:nrow(dtt)%/% 100 ->spl
        dttS<-list()
        po<-1
        for(g in unique(spl)){
          which(spl==g)->ind
          dttM<-balance_w(dtt[ind,])
          dttS[[po]]<-dttM
        }
        dtt<-ldply(dttS,data.frame)
      }
      dtt<-balance_w(dtt)
      dtt<-correct_bal_w(dtt)
      dtt<-correct_date_w(dtt)}
    rt<-dtt
    rt$full_name<-k
    rt$wallName<-l
    semibig[[runner2]]<-rt
    runner2<-runner2+1
    #create_sheet(filename,sheader,dtt)
    write.csv(dtt,file = filename)
  }
  biglist[[runner1]]<-ldply(semibig,data.frame)
  runner1<-runner1+1
  
}

bk<-ldply(biglist,data.frame)

bk$DebitCredit<-as.numeric(bk$DebitCredit)
bk$Balance<-as.numeric(bk$Balance)
un_user<-unique(wall[wall$SubCategory %in% c("Merchant payment", "cashback","Wallet Topup","Payment" ),c("full_name","mobile_no")])
bkk<-merge(bk,un_user,by="full_name",all.x = T)
yn_user<-unique(bkk[,c("full_name","mobile_no")])

if(is.na(args[3])){
  for(x in 1:nrow(yn_user)){
    nam<-yn_user[x,"full_name"]
    number<-yn_user[x,"mobile_no"]
    
    wall1<-bkk[bkk$full_name==nam ,]
    wall1<-wall1[wall1$mobile_no==number ,]
    #wall1<-wall1[wall1$SubCategory %in% c("Debit Card","Credit","Debit","Balance"),]
    
    if(nrow(wall1)!=0){
      wall1$time<-as.POSIXct(wall1$time)
      wall1$Index<- strftime(wall1$time, "%b-%Y")
      wall1$Index_day<- strftime(wall1$time, "%d")
      wall1$Balance<-as.numeric(wall1$Balance)
      
      # Balance related	Average daily balance (for a month)
      # Minimum balance (for a month)
      # Maximum balance (for a month)
      wall1<-wall1[order(as.POSIXct(wall1$time)),]
      wall1$avgB<-0
      for(p in unique(wall1$Index)){
        b1<-wall1[wall1$Index==p,]
        for(w in unique(b1$Index_day)){
          b2<-b1[b1$Index_day==w,]
          
          if(nrow(b2)==1){
            wall1[wall1$Index==p &wall1$Index_day==w,]$avgB<-b2$Balance
          }else{
            wall1[wall1$Index==p &wall1$Index_day==w,]$avgB<-mean(b2$Balance)
          }}
        
        b1<-wall1[wall1$Index==p,]
        ind<-diff(as.numeric(b1$Index_day))
        ind<-c(as.numeric(b1$Index_day[1]),ind)
        wall1[wall1$Index==p,]$avgB<-b1$Balance*ind
      }
      
      if(sum(wall1$Balance,na.rm = T)!=0){
        avg_bal<-aggregate( avgB ~ Index, wall1, sum,na.rm = T)
        avg_bal$avgB<-avg_bal$avgB/30
        colnames(avg_bal)<-c("Index","Average.Balance(30)")
        
        ratio_fre<-plyr::count(wall1,Index~SubCategory)
        ratio_fre<-dcast(ratio_fre,Index~SubCategory)
        cln<-c("Merchant payment", "cashback","Wallet Topup","Payment" )
        cln<-colnames(ratio_fre)[colnames(ratio_fre) %in% cln]
        
        ratio_fre$total_field<-rowSums(ratio_fre[,cln,drop=F],na.rm = T)
        
        cln<-"otherTransaction"
        cln<-colnames(ratio_fre)[colnames(ratio_fre) %in% cln]
        
        ratio_fre$generated<-rowSums(ratio_fre[,cln,drop=F],na.rm = T)
        ratio_fre$Missing_row_ratio<-ratio_fre$generated/ratio_fre$total_field
        ratio_fre<-ratio_fre[,c("Index","Missing_row_ratio")]
        
        # if(wall1$SubCategory %in% "wallName Balance"){
        #   wall1$DebitCredit<-wall1$Balance
        # }
        ratio_val<-aggregate(DebitCredit~Index+SubCategory,data=wall1,FUN = sum,na.rm =T)
        ratio_val<-dcast(ratio_val,Index~SubCategory)
        
        cln<-c("Merchant payment", "cashback","Wallet Topup","Payment" )
        cln<-colnames(ratio_val)[colnames(ratio_val) %in% cln]
        
        ratio_val$total_field<-rowSums(ratio_val[,cln,drop=F],na.rm = T)
        
        cln<-"otherTransaction"
        cln<-colnames(ratio_val)[colnames(ratio_val) %in% cln]
        
        ratio_val$generated<-rowSums(ratio_val[,cln,drop=F],na.rm = T)
        ratio_val$Missing_amount_ratio<-ratio_val$generated/ratio_val$total_field
        ratio_val<-ratio_val[,c("Index","Missing_amount_ratio")]
        ratio_val$Missing_amount_ratio<-ifelse(ratio_val$Missing_amount_ratio==Inf |is.nan(ratio_val$Missing_amount_ratio),0,ratio_val$Missing_amount_ratio)
        
        min_bal<-aggregate( Balance ~ Index, wall1, base::min,na.rm = T)
        colnames(min_bal)<-c("Index","Min.Balance")
        
        max_bal<-aggregate( Balance ~ Index, wall1, base::max,na.rm = T)
        colnames(max_bal)<-c("Index","Max.Balance")
      }else{
        avg_bal<-   data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        
        avg_bal<-data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(avg_bal)<-c("Index","Average.Balance(30)")
        
        min_bal<-data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(min_bal)<-c("Index","Min.Balance")
        
        max_bal<-data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(max_bal)<-c("Index","Max.Balance")
        
        ratio_fre<-data.frame("Index"<-unique(wall1$Index),"ratio"<-0)
        ratio_val<-data.frame("Index"<-unique(wall1$Index),"ratio"<-0)
        colnames(ratio_fre)<-c("Index","Missing_amount_ratio")
        colnames(ratio_val)<-c("Index","Missing_amount_ratio")
        
      }
      # Transactions	Salary amount (if >1 amount then measure of degree of variability)
      # Amount of credit transactions in a month
      # Number of credit transactions in a month
      # Average size and spread of credit transactions in a month
      # Amount/number/average/spread of debit and credit transactions by category
      CV <- function(sal){
        if(length(sal)>1){
          salary<-sum(sal)
          vari<-sd(sal)/mean(sal)*100
          
        }else{
          salary<-sal
          vari<-"not applicable"
          
        }
        return(paste0(salary,"#",vari))
      }
      
      
      # Amount of debit transactions in a month
      # Number of debit transactions in a month
      # Average size and spread of debit transactions in a month
      tmp<-bkk[bkk$Type_Transaction %in%"Debit" & bkk$full_name %in% nam &bkk$mobile_no%in%number,]
      
      freq<-function(vec){
        vec<-vec[!is.na(vec)]
        return(length(vec))
      }
      
      if(nrow(tmp)!=0){
        tmp$time<-as.POSIXct(tmp$time)
        tmp$Index<- strftime(tmp$time, "%b-%Y")
        
        tot_debit<- aggregate( DebitCredit ~ Index, tmp,sum)
        colnames(tot_debit)<-c("Index","Total.Debit")
        
        avg_debit<- aggregate( DebitCredit ~ Index, tmp,mean)
        colnames(avg_debit)<-c("Index","Average.Debit")
        
        count_debit<- aggregate( DebitCredit ~ Index, tmp,freq)
        colnames(count_debit)<-c("Index","Frequency.Debit")
        
        
      }else{
        tot_debit<-   data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(tot_debit)<-c("Index","Total.Debit")
        
        avg_debit<- data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(avg_debit)<-c("Index","Average.Debit")
        
        count_debit<- data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(count_debit)<-c("Index","Frequency.Debit")
        
      }
      
      
      ####################################
      tmp<-bkk[bkk$Type_Transaction %in%"Credit" & bkk$full_name %in% nam &bkk$mobile_no%in%number,]
      
      
      if(nrow(tmp)!=0){
        tmp$time<-as.POSIXct(tmp$time)
        tmp$Index<- strftime(tmp$time, "%b-%Y")
        
        tot_Credit<- aggregate( DebitCredit ~ Index, tmp,sum)
        colnames(tot_Credit)<-c("Index","Total.Credit")
        
        avg_Credit<- aggregate( DebitCredit ~ Index, tmp,mean)
        colnames(avg_Credit)<-c("Index","Average.Credit")
        
        count_Credit<- aggregate( DebitCredit ~ Index, tmp,freq)
        colnames(count_Credit)<-c("Index","Frequency.Credit")
        
        
      }else{
        tot_Credit<-   data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(tot_Credit)<-c("Index","Total.Credit")
        
        avg_Credit<- data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(avg_Credit)<-c("Index","Average.Credit")
        
        count_Credit<- data.frame("Index"<-unique(wall1$Index),"Salary"<-0)
        colnames(count_Credit)<-c("Index","Frequency.Credit")
        
      }
      
      
      # # of wallNames	Number of bank wallNames
      # Measure of degree of activity in wallName based on number of transactions
      # 
      # ATM withdrawals	Number of ATM withdrawals per month
      # Average amount and spread of ATM withdrawals per month
      # 
      
      bank_acc<-unique(wall1$wallName)
      #tmp<-wall1[wall1$SubCategory=="Debit Card" & !is.na(wall1$SubCategory),]
      
      
      out<-  merge(avg_bal,merge(min_bal,merge(max_bal,merge(tot_debit,merge(avg_debit,merge(count_debit,merge(tot_Credit,merge(avg_Credit,merge(count_Credit,merge(ratio_fre,ratio_val))))))))))
      out<- out[order(as.Date(paste("01-", out$Index, sep = ""), format = "%d-%b-%y")),]
      
      wb <- createWorkbook(type="xlsx")
      sheet <- createSheet(wb, sheetName = paste0("Report_",paste(nam,collapse = "_")))
      # add iris data using default settings
      xlsx.addHeader(wb, sheet, 
                     value=("Wallet Matrix "),level = 1)
      
      xlsx.addLineBreak(sheet, 2)
      xlsx.addHeader(wb, sheet, 
                     value=paste0("Name : ",nam),level = 3)
      xlsx.addHeader(wb, sheet, 
                     value=paste0("Nnumber: ",number),level = 3)
      xlsx.addLineBreak(sheet, 2)
      xlsx.addHeader(wb, sheet, 
                     value=paste0("Nnumber of wallet: ",freq(bank_acc)),level = 3)
      
      xlsx.addTable(wb, sheet, out)
      saveWorkbook(wb, paste0(dirName,"/report/bank/Report_",gsub(" ","_",nam),".xlsx"))
      
      print(x)}else{print("miss")}
  }
  
}
##############################################################################
##############################################################################
if(!is.na(args[3])){
  
  for(x in 1:nrow(yn_user)){
    nam<-yn_user[x,"full_name"]
    number<-yn_user[x,"mobile_no"]
    
    wall1<-bkk[bkk$full_name==nam ,]
    wall1<-wall1[wall1$mobile_no==number ,]
    #wall1<-wall1[wall1$SubCategory %in% c("Debit Card","Credit","Debit","Balance"),]
    acc<-unique(wall1$wallName)
    dir.create( paste0(dirName,"/report/wallet/",gsub(" ","_",nam)))
    for(r in acc){
      wall2<-wall1[wall1$wallName==r,]
      
      
      if(nrow(wall2)!=0){
        wall2$time<-as.POSIXct(wall2$time)
        wall2$Index<- strftime(wall2$time, "%b-%Y")
        wall2$Index_day<- strftime(wall2$time, "%d")
        wall2$Balance<-as.numeric(wall2$Balance)
        
        # Balance related	Average daily balance (for a month)
        # Minimum balance (for a month)
        # Maximum balance (for a month)
        wall2<-wall2[order(as.POSIXct(wall2$time)),]
        wall2$avgB<-0
        for(p in unique(wall2$Index)){
          b1<-wall2[wall2$Index==p,]
          for(w in unique(b1$Index_day)){
            b2<-b1[b1$Index_day==w,]
            
            if(nrow(b2)==1){
              wall2[wall2$Index==p &wall2$Index_day==w,]$avgB<-b2$Balance
            }else{
              wall2[wall2$Index==p &wall2$Index_day==w,]$avgB<-mean(b2$Balance)
            }}
          
          b1<-wall2[wall2$Index==p,]
          ind<-diff(as.numeric(b1$Index_day))
          ind<-c(as.numeric(b1$Index_day[1]),ind)
          wall2[wall2$Index==p,]$avgB<-b1$Balance*ind
        }
        
        if(sum(wall2$Balance,na.rm = T)!=0){
          avg_bal<-aggregate( avgB ~ Index, wall2, sum,na.rm = T)
          avg_bal$avgB<-avg_bal$avgB/30
          colnames(avg_bal)<-c("Index","Average.Balance(30)")
          
          ratio_fre<-plyr::count(wall2,Index~SubCategory)
          ratio_fre<-dcast(ratio_fre,Index~SubCategory)
          cln<-c("Credit","Debit", "Debit Card","wallName Balance")
          cln<-colnames(ratio_fre)[colnames(ratio_fre) %in% cln]
          
          ratio_fre$total_field<-rowSums(ratio_fre[,cln,drop=F],na.rm = T)
          
          cln<-c("otherCredit", "otherDebit")
          cln<-colnames(ratio_fre)[colnames(ratio_fre) %in% cln]
          
          ratio_fre$generated<-rowSums(ratio_fre[,cln,drop=F],na.rm = T)
          ratio_fre$Missing_row_ratio<-ratio_fre$generated/ratio_fre$total_field
          ratio_fre<-ratio_fre[,c("Index","Missing_row_ratio")]
          
          if(wall2$SubCategory %in% "wallName Balance"){
            wall2$DebitCredit<-wall2$Balance
          }
          ratio_val<-aggregate(DebitCredit~Index+SubCategory,data=wall2,FUN = sum,na.rm =T)
          ratio_val<-dcast(ratio_val,Index~SubCategory)
          
          cln<-c("Credit","Debit", "Debit Card")
          cln<-colnames(ratio_val)[colnames(ratio_val) %in% cln]
          
          ratio_val$total_field<-rowSums(ratio_val[,cln,drop=F],na.rm = T)
          
          cln<-c("otherCredit", "otherDebit")
          cln<-colnames(ratio_val)[colnames(ratio_val) %in% cln]
          
          ratio_val$generated<-rowSums(ratio_val[,cln,drop=F],na.rm = T)
          ratio_val$Missing_amount_ratio<-ratio_val$generated/ratio_val$total_field
          ratio_val<-ratio_val[,c("Index","Missing_amount_ratio")]
          ratio_val$Missing_amount_ratio<-ifelse(ratio_val$Missing_amount_ratio==Inf |is.nan(ratio_val$Missing_amount_ratio),0,ratio_val$Missing_amount_ratio)
          
          min_bal<-aggregate( Balance ~ Index, wall2, min,na.rm = T)
          colnames(min_bal)<-c("Index","Min.Balance")
          
          max_bal<-aggregate( Balance ~ Index, wall2, max,na.rm = T)
          colnames(max_bal)<-c("Index","Max.Balance")
        }else{
          avg_bal<-   data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          
          avg_bal<-data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(avg_bal)<-c("Index","Average.Balance(30)")
          
          min_bal<-data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(min_bal)<-c("Index","Min.Balance")
          
          max_bal<-data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(max_bal)<-c("Index","Max.Balance")
          
          ratio_fre<-data.frame("Index"<-unique(wall2$Index),"ratio"<-0)
          ratio_val<-data.frame("Index"<-unique(wall2$Index),"ratio"<-0)
          colnames(ratio_fre)<-c("Index","Missing_amount_ratio")
          colnames(ratio_val)<-c("Index","Missing_amount_ratio")
          
        }
        # Transactions	Salary amount (if >1 amount then measure of degree of variability)
        # Amount of credit transactions in a month
        # Number of credit transactions in a month
        # Average size and spread of credit transactions in a month
        # Amount/number/average/spread of debit and credit transactions by category
        CV <- function(sal){
          if(length(sal)>1){
            salary<-sum(sal)
            vari<-sd(sal)/mean(sal)*100
            
          }else{
            salary<-sal
            vari<-"not applicable"
            
          }
          return(paste0(salary,"#",vari))
        }
        
        tmp<-wall[wall$FunctionType%in%"Salary" & wall$SubCategory %in%"Credit" & wall$full_name %in% nam &wall$mobile_no%in%number,]
        
        if(nrow(tmp)!=0){
          tmp$time<-as.POSIXct(tmp$time)
          tmp$Index<- strftime(tmp$time, "%b-%Y")
          
          tp<- aggregate( DebitCredit ~ Index, tmp, CV)
          listHolder<-strsplit(tp$DebitCredit,"#")
          dd  <-  as.data.frame(matrix(unlist(listHolder), ncol=length(unlist(listHolder[1]))))
          tp<-cbind(tp$Index,dd)
          colnames(tp)<-c("Index","Salary","Variability")
        }else{
          tp<-   data.frame("Index"<-unique(wall2$Index),"Salary"<-0,"Variability"<-"Not applicable")
          colnames(tp)<-c("Index","Salary","Variability")
          
        }
        
        # Amount of debit transactions in a month
        # Number of debit transactions in a month
        # Average size and spread of debit transactions in a month
        tmp<-wall[wall$SubCategory %in%"Debit" & wall$full_name %in% nam &wall$mobile_no%in%number,]
        
        freq<-function(vec){
          vec<-vec[!is.na(vec)]
          return(length(vec))
        }
        
        if(nrow(tmp)!=0){
          tmp$time<-as.POSIXct(tmp$time)
          tmp$Index<- strftime(tmp$time, "%b-%Y")
          
          tot_debit<- aggregate( DebitCredit ~ Index, tmp,sum)
          colnames(tot_debit)<-c("Index","Total.Debit")
          
          avg_debit<- aggregate( DebitCredit ~ Index, tmp,mean)
          colnames(avg_debit)<-c("Index","Average.Debit")
          
          count_debit<- aggregate( DebitCredit ~ Index, tmp,freq)
          colnames(count_debit)<-c("Index","Frequency.Debit")
          
          
        }else{
          tot_debit<-   data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(tot_debit)<-c("Index","Total.Debit")
          
          avg_debit<- data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(avg_debit)<-c("Index","Average.Debit")
          
          count_debit<- data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(count_debit)<-c("Index","Frequency.Debit")
          
        }
        
        
        ####################################
        tmp<-wall[wall$SubCategory %in%"Credit" & wall$full_name %in% nam &wall$mobile_no%in%number,]
        
        
        if(nrow(tmp)!=0){
          tmp$time<-as.POSIXct(tmp$time)
          tmp$Index<- strftime(tmp$time, "%b-%Y")
          
          tot_Credit<- aggregate( DebitCredit ~ Index, tmp,sum)
          colnames(tot_Credit)<-c("Index","Total.Credit")
          
          avg_Credit<- aggregate( DebitCredit ~ Index, tmp,mean)
          colnames(avg_Credit)<-c("Index","Average.Credit")
          
          count_Credit<- aggregate( DebitCredit ~ Index, tmp,freq)
          colnames(count_Credit)<-c("Index","Frequency.Credit")
          
          
        }else{
          tot_Credit<-   data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(tot_Credit)<-c("Index","Total.Credit")
          
          avg_Credit<- data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(avg_Credit)<-c("Index","Average.Credit")
          
          count_Credit<- data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(count_Credit)<-c("Index","Frequency.Credit")
          
        }
        
        
        # # of wallNames	Number of bank wallNames
        # Measure of degree of activity in wallName based on number of transactions
        # 
        # ATM withdrawals	Number of ATM withdrawals per month
        # Average amount and spread of ATM withdrawals per month
        # 
        
        bank_acc<-unique(wall2$wallName)
        #tmp<-wall2[wall2$SubCategory=="Debit Card" & !is.na(wall2$SubCategory),]
        tmp<-wall[wall$SubCategory %in%"Debit Card" & wall$full_name %in% nam &wall$mobile_no%in%number & wall2$ModeType=="ATM",]
        
        
        if(nrow(tmp)!=0){
          tmp$time<-as.POSIXct(tmp$time)
          tmp$Index<- strftime(tmp$time, "%b-%Y")
          
          count_atm<- aggregate( DebitCredit ~ Index, tmp,freq)
          colnames(count_atm)<-c("Index","Frequency.ATM.Withdrawal")
          
          total_atm<- aggregate( DebitCredit ~ Index, tmp,sum)
          colnames(total_atm)<-c("Index","Total.ATM")
          
          
        }else{
          count_atm<- data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(count_atm)<-c("Index","Frequency.ATM.Withdrawal")
          
          total_atm<- data.frame("Index"<-unique(wall2$Index),"Salary"<-0)
          colnames(total_atm)<-c("Index","Total.ATM")
          
        }
        
        out<-  merge(avg_bal,merge(min_bal,merge(max_bal,merge(tp,merge(tot_debit,merge(avg_debit,merge(count_debit,merge(tot_Credit,merge(avg_Credit,merge(count_Credit,merge(count_atm,merge(total_atm,merge(ratio_fre,ratio_val)))))))))))))
        out<- out[order(as.Date(paste("01-", out$Index, sep = ""), format = "%d-%b-%y")),]
        
        wb <- createWorkbook(type="xlsx")
        sheet <- createSheet(wb, sheetName = paste0("Report_",paste(nam,collapse = "_")))
        # add iris data using default settings
        xlsx.addHeader(wb, sheet, 
                       value=("Bank Matrix "),level = 1)
        
        xlsx.addLineBreak(sheet, 2)
        xlsx.addHeader(wb, sheet, 
                       value=paste0("Name : ",nam),level = 3)
        xlsx.addHeader(wb, sheet, 
                       value=paste0("Nnumber: ",number),level = 3)
        xlsx.addLineBreak(sheet, 2)
        xlsx.addHeader(wb, sheet, 
                       value=paste0("Nnumber of bank wallName: ",freq(bank_acc)),level = 3)
        
        xlsx.addTable(wb, sheet, out)
        
        saveWorkbook(wb, paste0(dirName,"/report/bank/",gsub(" ","_",nam),"/Report_",gsub(" ","_",r),".xlsx"))
        
        print(x)}else{print("miss")}
      
    }
  }
}
