library('conover.test')
library("grid")

difftest = function(AllData,GroupResData,financiallist){
  temp1 = GroupResData[,c('Company','Final_Group')]
  Pvalue = matrix('na', 23, length(financiallist),dimnames = list(NULL,financiallist))  
  for (j in 1:length(financiallist)){
    financial = financiallist[j]
    temp2 = AllData[AllData$Earliest>1991,c('Acquirer','Company','Earliest',financial)]
    temp  = na.omit(merge(temp1, temp2, by = 'Company'))
    for (i in 1992:2014){
      if (nrow(temp[temp$Earliest==i,])==0){
        print(paste(i,financial,' NA'))
      }else{
        y = temp[temp$Earliest==i,financial]
        x = factor(temp[temp$Earliest==i,]$Final_Group)
        print(c(i,financial))
      
      ## parametric 
      #aov.res = aov(y~x)
      #print(TukeyHSD(aov.res,conf.level = 0.9))
      
      ## non-parametric, which should be used in our case
        kru.res = kruskal.test(y~x)
        Pvalue[i-1991,j] = kru.res$p.value
      #print(kru.res)
        con.res = try(conover.test::conover.test(y,x),silent = TRUE)
        if (is(con.res, "try-error")) {
          print('NA')
          }else{
            con.res
          } 
        boxplot(y~x,xlab = 'Group',ylab = financial,main = paste('Boxplot of Year',i))
      
      }
    
    }
    grid.newpage()
    par(mfg = c(1,1))
  }

  write.csv(Pvalue,'electronic_Difftest.csv',row.names = seq(1992,2014))
}
  
    
# example
par(mfrow = c(2,2))
AllData = read.csv('electronic.csv',header = T)
GroupResData = read.csv('ELECTRONICS Groupings on Six Patent Factors.csv',header = T)

financiallist = c('ROA','ROS','TOBINQ','PRODTV','RD','SIZE','LOGASSETS','LOGSALES','RDPRODTV0','RDPRODTV1','RDPRODTV2','RDPRODTV3','RDPRODTV4')
sink('CommServ_Difftest_Detail.txt')
Meida_Difftest = difftest(AllData,GroupResData,financiallist)
