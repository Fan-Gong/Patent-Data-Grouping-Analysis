library(gridGraphics)
library(plm)


#function to decide which model to use(choosing from pooling or within or random effect model)
plm_model = function(plm_data){
  ##data is a panle data frame contains A, C, E, original_Y, original_X, Y, X
  data_pool = plm(Y~X, data = plm_data, model = 'pooling')
  data_fe = plm(Y~X, data = plm_data, model = 'within')
  data_re = plm(Y~X, data = plm_data, model = 'random')
  p1 = pFtest(data_fe, data_pool)
  p2 = phtest(data_re, data_fe)
  if(p1$p.value > 0.05){return('pooling')}
  else{
    if(p2$p.value < 0.05){return('within')}
    else{return('random')}
  }
}

#Calculates the aggregated differences between the slopes of each model
aggregate.slope = function(df){
  ##df is a vector containing three slopes
  ds1 = abs(df[1] - df[2])
  return(ds1)
}

#company grouping function
grouping = function(data, point){
  ##data is a data frame contains A, E, C, original_Y, original_X, Y, final_group
  ##point is a cut point
  company_group = data.frame(acquirer = unique(data$A), company = unique(data$C), num_group1 = NA, num_group2 = NA, group = NA)
  index = 0
  for(j in unique(data$A)){
    index = index + 1
    company_group$num_group1[index] = sum(data[data$A == j,]$X <= point)
    company_group$num_group2[index] = sum(data[data$A == j,]$X > point)
    
    if(company_group$num_group1[index] < company_group$num_group2[index]){
      company_group$group[index] = 2
    }else if(company_group$num_group1[index] == company_group$num_group2[index]){
      company_group$group[index] = 0
    }else if(company_group$num_group1[index] > company_group$num_group2[index]){
      company_group$group[index] = 1
    }
  }
  
  for(i in 1:nrow(data)){
    data$final_group[i] = company_group[company_group$acquirer == data$A[i],]$group 
  }
  
  return(list(part1 = data[data$final_group == 1, -8],
              part2 = data[data$final_group == 2, -8],
              part3 = data[data$final_group == 0, -8],
              data = data,
              company_group = company_group))
  
}


#Main function to find cut point
cutpoint = function(Y, X, A, C, E, n1 = 100){
  ## Y is a column of financial variable in one industry
  ## X is a column of patent information in the same industry
  ## A is a column of the acquirer name
  ## C is a column of company name
  ## E is a column of earliest year
  ##n1 is The number of cut_point
  k = 2 ## number of subgroups
  
  ##Missing data removal
  data = na.omit(data.frame(A, C, E, Y, X))
  
  ##Data scaling
  scaled_data = data.frame(scale(data[,c('Y','X')])) 
  data = data.frame(data, scaled_data)
  colnames(data)[4:7] = c('original_Y', 'original_X', 'Y', 'X')
  data$final_group = NA
  
  ##variable pre-processing
  cut_point = seq(from = min(data$X), to = max(data$X), length.out = n1)
  slope = matrix(NA, ncol = k, nrow = n1) 
  colnames(slope) = c('slope1', 'slope2')
  
  ##Panel data linear model selection
  plm_data = plm.data(data, index = c('C','E'))
  method = plm_model(plm_data)
  
  ##variable calculating 
  for(i in 1:n1){
    print(i)
    point = cut_point[i]
    
    ##grouping calculate
    grouping_result = grouping(data = data, point)
    
    part1 = grouping_result$part1
    part2 = grouping_result$part2
    
    ## in case of no data in every part or each part only has one predictor
    if(nrow(part1) == 0 | length(unique(part1$X)) == 1){
      slope[i,1] = 0
    }else if(length(unique(part1$C)) == 1|length(unique(part1$C)) == length(unique(part1$X))){
      slope[i,1] = lm(Y~X, data = part1)$coefficients['X']
    }else{
      part1 = plm.data(grouping_result$part1, index = c('C', 'E'))
      slope[i,1] = plm(Y~X, data = part1, model = method)$coefficients['X']
    }
    
    if(nrow(part2) == 0 | length(unique(part2$X)) == 1){
      slope[i,2] = 0
    }else if(length(unique(part2$C)) == 1|length(unique(part2$C)) == length(unique(part2$X))){
      slope[i,2] = lm(Y~X, data = part2)$coefficients['X']
    }else{
      part2 = plm.data(grouping_result$part2, index = c('C', 'E')) 
      slope[i,2] = plm(Y~X, data = part2, model = method)$coefficients['X']
    }
  }
  
  ##Use aggregate.slope function to abtain aggregated slopes
  slope_aggre = apply(slope, 1, aggregate.slope)
  
  ##Find which pair of cutpoints lead to the max aggregated slope
  return(list(cutpoints = cut_point[which.max(slope_aggre)],
              aggregated_slope = max(slope_aggre)))
} 

#Function to analyse the subgroup
group_analyze = function(Y, X, A, C, E, point){
  ##data is a data frame contains A, C, original_Y, original_X, Y, X, final_group
  ##point is a cut point
  ##Missing data removal
  data = na.omit(data.frame(A, C, E, Y, X))
  ##Data scaling
  scaled_data = data.frame(scale(data[,c('Y','X')])) 
  data = data.frame(data, scaled_data)
  colnames(data)[4:7] = c('original_Y', 'original_X', 'Y', 'X')
  data$final_group = NA
  
  ##Panel data linear model selection
  plm_data = plm.data(data, index = c('C','E'))
  method_plm = plm_model(plm_data)
  
  result <<- grouping(data, point)
  
  part1 = result$part1
  part2 = result$part2
  part3 = result$part3
  
  if(nrow(part1) == 0 | length(unique(part1$X)) == 1){
    part1_summary = NA
    plot1 = NA
  }else if(length(unique(part1$C)) == 1){
    part1_summary = summary(lm(Y~X, data = part1))
    plot(X, Y, data = part1)
    abline(lm(Y~X, data = part1), col = 'red')
    plot1 = recordPlot()
  }else{
    panel_data1 <<- plm.data(part1, index = c('C', 'E'))
    part1_summary = summary(plm(Y~X, data = panel_data1, model = method_plm))
    plot(plm(Y~X, data = panel_data1, model = method_plm))
    plot1 = recordPlot()
  }
  
  if(nrow(part2) == 0 | length(unique(part2$X)) == 1){
    part2_summary = NA
    plot2 = NA
  }else if(length(unique(part2$C)) == 1){
    part2_summary = summary(lm(Y~X, data = part2))
    plot(X, Y, data = part2); abline(lm(Y~X, data = part2), col = 'red')
    plot2 = recordPlot()
  }else{
    panel_data2 <<- plm.data(part2, index = c('C', 'E'))
    part2_summary = summary(plm(Y~X, data = panel_data2, model = method_plm))
    plot(plm(Y~X, data = panel_data2, model = method_plm))
    plot2 = recordPlot()
  }
  
  return(list(###detailed inforamtion for every group
    part1_final = part1,
    part2_final = part2,
    part3_final = part3,
    part1_summary = part1_summary,
    part2_summary = part2_summary,
    data = result$data,
    company_group = result$company_group,
    method = method_plm,
    plot1 = plot1,
    plot2 = plot2
  ))
  
}




#Example
setwd(dir = '/Users/gongfan/Desktop/CBS RA/segmentation/Linear segmentation')
electronic = read.csv('electronic.csv', header = T)

##BACKINCORE
points_BACKINCORE = cutpoint(electronic$SIZE, electronic$Mean_BACKINCORE,
                            electronic$Acquirer, electronic$Company, electronic$Earliest)
analysis_BACKINCORE = group_analyze(electronic$SIZE, electronic$Mean_BACKINCORE, electronic$Acquirer, 
                                    electronic$Company, electronic$Earliest, points_BACKINCORE$cutpoints)

points_BACKINCORE$cutpoints
points_BACKINCORE$aggregated_slope
analysis_BACKINCORE$part1_summary
analysis_BACKINCORE$part2_summary
write.csv(analysis_BACKINCORE$company_group,'a.csv', row.names = F)

##BACKOUTCORE
points_BACKOUTCORE = cutpoint(electronic$SIZE, electronic$Mean_BACKOUTCORE,
                            electronic$Acquirer, electronic$Company, electronic$Earliest)
analysis_BACKOUTCORE = group_analyze(electronic$SIZE, electronic$Mean_BACKOUTCORE, electronic$Acquirer, 
                                     electronic$Company, electronic$Earliest, points_BACKOUTCORE$cutpoints)

points_BACKOUTCORE$cutpoints
points_BACKOUTCORE$aggregated_slope
analysis_BACKOUTCORE$part1_summary
analysis_BACKOUTCORE$part2_summary

##BACKOIFAC
points_BACKOIFAC = cutpoint(electronic$SIZE, electronic$Mean_BACKOIFAC,
                            electronic$Acquirer, electronic$Company, electronic$Earliest)
analysis_BACKOIFAC = group_analyze(electronic$SIZE, electronic$Mean_BACKOIFAC, electronic$Acquirer, 
                                   electronic$Company, electronic$Earliest, points_BACKOIFAC$cutpoints)

points_BACKOIFAC$cutpoints
points_BACKOIFAC$aggregated_slope
analysis_BACKOIFAC$part1_summary
analysis_BACKOIFAC$part2_summary

##FWDINCORE
points_FWDINCORE = cutpoint(electronic$SIZE, electronic$Mean_FWDINCORE,
                            electronic$Acquirer, electronic$Company, electronic$Earliest)
analysis_FWDINCORE = group_analyze(electronic$SIZE, electronic$Mean_FWDINCORE, electronic$Acquirer, 
                                   electronic$Company, electronic$Earliest, points_FWDINCORE$cutpoints)

points_FWDINCORE$cutpoints
points_FWDINCORE$aggregated_slope
analysis_FWDINCORE$part1_summary
analysis_FWDINCORE$part2_summary

##FWDOUTCORE
points_FWDOUTCORE = cutpoint(electronic$SIZE, electronic$Mean_FWDOUTCORE,
                            electronic$Acquirer, electronic$Company, electronic$Earliest)
analysis_FWDOUTCORE = group_analyze(electronic$SIZE, electronic$Mean_FWDOUTCORE, electronic$Acquirer, 
                                    electronic$Company, electronic$Earliest, points_FWDOUTCORE$cutpoints)

points_FWDOUTCORE$cutpoints
points_FWDOUTCORE$aggregated_slope
analysis_FWDOUTCORE$part1_summary
analysis_FWDOUTCORE$part2_summary

##FWDOIFAC
points_FWDOIFAC = cutpoint(electronic$SIZE, electronic$Mean_FWDOIFAC,
                            electronic$Acquirer, electronic$Company, electronic$Earliest)
analysis_FWDOIFAC = group_analyze(electronic$SIZE, electronic$Mean_FWDOIFAC, electronic$Acquirer, 
                                  electronic$Company, electronic$Earliest, points_FWDOIFAC$cutpoints)

points_FWDOIFAC$cutpoints
points_FWDOIFAC$aggregated_slope
analysis_FWDOIFAC$part1_summary
analysis_FWDOIFAC$part2_summary
