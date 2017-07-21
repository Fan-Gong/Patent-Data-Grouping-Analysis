library(dplyr)

#Function for gathering each group's information  
grouping = function(grouping_result, industry){
  #grouping_result is a data frame contains the grouping result that Kathy made, and she divided the data to five group.
  #industry is a data frame contains all the information in that industry
  group1 = grouping_result %>% select(Company, Final_Group, Check) %>% filter(Final_Group == 1)
  group1 = inner_join(group1, industry, by = "Company")
  group2 = grouping_result %>% select(Company, Final_Group, Check) %>% filter(Final_Group == 2)
  group2 = inner_join(group2, industry, by = "Company")
  group3 = grouping_result %>% select(Company, Final_Group, Check) %>% filter(Final_Group == 3)
  group3 = inner_join(group3, industry, by = "Company")
  group4 = grouping_result %>% select(Company, Final_Group, Check) %>% filter(Final_Group == 4)
  group4 = inner_join(group4, industry, by = "Company")
  group5 = grouping_result %>% select(Company, Final_Group, Check) %>% filter(Final_Group == 5)
  group5 = inner_join(group5, industry, by = "Company")
  
  return(list(group1 = group1,
              group2 = group2,
              group3 = group3,
              group4 = group4,
              group5 = group5))
  
}

#Function for calculating discriptive information 
discriptive = function(group, variable){
  #group is a list contains five groups' data frame
  #variable is the name of one financial variable
  
  each_group = function(group, variable){
    #group is a data frame contains all the information one group has
    df = data.frame()
    for(i in 1992:2014){
      cols = match(variable, names(group))
      E = match("Earliest", names(group))
      year = group %>% select(E, cols) %>% 
        filter(group$Earliest == i, is.na(group[,cols])==F)  
      cols = match(variable, names(year))
      df_year = year %>% summarise(num = n(), mean = mean(year[,cols]), median = median(year[,cols]),
                                   sd = sd(year[,cols]), max = max(year[,cols]), min = min(year[,cols]))
      df = rbind(df, df_year)
    }
    return(df)
  }
  group1_result = each_group(group$group1, variable)
  colnames(group1_result) = c("num_g1", "mean_g1", "median_g1", "sd_g1", "max_g1", "min_g1")
  group2_result = each_group(group$group2, variable)
  colnames(group2_result) = c("num_g2", "mean_g2", "median_g2", "sd_g2", "max_g2", "min_g2")
  group3_result = each_group(group$group3, variable)
  colnames(group3_result) = c("num_g3", "mean_g3", "median_g3", "sd_g3", "max_g3", "min_g3")
  group4_result = each_group(group$group4, variable)
  colnames(group4_result) = c("num_g4", "mean_g4", "median_g4", "sd_g4", "max_g4", "min_g4")
  group5_result = each_group(group$group5, variable)
  colnames(group5_result) = c("num_g5", "mean_g5", "median_g5", "sd_g5", "max_g5", "min_g5")
  
  year = data.frame(year = seq(from = 1992, to = 2014))
  group_result = data.frame(year, group1_result, group2_result, group3_result, group4_result, group5_result)
  group_result = group_result %>% mutate(num_sum = sum(num_g1, num_g2, num_g3, num_g4, num_g5))
  
  return(group_result)
  
}


##example
electronic_grouping = read.csv('ELECTRONICS Groupings on Six Patent Factors.csv', header = T)
electronic = read.csv("electronic.csv", header = T)

group = grouping(electronic_grouping, electronic)
result_ROA = discriptive(group, "ROA")
result_SIZE = discriptive(group, "SIZE")



