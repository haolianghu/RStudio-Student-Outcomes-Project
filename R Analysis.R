library('rsample')
library('modelr')
library('tidyverse')
library('mosaic')
library('fastDummies')
library('stringr')
source('https://tinyurl.com/y4krd9uy')



#################################################################################################################################
# DATA CLEANING AND WRANGLING

# Remove rows with null
no.na.data <- na.omit(R_Data)

# Creating dummy variables, need fastDummies library
EmploymentOutcomes <- dummy_cols(no.na.data, select_columns = c('WorkAuthorizationDetailed', 'CountryofCitizenship', 
                                                                'NorthAmericaRegion', 'Industry', 'Function', 'MonthOfferReceived',
                                                                'QuarterOfferReceived', 'YearOfferReceived', 'MBACSEAJobSource',
                                                                'MonthStart', 'QuarterStart', 'YearStart'))
# Drop unused and repetitive columns
EmploymentOutcomes = subset(EmploymentOutcomes, select = -c(WorkAuthorizationDetailed, CountryofCitizenship, NorthAmericaRegion, Industry, 
                                            Function, MonthOfferReceived, QuarterOfferReceived, YearOfferReceived, 
                                            MBACSEAJobSource, MonthStart, QuarterStart, YearStart, YearsExperienceConsolidated, 
                                            WorkAuthorizationConsolidated))

# Rename column names with spaces, need stringr library
names(EmploymentOutcomes)<-str_replace_all(names(EmploymentOutcomes), c(" " = "" , "/" = "" ))

# Split the data
EmploymentOutcomesSplit =  initial_split(EmploymentOutcomes, prop=0.77)
EmploymentOutcomesTrain = training(EmploymentOutcomesSplit)
EmploymentOutcomesTest  = testing(EmploymentOutcomesSplit)



#################################################################################################################################
# FEATURE ENGINEERING PART I

# Set scope 
lm_all = lm(AnnualSalary ~.-StudentID, data = EmploymentOutcomesTrain)

# Step wise selection
lm_some = lm(AnnualSalary ~ YearsofExperience + Function_Consulting + Function_FinanceAccounting + 
               Function_GeneralManagement + Function_HumanResources + Function_MarketingSales + 
               Function_OperationsLogistics + Function_Other, data = EmploymentOutcomesTrain)

lm_step = step(lm_some, scope=formula(lm_all))
formula(lm_step)
summary(lm_step)
simple_anova(lm_step) %>% round(3)
# Resulting rmse = 26183.62
rmse(lm_step, EmploymentOutcomesTest) 

# Forward selection
lm_empty = lm(AnnualSalary ~ 1, data=EmploymentOutcomesTrain)
lm_forward = step(lm_empty, direction='forward', scope=formula(lm_all)) 
formula(lm_forward)
summary(lm_forward)
simple_anova(lm_forward) %>% round(3) 
# Resulting rmse = 24699.71
rmse(lm_forward, EmploymentOutcomesTest) 

# Backward selection
lm_backward = step(lm_all, direction='backward') 
formula(lm_backward)
summary(lm_backward)
simple_anova(lm_backward) %>% round(3) 
# Resulting rmse = 24271.94
rmse(lm_backward, EmploymentOutcomesTest)



#################################################################################################################################
# FEATURE ENGINEERING PART II

# Simulation to find the best selection method, need tidyverse and mosaic library
rmse_sim = do(1000)*{
  
  # re-split into train and test cases
  EmploymentOutcomesSplit = initial_split(EmploymentOutcomes, prop=0.77)
  EmploymentOutcomesTrain = training(EmploymentOutcomesSplit)
  EmploymentOutcomesTest  = testing(EmploymentOutcomesSplit)
  
  # Fit to the training data
  # use `update` to refit the same model with a different set of data
  # we're using "update" here to avoid having to type out the giant model formulas
  lmS = update(lm_step, data=EmploymentOutcomesTrain)
  lmF = update(lm_forward, data=EmploymentOutcomesTrain)
  lmB = update(lm_backward, data=EmploymentOutcomesTrain)
  
  # Set scope formula
  # lm_all = lm(AnnualSalary ~.-StudentID, data = EmploymentOutcomesTrain)
  
  # Step wise
  # lm_some = lm(AnnualSalary ~ YearsofExperience + Function_Consulting + Function_FinanceAccounting + 
  #                Function_GeneralManagement + Function_HumanResources + Function_MarketingSales + 
  #                Function_OperationsLogistics + Function_Other, data = EmploymentOutcomesTrain)
  # lm_step = step(lm_some, scope=formula(lm_all))
  
  # Forward
  # lm_empty = lm(AnnualSalary ~ 1, data=EmploymentOutcomesTrain)
  # lm_forward = step(lm_empty, direction='forward', scope=formula(lm_all)) 
  
  # Backward
  # lm_backward = step(lm_all, direction='backward') 
  
  # Performance on test-set data
  model_errors = c(rmse(lmS, EmploymentOutcomesTest),
                   rmse(lmF, EmploymentOutcomesTest),
                   rmse(lmB, EmploymentOutcomesTest))
  
  # return the model errors from the do loop
  model_errors
}

# V1       V2       V3 
# 22066.28 23238.37 24371.29 
# V1, step wise selection, is best for this data set. 
colMeans(rmse_sim)

# Again, step wise selection information, the one we will use for final formula
formula(lmS)
# Passed linearity check 
plot(lmS)
# Look for statistically significant attributes
summary(lmS)

# Final model with attributes that are statistically significant from lmS
lmFinal = lm(AnnualSalary ~ Function_Consulting + Function_GeneralManagement + 
               Function_MarketingSales + Function_OperationsLogistics + 
               QuarterOfferReceived_3 + MonthOfferReceived_Jan + QuarterOfferReceived_4 + 
               Industry_Other + Industry_Government + CountryofCitizenship_Brazil + 
               YearStart_2020 + MonthOfferReceived_Nov + 
               `NorthAmericaRegion_Mid-Atlantic` + NorthAmericaRegion_Northeast + 
               `CountryofCitizenship_UnitedStates(USA),Italy` + 
               Industry_Retail, data = EmploymentOutcomes)
# Overview of the final model
summary(lmFinal)
# Statistically significant and have meaningful R^2 improvement are mainly from 
# MonthOfferReceived, Industry, Function, and Region (Location) related attributes
simple_anova(lmFinal) %>% round(3) 
# Passed linearity check, also student id 191 is an outlier
plot(lmFinal)
