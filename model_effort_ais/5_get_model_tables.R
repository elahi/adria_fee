################################################################################
##' @title Get summary tables for the final bam model
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-07-11
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD NECESSITIES #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

## Load final bam model
load(file = "workspace/model_data_revised/bam_2.RData")
m_check <- bam_2

##### FORMAT TABLE FOR PUBLICATION #####
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

summary_model <- summary(m_check)
summary_model$p.coeff
summary_model$pTerms.table
summary_model$pTerms.table
summary_model$p.coeff
summary_model

#Format ANOVA table - linear
anova_table <- summary_model$pTerms.table 
anova_table$Term <- rownames(anova_table)

# Format linear coefficients
p_table_rownames <- data.frame(names(summary_model$p.table[,1]))

p_table <- data.frame(summary_model$p.table) %>% 
  mutate(uci = Estimate + qnorm(0.975) * Std..Error, 
         lci = Estimate - qnorm(0.975) * Std..Error)

p_table <- cbind(p_table_rownames, p_table)

names(p_table) <- c("Parameter", "Estimate", "SE", "t", "P", "UCI", "LCI")

p_table <- round_df(p_table, digits = 4)

p_table

write.csv(p_table, file = "model_effort_ais/model_effort_ais_output/bam_final_p_table.csv")

# Format splines
s_table <- data.frame(summary_model$s.table)

# Remove Ref.df
s_table <- select(s_table, -Ref.df)
row.names(s_table)

write.csv(s_table, file = "model_effort_ais/model_effort_ais_output/bam_final_s_table.csv")
