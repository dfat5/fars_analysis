# creating the functions

# per_cis function 
perc_cis <- function(x, n) {
  p = x / n
  
  se = sqrt( (p*(1-p) ) /n)
  
  cu = p + 1.96 * se
  cl = p - 1.96 * se
  
  p_percent = round(p*100, 1)
  cu_percent = round(cu*100, 1)
  cl_percent = round(cl*100, 1)
  
  out <- paste0( p_percent, "%", " (", cl_percent, "%, ", cu_percent, "%)" )
  
  return(out)
}

perc_cis(x = 9000, n = 23000)


#test for trend using Cochran-Armitage trend test function


test_trend_ca <- function(drug, df = clean_fars){
  
 
  if (drug == "Nonalcohol") {
  to_test <- df %>% 
    filter(drug_type != "Alcohol") %>% 
    group_by(year, unique_id) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  
  ca_nonalcohol <- prop.trend.test(x = to_test$positive, 
                                n = to_test$trials)

  out <- data_frame(Z = round(sqrt(ca_nonalcohol$statistic), 1), 
                      p.value = round(ca_nonalcohol$p.value, 3))
  
  }
  else { 
    to_test <- df %>% 
      filter(drug_type == drug ) %>% 
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    
    ca_nonalcohol <- prop.trend.test(x = to_test$positive, 
                                     n = to_test$trials)
    
  
    out <- data_frame(Z = round(sqrt(ca_nonalcohol$statistic), 1), 
                        p.value = round(ca_nonalcohol$p.value, 3))
    
    }
    
return(out)
  
}

test_trend_ca(drug = "Nonalcohol")

test_trend_ca(drug = "Alcohol")

test_trend_ca(drug = "Stimulant")







