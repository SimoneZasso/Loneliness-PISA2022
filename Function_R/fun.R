calculate_composite <- function(data, list) {
  for (compositeScore in names(list)) {
    data <- data %>% 
      mutate({{compositeScore}} := rowMeans(select(., list[[compositeScore]]), na.rm = TRUE))
  }
  return(data)
}


my_alpha <- function(data, items) {
  data %>% 
    select(all_of(items)) %>% 
    psych::alpha() %>% 
    pluck(., "total", "raw_alpha") %>%
    round(., 2) %>% 
    print()
}


med_mcmc <- function(a, b, se_a, se_b, cov_ab_number, D, rep = 20000, my_seed){
  
  require(MASS)
  
  # paths
  a=as.numeric(c(a))
  b=as.numeric(c(b))
  
  #asymptotic sampling variance for a and b
  se_a <- as.numeric(c(se_a))^2
  se_b <- as.numeric(c(se_b))^2
  
  # cov between parameters
  cov_ab_number <- as.numeric(c(cov_ab_number))
  D <- as.numeric(c(D))
  cov_ab_D <- 10^(D)
  cov_ab <- cov_ab_number*cov_ab_D
  
  #rep=20000
  conf=95
  pest=c(a,b)
  acov <- matrix(c(
    se_a, cov_ab,
    cov_ab, se_b
  ),2,2)
  
  my_seed <- 111
  set.seed(my_seed)
  
  mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
  ab <- mcmc[,1]*mcmc[,2]
  low <- (1-conf/100)/2
  upp <- ((1-conf/100)/2)+(conf/100)
  LL <- quantile(ab,low)
  UL <- quantile(ab,upp)
  LL4 <- format(LL,digits=4)
  UL4 <- format(UL,digits=4)
  ################################################
  # The number of columns in the histogram can   #
  # be changed by replacing 'FD' below with      #
  # an integer value.                            #
  ################################################
  
  ind_effect <- a*b
  
  mediat_mcmc_values <- data.frame(
    LL4, UL4, ind_effect
  )
  
  return(as_tibble(mediat_mcmc_values))
  
  #print(ind_effect)
  
#  hist(ab,breaks='FD',col='skyblue',xlab=paste('Indirect effect = ', ind_effect,';', conf,'% Confidence Interval ','LL',LL4,'  UL',UL4), main='Distribution of Indirect Effect')
  #print(ab)
  
  
  # cat(
  #   paste0('Indirect effect = ',
  #          ind_effect,'; ',
  #          conf,'% Confidence Interval = ',
  #          'LL ',LL4,', UL ',UL4
  #   )
  # )
}
