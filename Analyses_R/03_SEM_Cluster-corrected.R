

# SEM models with cluster-robust standard errors (McNeish, Stapleton, & Silverman, 2017)
# Model 1 refers to the model with a direct path from Loneliness to Math Achievement
# Model 2 refers to the a nested model in which the above path was fixed to be zero




# SEM Model 1--------------------------------------------------------------

## Step 1: Model Formulation ----------------------------------------------

sem_m1 <- '
  # measurement models

    lon     =~ lonel_1 + lonel_2 + lonel_3
    mat_anx =~ matanx1 + matanx2 + matanx3 + matanx4 + matanx5 + matanx6
  
  # regressions (structural model)
    
    Math_ach ~ b*mat_anx
    Math_ach ~ c*lon
    mat_anx  ~ a*lon
    
  # covariates (structural model)
  
    lon + mat_anx + Math_ach ~ gender +
                           ESCS +
                           cooper +
                           Bully +
                           TS_rel +
                           persev +
                           BSMJ_sca
                          
  # covariates (covariances)
  
    gender ~~ ESCS +
              cooper +
              Bully +
              TS_rel +
              persev +
              BSMJ_sca

    ESCS   ~~ cooper +
              Bully +
              TS_rel +
              persev +
              BSMJ_sca

    cooper ~~ Bully +
              TS_rel +
              persev +
              BSMJ_sca

    Bully  ~~ TS_rel +
              persev +
              BSMJ_sca

    TS_rel ~~ persev +
              BSMJ_sca

    persev  ~~ BSMJ_sca
   
  # residual covariance(s) 

      matanx5 ~~ matanx6
      
  # indirect and total effects
         ab := a * b
'


## Step 2: Model Estimation -----------------------------------------------

fit_sem_m1 <- sem(
  sem_m1,
  data = data_mlSEM,
  missing = "ML",
  estimator = "MLR",
  cluster = "CNTRYID"
)


## Step 3: Display output -------------------------------------------------

summary(
  fit_sem_m1,
  fit.measures = TRUE,
  rsquare=TRUE,
  standardized = TRUE
  )


## Step 4 (optional): Visualize Modification Indices ----------------------

# modindices(fit_sem_m1,
#            sort = TRUE,
#            maximum.number = 5)


## Step 5 (optional): Visualize your SEM ----------------------------------

# semPaths(fit_sem_m1,
#          whatLabels = "std",
#          sizeLat = 10,
#          nCharNodes = 7)


# SEM Model 2--------------------------------------------------------------


## Step 1: Model Formulation ----------------------------------------------

# sem_m2 <- paste(sem_m1, "Math_ach ~ 0*lon") # fix to be zero the path `c`

sem_m2 <- gsub(
  "Math_ach ~ c\\*lon", # old part to be replaced
  "Math_ach ~ 0*lon",   # new part (fix to be zero the path `c`)
  sem_m1
  )

## Step 2: Model Estimation -----------------------------------------------

fit_sem_m2 <- sem(
  sem_m2,
  data = data_mlSEM,
  missing = "ML",
  estimator = "MLR",
  cluster = "CNTRYID"
)


## Step 3: Display output -------------------------------------------------

summary(
  fit_sem_m2,
  fit.measures = TRUE,
  rsquare=TRUE,
  standardized = TRUE
  )



# Model Comparison --------------------------------------------------------

lavTestLRT(fit_sem_m1, fit_sem_m2, method = "satorra.bentler.2001")

# Scaled Chi-Squared Difference Test (method = “satorra.bentler.2001”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df      AIC      BIC Chisq Chisq diff Df diff Pr(>Chisq)   
# fit_sem_m1 81 18193031 18194039 58416                                 
# fit_sem_m2 82 18193782 18194779 59169     10.168       1   0.001429 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

