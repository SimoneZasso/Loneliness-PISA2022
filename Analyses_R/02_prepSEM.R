

# Prepare data for Multilevel SEM ----------------------------------------------------

data_mlSEM <- myData %>%
  # mutate(
  #   id_new = row_number() # Create an ID based on `row_number`
  # ) %>% 
  rowwise() %>% 
  mutate(
    Math_ach  = PV1MATH/100,   # rescaling Math Achievement
    BSMJ_sca  = BSMJ/10        # Rescaling BSMJ
  ) %>% 
  select(
    
    # ID and Grouping/clustering variables
    
    # id_new,
    CNTRYID,  # Country ID
    CNTSCHID, # School ID
    
    
    # Covariates
    
    ST004D01T,    # Gender
    ESCS,         # Socio-economic status
    Cooperation,  # Cooperation
    Bully,        # Bullying
    TS_rel,       # Teacher-Student relationship
    Perseverance, # Perseverance (Conscientiousness)
    # BSMJ,        # Expected status
    BSMJ_sca,     # Rescaled version of BSMJ
    
    # Variables related to `X`
    Loneliness,     # Composite score mean(i1_rev + i4_rev + i6_rev)
    ST034Q01TA_rev, # Item 1 of School Belongingess
    ST034Q04TA_rev, # Item 4 of School Belongingess
    ST034Q06TA_rev, # Item 6 of School Belongingess
    
    # Variables related to `M`
    Matanx,        # Composite score (see below the items)
    ST292Q01JA_rev,
    ST292Q02JA_rev,
    ST292Q03JA_rev,
    ST292Q04JA_rev,
    ST292Q05JA_rev,
    ST292Q06JA_rev,
    
    # Variables related to Math Achievement
#    PV1MATH,
    Math_ach      # Rescaled version of `PV1MATH`
  ) %>%
  dplyr::rename(
    schoo_id =   CNTSCHID,
    gender   =   ST004D01T, # 1 = f, 2 = m 
    cooper   =   Cooperation,
    lonel    =   Loneliness, 
    lonel_1  =   ST034Q01TA_rev,
    lonel_2  =   ST034Q04TA_rev,
    lonel_3  =   ST034Q06TA_rev,
    matanx1  =   ST292Q01JA_rev,
    matanx2  =   ST292Q02JA_rev,
    matanx3  =   ST292Q03JA_rev,
    matanx4  =   ST292Q04JA_rev,
    matanx5  =   ST292Q05JA_rev,
    matanx6  =   ST292Q06JA_rev,
    persev   =   Perseverance
  ) %>% 
  round(., 3)


# Export .dat file for ML-SEM ------------------------------------------------

prepareMplusData(data_mlSEM, "./analyses_MPlus/data_mlSEM.dat")


# Check Descr Stats -------------------------------------------------------

data_mlSEM %>%
  select(gender:Math_ach) %>%
  psych::describe(fast = F)
