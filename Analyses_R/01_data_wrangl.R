
# Read data ---------------------------------------------------------------

myData <- read_sav("./data/CY08MSP_STU_QQQ.sav")


# Reverse items -----------------------------------------------------------

# List of items with 4-point Likert scale

rev_item_4 <- c(
  "ST034Q02TA", "ST034Q03TA", "ST034Q05TA", "ST263Q02JA", "ST263Q04JA",
  "ST263Q06JA", "ST034Q01TA", "ST034Q04TA","ST034Q06TA","ST292Q01JA",
  "ST292Q02JA"	,"ST292Q03JA",	"ST292Q04JA",	"ST292Q05JA","ST292Q06JA",
  "ST267Q04JA","ST267Q08JA","ST292Q01JA",	"ST292Q02JA"	,"ST292Q03JA",
  "ST292Q04JA",	"ST292Q05JA_rev","ST292Q06JA") 
# "ST263Q08JA item with all missing values")

# List of items with 5-point Likert scale

rev_item_5 <- c(
  "ST343Q07JA","ST305Q04JA","ST305Q07JA","ST305Q08JA","ST343Q02JA",
  "ST343Q04JA","ST343Q05JA","ST343Q10JA","ST301Q03JA", "ST301Q08JA",
  "ST313Q02JA","ST313Q03JA","ST313Q04JA","ST313Q06JA","ST313Q08JA",
  "ST313Q09JA","ST313Q10JA","ST311Q01JA","ST311Q05JA","ST311Q07JA",
  "ST307Q04JA","ST307Q06JA","ST307Q07JA","ST307Q10JA","ST345Q01JA",
  "ST345Q03JA","ST345Q04JA","ST345Q07JA","ST345Q10JA"
)


for (i in seq_along(rev_item_4)) {
  myData %<>%
    mutate(
      !!sym(paste0(rev_item_4[i], "_rev")) := 5 - !!sym(rev_item_4[i])
    )
}

for (i in seq_along(rev_item_5)) {
  myData %<>%
    mutate(
      !!sym(paste0(rev_item_5[i], "_rev")) := 6 - !!sym(rev_item_5[i])
    )
}


# Composite scores --------------------------------------------------------


composite_scores <- list(
  Cooperation            = c("ST343Q01JA", "ST343Q02JA_rev", "ST343Q03JA", "ST343Q04JA_rev",
                             "ST343Q05JA_rev", "ST343Q06JA", "ST343Q07JA_rev", "ST343Q08JA",
                             "ST343Q09JA", "ST343Q10JA_rev"),
  Loneliness             = c("ST034Q01TA_rev", "ST034Q04TA_rev","ST034Q06TA_rev"),
  TS_rel                 = c("ST267Q01JA",	"ST267Q02JA",	"ST267Q03JA",	"ST267Q04JA_rev",
                             "ST267Q05JA",	"ST267Q06JA",	"ST267Q07JA",	"ST267Q08JA_rev"),
  Matanx                 = c("ST292Q01JA_rev",	"ST292Q02JA_rev"	,"ST292Q03JA_rev",
                             "ST292Q04JA_rev",	"ST292Q05JA_rev","ST292Q06JA_rev"),
  Bully                  = c("ST038Q03NA"	,"ST038Q04NA",	"ST038Q05NA",	"ST038Q06NA",
                             "ST038Q07NA",	"ST038Q08NA"	,"ST038Q09JA",	"ST038Q10JA",	"ST038Q11JA"),
   Perseverance           = c("ST307Q01JA", "ST307Q02JA", "ST307Q03JA", "ST307Q04JA_rev",
                             "ST307Q05JA", "ST307Q06JA_rev", "ST307Q07JA_rev", "ST307Q08JA",
                             "ST307Q09JA", "ST307Q10JA_rev")
 )



myData <- calculate_composite(
  data = myData,
  list = composite_scores
  )



## Cronbach's Alpha --------------------------------------------------------

purrr::map(composite_scores, ~ my_alpha(myData, .))



# Omega Within and Between ------------------------------------------------

# loneliness
omegaSEM(
  items = c("ST034Q01TA_rev", "ST034Q04TA_rev","ST034Q06TA_rev"),
  id = "CNTRYID",
  data = myData,
  savemodel = FALSE
)

# math anx
omegaSEM(
  items = c("ST292Q01JA_rev",	"ST292Q02JA_rev"	,"ST292Q03JA_rev",
            "ST292Q04JA_rev",	"ST292Q05JA_rev", "ST292Q06JA_rev"),
  id = "CNTRYID",
  data = myData,
  savemodel = FALSE
)



# Describe ----------------------------------------------------------------

myData %>%
  select(
    "PV1MATH", "Matanx", "Loneliness", "ST004D01T",
    "ESCS","Cooperation","Bully","TS_rel") %>%
  psych::describe()


