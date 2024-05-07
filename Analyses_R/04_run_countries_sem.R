
# Run with 32gb RAM pc  -------------------------------------------------------


sem_analysis_country <- data.frame()


lista_country <- c(
  
  # List without countries that showed all missing values on some variables 
  
  8 ,31,	32,	36,	40,	56,	76,	96,	100, 152,	158,
  170, 191,  203,	208,	214,	222,	233,	246,
  250,	268,	275,	276,	300,  344,	348,	352,	360,
  372,	380,	383,	388,	398,	400,	410,	428,
  440,	446,	458,	470,	484,	496,	498,	499,	504,	528,
  554,	578,	591,	604,	608,	616,	620,	634,	642,
  682,	688,	702,	703,	705,	724,	756,	764,
  784,	792,	807,	826,	858,	860,	901
  
  # Complete list of all countries
  
  # 8 ,31,	32,	36,	40,	56,	76,	96,	100,	116,	124,	152,	158,
  # 170,	188,	191,	196,	203,	208,	214,	222,	233,	246,
  # 250,	268,	275,	276,	300,	320,	344,	348,	352,	360,
  # 372,	376,	380,	383,	388,	392,	398,	400,	410,	428,
  # 440,	446,	458,	470,	484,	496,	498,	499,	504,	528,
  # 554,	578,	591,	600,	604,	608,	616,	620,	634,	642,
  # 682,	688,	702,	703,	704,	705,	724,	752,	756,	764,
  # 784,	792,	807,	826,	840,	858,	860,	901
)



# Create a dataframe storing all info -------------------------------------

# NOTE: It takes about 1 hour to run

for (country_id in lista_country) {
  sem_analysis_country <- run_analysis_and_save(country_id, sem_analysis_country)
}

## Extract model comparison and parameters table ------------------------------

sem_analysis_country <- change_name_country(sem_analysis_country)

write_xlsx(sem_analysis_country, "./output/model_comparison.xlsx")


# Partial Mediation Models ------------------------------------------------

sem_analysis_country_PartMed2 <- c()

lista_country_partMed <- c(807, 76, 170, 208, 32, 504, 860, 222, 608, 214, 591)


for (country_id in lista_country_partMed) {
  sem_analysis_country_PartMed2 <- run_analysis_and_save_PartMed(country_id, sem_analysis_country_PartMed2)
}

## Extract model comparison and parameters table ------------------------------

sem_analysis_country_PartMed2 <- change_name_country(sem_analysis_country_PartMed2)

write_xlsx(sem_analysis_country_PartMed2, "./output/fit_parameters_PartMed.xlsx")



