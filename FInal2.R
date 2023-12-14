# ################################################################# #

#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####

# ################################################################# #



### Clear memory

rm(list = ls())



### Load Apollo library

library(apollo)



### Initialise code

apollo_initialise()



### Set core controls

apollo_control = list(
  
  modelName  ="MNL",
  
  modelDescr ="Simple MNL model on Airline choice data",
  
  indivID    ="Id"
  
)



# ################################################################# #

#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####

# ################################################################# #



setwd("C:/Users/hp/Desktop/M.Tech thesis/Full data application")

database = read.csv("final MNL data.csv",header=TRUE)



# ################################################################# #

#### DEFINE MODEL PARAMETERS                                     ####

# ################################################################# #



### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(asc_Alt1   = 0,
              
              asc_Alt2   = 0,
              
              asc_Alt3   = 0,
              asc_Alt4   = 0,
              asc_Alt5   = 0,
              asc_Alt6   = 0,
              asc_Alt7   = 0,
              asc_Alt8   = 0,
              asc_Alt9   = 0,
              asc_Alt10  = 0,
              asc_Alt11  = 0,
              asc_Alt12  = 0,
              asc_Alt13  = 0,
              asc_Alt14  = 0,
              asc_Alt15  = 0,
              asc_Alt16  = 0,
              asc_Alt17  = 0,
              asc_Alt18  = 0,
              asc_Alt19  = 0,
              asc_Alt20  = 0,
              asc_Alt21  = 0,
              asc_Alt22  = 0,
              asc_Alt23  = 0,
              asc_Alt24  = 0,
              asc_Alt25  = 0,
              asc_Alt26  = 0,
              asc_Alt27  = 0,
              asc_Alt28  = 0,
              asc_Alt29  = 0,
              asc_Alt30  = 0,
              asc_Alt31  = 0,
              asc_Alt32  = 0,
              asc_Alt33  = 0,
              asc_Alt34  = 0,
              asc_Alt35  = 0,
              asc_Alt36  = 0,
              asc_Alt37  = 0,
              asc_Alt38  = 0,
              asc_Alt39  = 0,
              asc_Alt40  = 0,
              asc_Alt41  = 0,
              asc_Alt42  = 0,
              asc_Alt43  = 0,
              asc_Alt44  = 0,
              asc_Alt45  = 0,
              asc_Alt46  = 0,
              asc_Alt47  = 0,
              asc_Alt48  = 0,
              asc_Alt49  = 0,
              asc_Alt50  = 0,
              asc_Alt51  = 0,
              asc_Alt52  = 0,
              asc_Alt53  = 0,
              asc_Alt54  = 0,
              asc_Alt55  = 0,
              asc_Alt56  = 0,
              asc_Alt57  = 0,
              asc_Alt58  = 0,
              asc_Alt59  = 0,
              asc_Alt60  = 0,
              asc_Alt61  = 0,
              asc_Alt62  = 0,
              asc_Alt63  = 0,
              asc_Alt64  = 0,
              asc_Alt65  = 0,
              asc_Alt66  = 0,
              asc_Alt67  = 0,
              asc_Alt68  = 0,
              asc_Alt69  = 0,
              asc_Alt70  = 0,
              asc_Alt71  = 0,
              asc_Alt72  = 0,
              asc_Alt73  = 0,
              asc_Alt74  = 0,
              asc_Alt75  = 0,
              asc_Alt76  = 0,
              asc_Alt77  = 0,
              asc_Alt78  = 0,
              asc_Alt79  = 0,
              asc_Alt80  = 0,
              asc_Alt81  = 0,
              asc_Alt82  = 0,
              asc_Alt83  = 0,
              asc_Alt84  = 0,
              asc_Alt85  = 0,
              asc_Alt86  = 0,
              asc_Alt87  = 0,
              asc_Alt88  = 0,
              asc_Alt89  = 0,
              asc_Alt90  = 0,
              asc_Alt91  = 0,
              asc_Alt92  = 0,
              asc_Alt93  = 0,
              asc_Alt94  = 0,
              asc_Alt95  = 0,
              asc_Alt96  = 0,
              asc_Alt97  = 0,
              asc_Alt98  = 0,
              asc_Alt99  = 0,
              asc_Alt100  = 0,
              
              
              B_fAirline = 0,
              B_stayDurationMinutes  = 0,
              B_totalPrice = 0,
              B_totalTripDurationMinutes = 0,
              B_dtd = 0,
              B_nAirlines = 0,
              B_nFlights = 0,
              B_outDepTime_sin = 0,
              B_outDepTime_cos = 0,
              B_outArrTime_sin = 0,
              B_outArrTime_cos = 0
              
              
)




### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none

apollo_fixed = c("asc_Alt1")



# ################################################################# #

#### GROUP AND VALIDATE INPUTS                                   ####

# ################################################################# #



apollo_inputs = apollo_validateInputs()



# ################################################################# #

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####

# ################################################################# #



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  
  
  ### Attach inputs and detach after function exit
  
  apollo_attach(apollo_beta, apollo_inputs)
  
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  
  
  ### Create list of probabilities P
  
  P = list()
  
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  
  V = list()
  
  
  # List of alternatives
  # Create a vector with 100 alternative names
  alt_names <- paste0('Alt', 1:100)
  
  
  # List to store utility equations
  V <- list()
  
  # Loop to create and print utility equations
  for (alt in alt_names) {
    alt_index <- gsub("Alt", "", alt)
    utility_eq <- paste0(
      "V[['", alt, "']] = asc_", alt, " + ",
      "B_fAirline * fAirline_", alt_index,  " + ",
      "B_stayDurationMinutes*stayDurationMinutes_", alt_index, " + B_totalPrice*totalPrice_", alt_index, " + ",
      "B_totalTripDurationMinutes*totalTripDurationMinutes_", alt_index, " + B_dtd*dtd_", alt_index,
      " + B_nAirlines*nAirlines_", alt_index , " + ",
      "B_nFlights*nFlights_", alt_index, " + B_outDepTime_sin*outDepTime_sin_", alt_index , " + ",
      "B_outDepTime_cos*outDepTime_cos_", alt_index, " + B_outArrTime_sin*outArrTime_sin_", alt_index, " + ",
      "B_outArrTime_cos*outArrTime_cos_", alt_index
    )
    eval(parse(text = utility_eq))  # Evaluate and assign the utility equation
  }
  
  
  
  
  ### Define settings for MNL model component
  
  mnl_settings = list(
    
    alternatives  = c(Alt1=1, Alt2=2, Alt3=3, Alt4=4, Alt5=5, Alt6=6, Alt7=7, Alt8=8, Alt9=9, Alt10=10,
                      Alt11=11, Alt12=12, Alt13=13, Alt14=14, Alt15=15, Alt16=16, Alt17=17, Alt18=18, Alt19=19, Alt20=20,
                      Alt21=21, Alt22=22, Alt23=23, Alt24=24, Alt25=25, Alt26=26, Alt27=27, Alt28=28, Alt29=29, Alt30=30,
                      Alt31=31, Alt32=32, Alt33=33, Alt34=34, Alt35=35, Alt36=36, Alt37=37, Alt38=38, Alt39=39, Alt40=40,
                      Alt41=41, Alt42=42, Alt43=43, Alt44=44, Alt45=45, Alt46=46, Alt47=47, Alt48=48, Alt49=49, Alt50=50,
                      Alt51=51, Alt52=52, Alt53=53, Alt54=54, Alt55=55, Alt56=56, Alt57=57, Alt58=58, Alt59=59, Alt60=60,
                      Alt61=61, Alt62=62, Alt63=63, Alt64=64, Alt65=65, Alt66=66, Alt67=67, Alt68=68, Alt69=69, Alt70=70,
                      Alt71=71, Alt72=72, Alt73=73, Alt74=74, Alt75=75, Alt76=76, Alt77=77, Alt78=78, Alt79=79, Alt80=80,
                      Alt81=81, Alt82=82, Alt83=83, Alt84=84, Alt85=85, Alt86=86, Alt87=87, Alt88=88, Alt89=89, Alt90=90,
                      Alt91=91, Alt92=92, Alt93=93, Alt94=94, Alt95=95, Alt96=96, Alt97=97, Alt98=98, Alt99=99, Alt100=100
    ), 
    
    avail         = list(Alt1=1, Alt2=1, Alt3=1, Alt4=1, Alt5=1, Alt6=1, Alt7=1, Alt8=1, Alt9=1, Alt10=1,
                         Alt11=1, Alt12=1, Alt13=1, Alt14=1, Alt15=1, Alt16=1, Alt17=1, Alt18=1, Alt19=1, Alt20=1,
                         Alt21=1, Alt22=1, Alt23=1, Alt24=1, Alt25=1, Alt26=1, Alt27=1, Alt28=1, Alt29=1, Alt30=1,
                         Alt31=1, Alt32=1, Alt33=1, Alt34=1, Alt35=1, Alt36=1, Alt37=1, Alt38=1, Alt39=1, Alt40=1,
                         Alt41=1, Alt42=1, Alt43=1, Alt44=1, Alt45=1, Alt46=1, Alt47=1, Alt48=1, Alt49=1, Alt50=1,
                         Alt51=1, Alt52=1, Alt53=1, Alt54=1, Alt55=1, Alt56=1, Alt57=1, Alt58=1, Alt59=1, Alt60=1,
                         Alt61=1, Alt62=1, Alt63=1, Alt64=1, Alt65=1, Alt66=1, Alt67=1, Alt68=1, Alt69=1, Alt70=1,
                         Alt71=1, Alt72=1, Alt73=1, Alt74=1, Alt75=1, Alt76=1, Alt77=1, Alt78=1, Alt79=1, Alt80=1,
                         Alt81=1, Alt82=1, Alt83=1, Alt84=1, Alt85=1, Alt86=1, Alt87=1, Alt88=1, Alt89=1, Alt90=1,
                         Alt91=1, Alt92=1, Alt93=1, Alt94=1, Alt95=1, Alt96=1, Alt97=1, Alt98=1, Alt99=1, Alt100=1
    ), 
    
    choiceVar     = choice,
    
    V             = V
    
  )
  
  
  
  ### Compute probabilities using MNL model
  
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  #p = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
  
}



# ################################################################# #

#### MODEL ESTIMATION                                            ####

# ################################################################# #



model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)



# ################################################################# #

#### MODEL OUTPUTS                                               ####

# ################################################################# #



# ----------------------------------------------------------------- #

#---- FORMATTED OUTPUT (TO SCREEN)                               ----

# ----------------------------------------------------------------- #



apollo_modelOutput(model)



# ----------------------------------------------------------------- #

#---- FORMATTED OUTPUT (TO FILE, using model name)               ----

# ----------------------------------------------------------------- #



apollo_saveOutput(model)
model <- apollo_loadModel("C:/Users/hp/Desktop/M.Tech thesis/Full data apllication/MNL_model.rds")
apollo_saveOutput(model)
warnings()
