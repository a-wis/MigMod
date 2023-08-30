#' Function to estimate migration model
#'
#' @export
#' @param df A data frame with migration data for sending countries, receiving countries, data quality measures and covariates
#' @param sending A vector of ISO2 codes with sending countries to be used in the model (recommended to keep SE and FI)
#' @param receiving A vector of ISO2 codes with receiving countries to be used in the model (recommended to keep SE and FI)
#' @param years A vector of years to be used in the model (2010 to 2019)
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
migmod_m <- function(df,
                              sending = c("SE","FI","IT","PL"),
                              receiving = c("SE","FI","IT","PL"),
                              years = 2010:2019, ...){

standata <- data_2_standata(df=df,
                            sending=sending,
                            receiving=receiving,
                            years=years)

# initial values ####
inits=list(beta=rep(0,standata$N_preds),sigma=c(0.1,0.1),
           sigma_psi_1=0.1, sigma_psi_2=0.1,
           sigma_y=0.1,
           lambda1=c(0.5,0.5,0.5),lambda2=c(0.5,0.5,0.5),
           psi_c0=1,psi_c1=0.1)


out <- rstan::sampling(stanmodels$migmod,
                       data = standata,
                       cores=2, chains=2,
                       init = list(inits,inits),
                       iter = 1000, warmup = 500,
                       control = list(adapt_delta=0.95,
                                      max_treedepth=15),
                       ...)
return(out)
}
