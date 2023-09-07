#' Function to estimate migration model
#'
#' @description The model estimates 'true', unobserved bilateral migration flows \eqn{y_{ijt}} from sending country \eqn{i} to receiving country \eqn{j} in year \eqn{t}. It relies on data reported by receiving countries (immigration, \eqn{z_{ijt}^R}) and sending countries (emigration, \eqn{z_{ijt}^S}). It is based on methodology developed by Raymer \emph{et al}. (2013). The data are Poisson-distributed with mean \eqn{\mu_{ijt}^k}:
#'
#' @description \eqn{z_{ijt}^k\sim\mathrm{Poisson}\left(\mu_{ijt}^k\right)}{z_{ijt}^k\sim\mathrm{Poisson}\left(\mu_{ijt}^k\right)}.
#'
#' @description The true migration flows then result from correcting the expected values of the data for biases represented by parameters \eqn{\lambda} and different accuracies of the sending and receiving countries' data \eqn{\sigma}:
#'
#' @description \eqn{\log\mu_{ijt}^k  \sim \mathrm{normal}\left(\log y_{ijt}+\lambda_{f(i,j)}, \sigma_{g(i,j)}^k\right)}{\log\mu_{ijt}^k  \sim \mathrm{normal}\left(\log y_{ijt}+\lambda_{f(i,j)}, \sigma_{g(i,j)}^k\right)}.
#'
#' @description The true flows are then modelled by using a mixed effects model that contains an autoregressive part with parameter \eqn{\phi} and a gravity-type model with populations of sending (\eqn{X_{it}}) and receiving (\eqn{X_{jt}}) countries as well as distance \eqn{D_{ij}} between them:
#'
#' @description \eqn{\log y_{ijt}^k  \sim \mathrm{normal}\left(\psi_0 + \psi_{1,ij}\log y_{ijt-1} + \beta_1 \log X_{it} + \beta_2 \log X_{jt} + \beta_3 D_{ij}, \sigma_y\right)}
#'
#' @description Note, that this specification requires that there are no fully unobserved flows in the model.
#' @export
#' @param df A data frame with migration data for sending countries, receiving countries, data quality measures and covariates
#' @param sending A vector of ISO2 codes with sending countries to be used in the model (recommended to keep SE and FI)
#' @param receiving A vector of ISO2 codes with receiving countries to be used in the model (recommended to keep SE and FI)
#' @param years A vector of years to be used in the model (2010 to 2019).
#' @param ref.country A character with an ISO2 code receiving country which immigration data are is used as a reference. This assumes that the data reported by this country are not biased. This assumption ensures identification of model parameters. It is strongly recommended to use Sweden ("SE") as a reference.
#' @param ... Arguments passed to function \code{\link[rstan]{sampling}} from package \pkg{\link{rstan}} (e.g. iter, chains) or \code{\link[MigMod]{data_2_standata}}.
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @references Raymer J., Wiśniowski A., Forster J. J., Smith P. W. & Bijak J. (2013). Integrated modeling of European migration. \emph{Journal of the American Statistical Association}, 108(503), 801-819. \href{https://doi.org/10.1080/01621459.2013.789435}{https://doi.org/10.1080/01621459.2013.789435}.
#'
#' @seealso \code{\link[rstan]{stan}}
migmod_m <- function(df,
                     sending = c("SE","FI","IT","PL"),
                     receiving = c("SE","FI","IT","PL"),
                     years = 2010:2019,
                     ref.country="SE",
                     ...){

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
