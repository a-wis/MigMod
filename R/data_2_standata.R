#' Function transforming data frame to a list - input to Stan
#'
#' @import dplyr
#' @import tidyr
#' @export
#' @param df A data frame with migration data for sending countries, receiving countries, data quality measures and covariates
#' @param sending A vector of ISO2 codes with sending countries to be used in the model (recommended to keep SE and FI)
#' @param receiving A vector of ISO2 codes with receiving countries to be used in the model (recommended to keep SE and FI)
#' @param years A vector of years to be used in the model (2010 to 2019)
#' @param ref.country A character with an ISO2 code receiving country which immigration data are is used as a reference. This assumes that the data reported by this country are not biased. This assumption ensures identification of model parameters. It is strongly recommended to use Sweden ("SE") as a reference.
#' @param priors A list containing hyperparameters for the beta distributions representing high and low undercounting (i.e. bias) of emigration and immigration (EL, EH, IL, IH). Default values are based on experts' assessment in Wiśniowski \emph{et al}. (2013).
#' @return A list to be passed on to the stan model that contains all required inputs
#' @references Wiśniowski A., Bijak J., Christiansen S., Forster J. J., Keilman N., Raymer J., & Smith P. W. (2013). Utilising expert opinion to improve the measurement of international migration in Europe. \emph{Journal of Official Statistics}, 29(4), 583-607. \href{https://doi.org/10.2478/jos-2013-0041}{https://doi.org/10.2478/jos-2013-0041}
#'
data_2_standata = function(df, sending = c("SE","FI","IT","PL"),
                           receiving = c("SE","FI","IT","PL"),
                           years = 2010:2019,
                           ref.country="SE",
                           priors = list(EL=c(16.95790, 44.94987),
                                         EH=c(38.52081, 32.13520),
                                         IL=c(8.066021, 57.490667),
                                         IH=c(20.89134, 44.56385))){
  df <- df %>%
    dplyr::filter(Sending_iso2 %in% sending,
           Receiving_iso2 %in% receiving,
           Year %in% years) %>%
    dplyr::mutate(corr_f=Corridor %>% as.factor() %>% as.numeric(),
           orig=Sending %>% as.factor() %>% as.numeric(),
           dest=Receiving %>% as.factor() %>% as.numeric(),
           benchmark=ifelse(Receiving_iso2==ref.country,1,0))

  #data undercounting auxiliary objects
  XL_em=df %>%
    dplyr::filter(!is.na(Emig)) %>%
    dplyr::select(Emig,UE) %>%
    model.matrix(Emig~UE-1,data=.)

  XL_im=df %>%
    dplyr::filter(!is.na(Immi)) %>%
    dplyr::select(Immi,UI,benchmark) %>%
    model.matrix(Immi~benchmark+UI-1,data=.)
  XL_im[,2]=XL_im[,2]-XL_im[,1]
  XL_im=XL_im[,2:4]

  out=list(N_mis=df %>%
             dplyr::filter(is.na(Immi)& is.na(Emig)) %>%
             pull(Emig) %>%
             length(), # N missing E & I
           N_obs=df %>%
             dplyr::filter(Year<2020 & (!is.na(Immi) | !is.na(Emig))) %>%
             pull(Immi) %>% length(), # N obs E & I
           N_all= dim(df)[1], # N all
           N_obs1=df %>%
             dplyr::filter(Year<2020, !is.na(Immi)) %>%
             pull(Immi) %>% length(), # N obs I
           N_mis1=df %>%
             pull(Immi) %>% is.na() %>%
             sum(), # N missing I
           N_obs2=df %>%
             dplyr::filter(Year<2020, !is.na(Emig)) %>%
             pull(Emig) %>% length(), # N obs E
           N_mis2=df %>%
             pull(Emig) %>% is.na() %>%
             sum(), # N missing E
           N_preds=3, # No. predictors
           z1=df %>%
             dplyr::filter(Year<2020, !is.na(Immi)) %>%
             pull(Immi), # data I
           z2=df %>%
             dplyr::filter(Year<2020, !is.na(Emig)) %>%
             pull(Emig), # data E
           z_ind1 = df %>%
             select(Immi) %>%
             is.nna() %>% which(), # index obs I
           z_ind2 = df %>%
             select(Emig) %>%
             is.nna() %>% which(), # index obs E
           X_all=df %>%
             select(Pop.Sending,
                    Pop.Receiving,
                    distw) %>%
             log %>%
             scale %>%
             as.matrix, # predictors matrix
           corridor_all=df %>%
             pull(corr_f), # corridors index
           N_corr=length(unique(df$Corridor)), # N corridors
           acc_e=df %>%
             mutate(acc_E=as.numeric(fct_recode(AE,"Low"="Medium"))) %>%
             dplyr::filter(!is.na(Emig)) %>%
             pull(acc_E), # accuracy index E
           acc_i=df %>%
             mutate(acc_I=as.numeric(fct_recode(AI,"Low"="Medium"))) %>%
             dplyr::filter(!is.na(Immi)) %>%
             pull(acc_I), # accuracy index I
           XL_e=XL_em,
           XL_i=XL_im,
           hypv_psi0=3.0,
           hypv_psi1=0.1,
           hypm_psi1=1.0,
           EL=priors$EL,
           EH=priors$EH,
           IL=priors$IL,
           IH=priors$IH
  )
  return(out)
}
