#' Function transforming data frame with data to standata (a list)
#' @param df A data frame with migration data for sending countries, receiving countries, data quality measures and covariates
#' @param sending A vector of ISO2 codes with sending countries to be used in the model (recommended to keep SE and FI)
#' @param receiving A vector of ISO2 codes with receiving countries to be used in the model (recommended to keep SE and FI)
#' @param years A vector of years to be used in the model (2010 to 2019)
#' @return A list to be passed on to the stan model that contains all required inputs
#'
data_2_standata = function(df, sending = c("SE","FI","IT","PL"),
                           receiving = c("SE","FI","IT","PL"),
                           years = 2010:2019){
  df <- df %>%
    filter(Sending_iso2 %in% sending,
           Receiving_iso2 %in% receiving,
           Year %in% years) %>%
    mutate(corr_f=Corridor %>% as.factor() %>% as.numeric(),
           orig=Sending %>% as.factor() %>% as.numeric(),
           dest=Receiving %>% as.factor() %>% as.numeric())

  #data undercounting auxiliary objects
  XL_em=df %>%
    filter(!is.na(Emig)) %>%
    select(Emig,UE) %>%
    model.matrix(Emig~UE-1,data=.)

  XL_im=df %>%
    filter(!is.na(Immi)) %>%
    select(Immi,UI) %>%
    model.matrix(Immi~UI-1,data=.)
  XL_im[,1]=0

  out=list(N_mis=df %>%
             filter(is.na(Immi)& is.na(Emig)) %>%
             pull(Emig) %>%
             length(), # N missing E & I
           N_obs=df %>%
             filter(Year<2020 & (!is.na(Immi) | !is.na(Emig))) %>%
             pull(Immi) %>% length(), # N obs E & I
           N_all= dim(df)[1], # N all
           N_obs1=df %>%
             filter(Year<2020, !is.na(Immi)) %>%
             pull(Immi) %>% length(), # N obs I
           N_mis1=df %>%
             pull(Immi) %>% is.na() %>%
             sum(), # N missing I
           N_obs2=df %>%
             filter(Year<2020, !is.na(Emig)) %>%
             pull(Emig) %>% length(), # N obs E
           N_mis2=df %>%
             pull(Emig) %>% is.na() %>%
             sum(), # N missing E
           N_preds=3, # No. predictors
           z1=df %>%
             filter(Year<2020, !is.na(Immi)) %>%
             pull(Immi), # data I
           z2=df %>%
             filter(Year<2020, !is.na(Emig)) %>%
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
             filter(!is.na(Emig)) %>%
             pull(acc_E), # accuracy index E
           acc_i=df %>%
             mutate(acc_I=as.numeric(fct_recode(AI,"Low"="Medium"))) %>%
             filter(!is.na(Immi)) %>%
             pull(acc_I), # accuracy index I
           XL_e=XL_em,
           XL_i=XL_im,
           hypv_phi=3.0,
           hypv_psi0=3.0,
           hypv_psi1=0.1,
           hypm_psi1=1.0,
           EL=c(16.95790, 44.94987),
           EH=c(38.52081, 32.13520),
           IL=c(8.066021, 57.490667),
           IH=c(20.89134, 44.56385)
  )
  return(out)
}