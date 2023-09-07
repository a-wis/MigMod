#' Function to plot synthetic migration estimates
#'
#' @export
#' @param df A data frame with migration data for sending countries, receiving countries, data quality measures and covariates
#' @param mmfit A `stanfit` object produced by the function `migmod_m` that contains estimates of the model parameters and synthetic estimates of the "true" migration.
#' @param log.m Logical (`FALSE` by default). The function produces a plot with estimates on a level-scale. Setting this option to `TRUE` will produce results on a log scale.
#' @param sending A vector of ISO2 codes with sending countries to be used in the model (recommended to keep SE and FI)
#' @param receiving A vector of ISO2 codes with receiving countries to be used in the model (recommended to keep SE and FI)
#' @param years A vector of years to be used in the model (2010 to 2019)
#' @return An object of class `ggplot`.
#'
plot_migmod <- function(df,
                        mmfit,
                        log.m = FALSE,
                        sending = c("SE","FI","IT","PL"),
                        receiving = c("SE","FI","IT","PL"),
                        years = 2010:2019){

dat1 = df %>%
  filter(Sending_iso2 %in% sending,
         Receiving_iso2 %in% receiving,
         Year %in% years)

sumy1=rstan::summary(mmfit,pars=ifelse(log.m==FALSE,"yl","y"))$summary %>%
  as.data.frame() %>%
  bind_cols(dat1) %>%
  mutate(UI=fct_relevel(UI,"high","medium","low"),
         UE=fct_relevel(UE,"high","medium","low"),
         Immi=ifelse(log.m==FALSE,Immi,log(Immi)),
         Emig=ifelse(log.m==FALSE,Emig,log(Emig)))

sumy1 %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=(`2.5%`),ymax=(`97.5%`),fill="95% CI"),alpha=0.4) +
  geom_point(aes(x=Year,y=Immi,colour="Immi",shape=AI, size=UI),
             alpha=0.5) +
  geom_point(aes(x=Year,y=Emig,colour="Emig",shape=AE, size=UE),
             alpha=0.5) +
  scale_shape_discrete(na.translate = F) +
  scale_size_discrete(range=c(1,3)) +
  scale_x_continuous(breaks=seq(2010,2019,3)) +
  scale_fill_manual(values = "darkgreen") +
  facet_grid(Sending_iso2~Receiving_iso2,switch = "y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
  labs(y="Migration flows",
       colour="Measure",
       size="Bias",
       shape="Accuracy",
       fill="Estimate")
}
