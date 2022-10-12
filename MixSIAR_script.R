#### MixSIAR

library(MixSIAR)

### from https://peerj.com/articles/5096/

mix.filename.pp<-"../../Data/MixSIAR/mixture_ie_consumer_keysites_2022_10_11_month_category.csv"
mix.pp <- load_mix_data(filename=mix.filename.pp, 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Location", "MonthYear"), 
                     fac_random=c(TRUE, TRUE), 
                     fac_nested=c(FALSE, FALSE), 
                     cont_effects=NULL)

#difficult to get continuous effect with date, went with day of the year for 2021, and then 365+day of year for 2022 dates
#may be a better way to do this

source.filename.pp<-"../../Data/MixSIAR/source_tidy_2022_10_11.csv"
source.pp <- load_source_data(filename=source.filename.pp,
                           source_factors=NULL,
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix.pp)

#set discrimination factor to 0
#per manual, TDF is the amount that a consumer's tissue biotracer values are modified (enriched/depleted) after consuming a source. If tracers are conservative, then set TDF = 0 (ex. essential fatty acids, fatty acid profile data, element concentrations). 
discr.filename<-"../../Data/MixSIAR/discr_tidy_2022_10_11.csv"
discr <- load_discr_data(filename=discr.filename, mix.pp)

plot_data(filename="./MixSIAR_output/2022_10_11_month_as_factor_normal/isospace_plot", plot_save_pdf=TRUE, plot_save_png=TRUE, mix.pp,source.pp, discr)

calc_area(source=source.pp,mix=mix.pp,discr=discr)
#21.53565

plot_prior(alpha.prior=1,source.pp)

# Write the JAGS model file

model_filename <- "./MixSIAR_output/2022_10_11_month_as_factor_normal/MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix.pp, source.pp)

run <- list(chainLength=100,000, burn=50,000, thin=50, chains=3, calcDIC=TRUE)

jags.1 <- run_model(run="test", mix.pp, source.pp, discr, model_filename)

jags.1 <- run_model(run="normal", mix.pp, source.pp, discr, model_filename)

output_JAGS(jags.1, mix.pp, source.pp, output_options=list(summary_save = TRUE, summary_name = "summary_statistics",
                                                           sup_post = FALSE, plot_post_save_pdf = TRUE, plot_post_name = "posterior_density",
                                                           sup_pairs = FALSE, plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", sup_xy
                                                           = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                                                             FALSE, geweke = TRUE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                                                             FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                                                             FALSE, diag_save_ggmcmc = TRUE))
plot_continuous_var(jags.1, mix.pp, source.pp, output_options=list(summary_save = TRUE, summary_name = "summary_statistics",
                                                                   sup_post = FALSE, plot_post_save_pdf = TRUE, plot_post_name = "posterior_density",
                                                                   sup_pairs = FALSE, plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", sup_xy
                                                                   = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                                                                    FALSE, geweke = TRUE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                                                                     FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                                                                     FALSE, diag_save_ggmcmc = TRUE))




####### Generate example plot
mix.filename.pp<-"../../Data/MixSIAR/mixture_ie_consumer_datenumeric.csv"
mix.pp <- load_mix_data(filename=mix.filename.pp, 
                        iso_names=c("d13C","d15N"), 
                        factors=c("Location"), 
                        fac_random=c(TRUE), 
                        fac_nested=c(FALSE), 
                        cont_effects=c("Date"))

source.filename.pp<-"../../Data/MixSIAR/source_example.csv"
source.pp <- load_source_data(filename=source.filename.pp,
                              source_factors=NULL,
                              conc_dep=FALSE, 
                              data_type="means", 
                              mix.pp)

#set discrimination factor to 0
#per manual, TDF is the amount that a consumer's tissue biotracer values are modified (enriched/depleted) after consuming a source. If tracers are conservative, then set TDF = 0 (ex. essential fatty acids, fatty acid profile data, element concentrations). 
discr.filename<-"../../Data/MixSIAR/discr_example.csv"
discr <- load_discr_data(filename=discr.filename, mix.pp)

plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix.pp,source.pp, discr)

calc_area(source=source.pp,mix=mix.pp,discr=discr)

plot_prior(alpha.prior=1,source.pp)

# Write the JAGS model file

model_filename <- "MixSIAR_model_example.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix.pp, source.pp)

run <- list(chainLength=1000, burn=500, thin=1, chains=3, calcDIC=TRUE)

jags.1 <- run_model(run="test", mix.pp, source.pp, discr, model_filename)

jags.1 <- run_model(run="normal", mix.pp, source.pp, discr, model_filename)

output_JAGS(jags.1, mix.pp, source.pp, output_options=list(summary_save = TRUE, summary_name = "summary_statistics",
                                                           sup_post = FALSE, plot_post_save_pdf = TRUE, plot_post_name = "posterior_density",
                                                           sup_pairs = FALSE, plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", sup_xy
                                                           = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                                                             FALSE, geweke = TRUE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                                                             FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                                                             FALSE, diag_save_ggmcmc = TRUE))
plot_continuous_var(jags.1, mix.pp, source.pp, alphaCI=0.75, output_options=list(summary_save = TRUE, summary_name = "summary_statistics",
                                                                   sup_post = FALSE, plot_post_save_pdf = TRUE, plot_post_name = "posterior_density",
                                                                   sup_pairs = FALSE, plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", sup_xy
                                                                   = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                                                                     FALSE, geweke = TRUE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                                                                     FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                                                                     FALSE, diag_save_ggmcmc = TRUE))
