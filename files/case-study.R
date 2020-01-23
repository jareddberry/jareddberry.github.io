# Libraries for case-study
library(dplyr) # For data wrangling/manipulation
library(quantmod) # For data pulls from Fred/Yahoo! Finance
library(sandwich) # For robust standard errors
library(lmtest) # For applying robust standard errors to regression output
library(tidyr) # Chiefly for the `gather` function to melt data.frames
library(reshape2) # Chiefly for the `melt` function to melt data.frames
library(purrr) # Chiefly for the `reduce` function to condense lists
library(ggplot2) # For data visualization
library(stargazer) # For pretty regression tables

# INGESTION -------------------------------------------------------------------

# Using `getSymbols` from quantmod
?getSymbols

# Stock prices from Yahoo! Finance
getSymbols('AAPL', src='yahoo', auto.assign = F) %>%
  tail

# Series from FRED
getSymbols('T10Y2Y', src='FRED', auto.assign = F) %>%
  head

# Identify series of interest
series <- c('EXUSUK', # US/UK Exchange Rate
            'EXJPUS', # Japan/US Exchange Rate
            'EXCAUS', # Canada/US Exchange Rate
            'EXSZUS', # Switzerland/US Exchange Rate
            'GBRCPIALLMINMEI', # UK CPI
            'CPALTT01JPM661S', # Japan CPI
            'CPALCY01CAM661N', # Canada CPI
            'CPALCY01USM661N', # US CPI
            'CHECPIALLMINMEI'  # Swiss CPI
            )

# Ingest series from FRED via quantmod library
# Each series is an `xts` object (an R time-series class) - we'll want to turn them into something more familiar
getSymbols('EXUSUK', src='FRED')
str(EXUSUK)
head(index(EXUSUK))
head(EXUSUK)

exusuk <- data.frame(date=index(EXUSUK),
                     EXUSUK=EXUSUK[1],
                     stringsAsFactors = F)

head(exusuk)

# Use a loop to store each element in a list, for convenience
series_dfs <- vector('list', length(series))
for(i in seq_along(series)) {
  series_xts <- getSymbols(series[i], src='FRED', auto.assign = F)
  series_df <- data.frame(date=index(series_xts),
                          series=series_xts,
                          stringsAsFactors = F)
  names(series_df)[2] <- series[i]
  
  series_dfs[[i]] <- series_df
  
}

# Boil these down into a data.frame
all_series_df <- reduce(series_dfs, full_join, by='date')
glimpse(all_series_df)

# Wrapper to use 'off-the-shelf'
get_series_df <- function(series, source, start, end) {
  series_dfs <- vector('list', length(series))
  for(i in seq_along(series)) {
    series_xts <- getSymbols(series[i], src=source, auto.assign = F)
    series_df <- data.frame(date=index(series_xts),
                            series=series_xts,
                            stringsAsFactors = F)
    names(series_df)[2] <- series[i]
    
    series_dfs[[i]] <- series_df
    
  }
  
  all_series_df <- purrr::reduce(series_dfs, full_join, by='date')
  filtered_series_df <- filter(all_series_df, 
                               all_series_df$date <= end,
                               all_series_df$date >= start)
  
  return(filtered_series_df)
  
}

all_series_df <- read.csv('all_series_df.csv', stringsAsFactors = F)

# WRANGLING -------------------------------------------------------------------

# dplyr pipes to sort, filter, and transform data into relevant series of interest
models_data_df <- 
  all_series_df %>%
  arrange(date) %>%
  filter(date >= '1971-01-01' & date <= '2015-06-01') %>%
  transmute(date = date,
            lusukxr = log(EXUSUK),
            luscaxr = log(1/EXCAUS),
            lusjpxr = log(1/EXJPUS),
            lusszxr = log(1/EXSZUS),
            lusukppp = log(CPALCY01USM661N/GBRCPIALLMINMEI),
            luscappp = log(CPALCY01USM661N/CPALCY01CAM661N),
            lusjpppp = log(CPALCY01USM661N/CPALTT01JPM661S),
            lusszppp = log(CPALCY01USM661N/CHECPIALLMINMEI)
            )

# VISUALIZING I ---------------------------------------------------------------

# A list for storing useful bits of variables/labels for our models/visualizations
model_series <- list(
  'USD/Pound Sterling' = list(
    c('lusukxr', 'lusukppp'), 
    c('Log($/£)', 'Log(P/P*)')),
  'USD/Canadian Dollar' = list(
    c('luscaxr', 'luscappp'),
    c('Log($/$)', 'Log(P/P*)')),
  'USD/Japanese Yen' = list(
    c('lusjpxr', 'lusjpppp'),
    c('Log($/¥)', 'Log(P/P*)')),
  'USD/Swiss Franc' = list(
    c('lusszxr', 'lusszppp'),
    c('Log($/₣)', 'Log(P/P*)'))
  )

# Melt it down (tidyr `gather`)
plots_data_df <- gather(models_data_df, key=variable, value=value, -date)
glimpse(plots_data_df)

# Using a loop
for (i in seq_along(model_series)) {
  plot_title <- paste0('Figure ', i, ': Monthly Exchange Rate and PPP - ', names(model_series)[i])
  p <-
    ggplot(data=plots_data_df[plots_data_df$variable %in% model_series[[i]][[1]],], aes(x=date, y=value, col=variable)) +
      geom_line(size=1) +
      scale_color_discrete(labels=model_series[[i]][[2]][c(2,1)]) + # Manually setting legend labels without factors
      scale_x_date(date_breaks = '4 years', date_labels = '%b-%y') + # Formatting date axes
      labs(title = plot_title,
           caption = 'Source: FRED') +
      theme_minimal() +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = 'top')
  print(p)
  ggsave(paste0(gsub('/', '_', names(model_series[i])), '.png'))
}

# Using faceting
plots_data_df$country <- ifelse(grepl('usuk', plots_data_df$variable), 'USD/Pound Sterling',
                         ifelse(grepl('usca', plots_data_df$variable), 'USD/Canadian Dollar',
                         ifelse(grepl('usjp', plots_data_df$variable), 'USD/Japanese Yen',
                         ifelse(grepl('ussz', plots_data_df$variable), 'USD/Swiss Franc', NA))))

plots_data_df$series <- ifelse(grepl('ppp', plots_data_df$variable), 'Log(P/P*)',
                        ifelse(grepl('xr', plots_data_df$variable), 'Log($/$*)', NA))

plot_title <- 'Figure 1: Monthly Exchange Rate and PPP'
ggplot(data=plots_data_df, aes(x=date, y=value, col=series)) +
  geom_line(size=1) +
  facet_wrap(~country, scales='free_y') + # Facet (plot), by country with facet-specific y-axes
  scale_x_date(date_labels = '%b-%y') +
  labs(title = plot_title,
       caption = 'Source: FRED') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'top')

ggsave('monthly_xr_ppp_all.png')


# MODELING --------------------------------------------------------------------

# Force the `dplyr` lag function as default in this session
lag <- dplyr::lag

# Set-up helper function for calculating RMSE
rmse <- function(predicted,
                 observed) {
  
  if(length(predicted) != length(observed)){
    stop('Predicted and observed series are not the same length!')
  } else {
    return(sqrt(sum((predicted - observed)^2)/length(observed)))
  }
  
}

# Set vector of time horizons which distinguish our covariates
horizons <- 1:60
for(i in seq_along(model_series)) {
  
  message(paste0('Running models for ', names(model_series)[i]))
  
  # Pull out relevant data
  series <- model_series[[i]][[1]]
  y <- models_data_df[[series[1]]]
  x1 <- models_data_df[[series[1]]]
  x2 <- models_data_df[[series[2]]]
  
  # Storage for models run
  models <- vector('list', length=length(horizons))
  for(j in horizons) {
    
    # Models run at lag l ---
    l <- j
    
    # Hybrid models
    hybrid_fit <- lm(y ~ lag(x1, l) + lag(x2, l))
    hybrid_rob <- coeftest(hybrid_fit, function(x) vcovHAC(x))
    
    hybrid_preds <- predict(hybrid_fit)
    idx <- length(y) - length(hybrid_preds)
    hybrid_rmse <- rmse(predicted = hybrid_preds,
                        observed = y[(1+idx):length(y)])
    
    # Univariate models
    univariate_fit <- lm(y ~ lag(x1, l))
    univariate_rob <- coeftest(univariate_fit, function(x) vcovHAC(x))
    
    univariate_preds <- predict(univariate_fit)
    idx <- length(y) - length(univariate_preds)
    univariate_rmse <- rmse(predicted = univariate_preds,
                        observed = y[(1+idx):length(y)])
    
    # Diebold-Mariano tests
    dm <- NULL # dm.test(residuals(univariate_fit), residuals(hybrid_fit))
    
    models[[j]] <- list('hbr_fit'=hybrid_fit,
                        'hbr_summ'=summary(hybrid_fit),
                        'hbr_rob'=hybrid_rob,
                        'hbr_rmse'=hybrid_rmse,
                        'uni_fit'=univariate_fit,
                        'uni_summ'=summary(univariate_fit),
                        'uni_rob'=univariate_rob,
                        'uni_rmse'=univariate_rmse,
                        'dm'=dm)
    
  }
  
  # Name according to lag structure
  names(models) <- horizons
  
  # Append to our list of models
  model_series[[i]] <- append(model_series[[i]], 
                              list('models'=models))
  
}

# VISUALIZING II --------------------------------------------------------------

# More storage
coef_plots <- vector('list', length(model_series))
rmse_plots <- vector('list', length(model_series))
for(i in seq_along(model_series)) {
  
  model_info <- model_series[[i]]
  
  # Coefficient plots with error bars ---
  coef_plot_df <- data.frame(country = names(model_series)[i],
                             horizon = names(model_info$models),
                             theory_coef = sapply(model_info$models, function(x) x$hbr_fit$coefficients[3]),
                             series_coef = sapply(model_info$models, function(x) x$hbr_fit$coefficients[2]),
                             theory_se = sapply(model_info$models, function(x) x$hbr_rob[3,2]),
                             series_se = sapply(model_info$models, function(x) x$hbr_rob[2,2]),
                             stringsAsFactors =F)
  
  # Melt
  coef_plot_df <-
    coef_plot_df %>%
    gather(key=variable, value=value, -c(country:horizon)) %>%
    separate(variable, c('covariate', 'metric'), sep='_') %>%
    spread(key=metric, value=value) %>%
    mutate(horizon = as.numeric(horizon)) %>%
    arrange(covariate, horizon)
  
  # Store
  coef_plots[[i]] <- coef_plot_df
  
  
  # RMSE plots data ---
  rmse_plot_df <- data.frame(country = names(model_series)[i],
                             horizon = names(model_info$models),
                             Hybrid_r2 = sapply(model_info$models, function(x) x$hbr_summ$r.squared),
                             Hybrid_rmse = sapply(model_info$models, function(x) x$hbr_rmse),
                             Univariate_r2 = sapply(model_info$models, function(x) x$uni_summ$r.squared),
                             Univariate_rmse = sapply(model_info$models, function(x) x$uni_rmse),
                             stringsAsFactors=F)
  
  # Melt
  rmse_plot_df <- 
    rmse_plot_df %>%
    gather(key=variable, value=value, -c(country:horizon)) %>%
    separate(variable, c('model', 'series'), sep='_') %>%
    mutate(horizon = as.numeric(horizon)) %>%
    arrange(horizon)
  
  # Store
  rmse_plots[[i]] <- rmse_plot_df
  
}

all_coef_plots <- reduce(coef_plots, bind_rows)
all_rmse_plots <- reduce(rmse_plots, bind_rows)

# Coefficient evolution plots ---

ggplot(all_coef_plots, aes(x=horizon, y=coef, col=covariate)) +
  geom_errorbar(aes(ymin=coef-se, ymax=coef+se), col='black') + 
  geom_point() +
  facet_wrap(~country, scales = 'free_y') +
  scale_color_discrete(labels=c('$/$*', 'P/P*')) + # Manually setting legend labels without factors
  labs(title = 'Figure 2: Evolution of Coefficients for Forecasts - HAC Standard Error Bars',
       caption = 'Source: FRED',
       x = 'k-lags',
       y = 'βk') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'top')

ggsave('coef_evolution_facet_plot.png')

# Model fit plots ---

ggplot(all_rmse_plots, aes(x=horizon, y=value, col=series)) +
  geom_point() +
  facet_grid(model~country, scales='free_y') +
  scale_color_discrete(labels=c('R^2', 'RMSFE')) + # Manually setting legend labels without factors
  labs(title = 'Figure 3: Model Fit for Forecasts - R^2 and RMSE',
       caption = 'Source: FRED',
       x = 'k-lags',
       y = 'Metric') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'top')

ggsave('model_fit_facet_plot.png')

# Just Hybrid
ggplot(all_rmse_plots[all_rmse_plots$model == 'Hybrid',], aes(x=horizon, y=value, col=series)) +
  geom_point() +
  facet_grid(series~country, scales = 'free_y') +
  scale_color_discrete(labels=c('R^2', 'RMSFE')) + # Manually setting legend labels without factors
  labs(title = 'Figure 3a: Model Fit for Hybrid Model Forecasts - R^2 and RMSE',
       caption = 'Source: FRED',
       x = 'k-lags',
       y = 'Metric') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = 'top')

ggsave('hybrid_model_fit_facet_plot.png')

# Just Univariate
ggplot(all_rmse_plots[all_rmse_plots$model == 'Univariate',], aes(x=horizon, y=value, col=series)) +
  geom_point() +
  facet_grid(series~country, scales = 'free_y') +
  scale_color_discrete(labels=c('R^2', 'RMSFE')) + # Manually setting legend labels without factors
  labs(title = 'Figure 3b: Model Fit for Univariate Model Forecasts - R^2 and RMSE',
       caption = 'Source: FRED',
       x = 'k-lags',
       y = 'Metric') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = 'top')

ggsave('univariate_model_fit_facet_plot.png')

# REPORTING -------------------------------------------------------------------

table_horizons <- c(1, 3, 4, 6, 9, 12, 16, 24, 32, 36, 48, 60)
for(i in seq_along(model_series)) {
  
  # Set up parameters/data
  model_info <- model_series[[i]]$models[table_horizons]
  
  hybrid_models <- lapply(model_info, function(x) x$hbr_fit)
  hybrid_robust_ses <- lapply(model_info, function(x) x$hbr_rob[, 'Std. Error'])
  univariate_models <- lapply(model_info, function(x) x$uni_fit)
  univariate_robust_ses <- lapply(model_info, function(x) x$uni_rob[, 'Std. Error'])
  
  output_type <- 'text'
  output_ext <- switch(output_type, text='.txt', html='.html', latex='.tex')
  
  # Hybrid model regression tables
  stargazer(hybrid_models,
            align = T,
            notes.align = "c",
            title = paste0("Table ", i,
                           "a: Hybrid Model Forecasting Regression Output (1- to 60-month Ahead Forecast Horizons) – ",
                           names(model_series)[i],
                           ' Exchange Rate Forecasts'),
            se = hybrid_robust_ses,
            covariate.labels = c(
              paste0('Log(', names(model_series)[i], ')'),
              'Log(P/P*)'),
            dep.var.caption = paste0('Dependent Variable: Log(', names(model_series)[i], ')'),
            dep.var.labels = 'k-months ahead',
            column.labels = as.character(table_horizons),
            model.numbers = F,
            omit.stat=c("ser", "f", "rsq"),
            add.lines = list(c('RMSE', sapply(model_info, function(x) round(x$hbr_rmse, 3)))),
            notes = c('HAC robust standard errors in parentheses',
                      '*p<0.1; **p<0.05; ***p<0.01'),
            notes.append = F,
            out.header = T,
            type=output_type,
            out = paste0('../tables/', gsub('/', '_', names(model_series[i])), '_hybrid', output_ext))
  
  # Univariate model regression tables
  stargazer(univariate_models,
            align = T,
            notes.align = "c",
            title = paste0("Table ", i,
                           "b: Univariate Autoregressive Forecasting Model Regression Output (1- to 60-month Ahead Forecast Horizons) – ",
                           names(model_series)[i],
                           ' Exchange Rate Forecasts'),
            se = univariate_robust_ses,
            covariate.labels = c(paste0('Log(', names(model_series)[i], ')')),
            dep.var.caption = paste0('Dependent Variable: Log(', names(model_series)[i], ')'),
            dep.var.labels = 'k-months ahead',
            column.labels = as.character(table_horizons),
            model.numbers = F,
            omit.stat=c("ser", "f", "rsq"),
            add.lines = list(c('RMSE', sapply(model_info, function(x) round(x$uni_rmse, 3)))),
            notes = c('HAC robust standard errors in parentheses',
                      '*p<0.1; **p<0.05; ***p<0.01'),
            notes.append = F,
            out.header = T,
            type=output_type,
            out = paste0(gsub('/', '_', names(model_series[i])), '_univariate', output_ext))
  
}
