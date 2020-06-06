library(kableExtra)
library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores())
options(scipen = 999)
set.seed(44)
options(knitr.kable.NA = '')


# load data ---------------------------------------------------------------

Citibike <- read_csv('Analyses/Bayesian-Citibike/Daily_Citibike_counts.csv')

# split dataset to train and test
sample_indices <- sample(c(TRUE, FALSE), nrow(Citibike), replace = TRUE, prob = c(0.8, 0.2))
Citibike_train <- Citibike[sample_indices,]
Citibike_test <- Citibike[!sample_indices,]


# custom functions --------------------------------------------------------

theme_custom <- function() {
  theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = NA),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(color = "gray30",
                                face = "bold"),
      plot.subtitle = element_text(size = 10,
                                   color = "gray30"),
      text = element_text(family = "Helvetica"),
      plot.caption = element_text(face = "italic",
                                  size = 6,
                                  color = 'grey50'),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.key = element_rect(fill = NA)
    )
}
theme_set(theme_custom())

save_plot <- function(name, plot = ggplot2::last_plot(), type = "svg", height = 4, width = 6.5){
  # function saves ggplots with standardized sizes
  
  ggplot2::ggsave(
    filename = paste0('Analyses/Bayesian-Citibike/Blog-post/Plots/', name, '.', type),
    plot = plot,
    device = type,
    height = height,
    width = width
  )
}

blog_color <- '#394e48'

RMSE <- function(Y_hat, Y){
  #function to calculate RMSE
  sqrt(mean((Y_hat - Y)^2))
}


# intro -------------------------------------------------------------------

tribble(
  ~Variable, ~Type, ~Description,
  'Trip_count', 'continuous', 'Count of daily Citi Bike trips',
  'Weekday', 'boolean', '1 = weekday, 0 = weekend',
  'Precipitation', 'boolean', '1 = rain, 0 = no rain',
  'Temp', 'integer', 'Average daily temperature in Fahrenheit',
  'Gust_speed', 'continuous', 'Maximum gust speed in miles per hour'
  ) %>%
  kable() %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))




get_prior(Trip_count ~ Weekday + Precipitation + Temp + Gust_speed,
           data = Citibike_train, family = 'negbinomial')


priors <- prior(normal(0, 0.01), coef = "Gust_speed") +
  prior(normal(-0.5 , 0.1), coef = "Precipitation") +
  prior(normal(0.0, 0.01), coef = "Temp") +
  prior(normal(0.5, 0.1), coef = "Weekday") +
  prior(normal(10, 1), class = "Intercept") +
  prior(exponential(10), class = "shape")



priors[, 1:3] %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))


draws <- brm(Trip_count ~ Weekday + Precipitation + Temp + Gust_speed,
          data = Citibike_train, family = 'negbinomial',
          prior = priors, sample_prior = "only")

prior_mu <- pp_expect(draws, nsamples = 4000)


# grid of density plots of estimates
draws_long <- as.matrix(draws) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_remove(name, "^b_"),
         name = str_remove(name, "__"),
         name = factor(name, levels = c("Gust_speed", "Precipitation", "Temp", "Weekday", "Intercept", "shape", 'lp'))) %>% 
  filter(name != 'lp')
draws_long %>% 
  ggplot(aes(x = value)) +
  geom_density(color = blog_color) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Densities of prior estimates',
       subtitle = '4,000 samples',
       x = NULL,
       y = NULL)
save_plot('prior_estimates')

# density plot of mus
prior_mu %>%
  as.vector() %>%
  enframe() %>%
  ggplot(aes(x = value)) +
  geom_density(color = blog_color) +
  geom_vline(xintercept = quantile(prior_mu, .95), color = blog_color) +
  geom_vline(xintercept = quantile(prior_mu, .05), color = blog_color) +
  geom_text(data = tibble(x = c(quantile(prior_mu, .05), quantile(prior_mu, .95)), 
                          y = 0.5, 
                          label = c('5th percentile', '95th percentile')),
            aes(x = x, y = y, label = label), hjust = c(1, 0), nudge_x = c(-0.1, 0.1),
            family = 'Helvetica') +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = NULL) +
  labs(title = "Density of prior draws",
       subtitle = "4,000 samples",
       x = 'Predicted daily Citi Bike trips',
       y = NULL)
save_plot('prior_draws')



# Conditioning on the observed data ---------------------------------------

post_nb <- update(draws, sample_prior = "no")

summary(post_nb)$fixed %>%
  rbind(summary(post_nb)$spec_pars) %>% 
  kable(digits = c(2, 2, 2, 2, 3, 0, 0), format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))

post_mu <- pp_expect(post_nb, nsamples = 4000)

paste0('[', scales::comma(quantile(post_mu, 0.1)), ', ', scales::comma(quantile(post_mu, 0.9)), ']')
paste0('[', scales::comma(quantile(Citibike_test$Trip_count, 0.1)), ', ', scales::comma(quantile(Citibike_test$Trip_count, 0.9)), ']')
scales::comma(mean(post_mu))
scales::comma(median(post_mu))
scales::comma(mean(Citibike_test$Trip_count))
scales::comma(median(Citibike_test$Trip_count))

# grid of density plots of estimates
as.matrix(post_nb) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_remove(name, "^b_"),
         name = str_remove(name, "__"),
         name = factor(name, levels = c("Gust_speed", "Precipitation", "Temp", "Weekday", "Intercept", "shape", 'lp'))) %>% 
  filter(name != 'lp') %>% 
  bind_cols(draws_long) %>% 
  ggplot() +
  geom_density(aes(x = value1), color = blog_color) +
  geom_density(aes(x = value), fill = blog_color, alpha = 0.6, color = 'white') +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Densities of posterior estimates',
       subtitle = '4,000 samples',
       x = NULL,
       y = NULL)
save_plot('posterior_estimates')

# density plot of mus
post_mu %>%
  as.vector() %>%
  enframe() %>%
  bind_cols(prior_mu %>% as.vector() %>% enframe()) %>% 
  ggplot() +
  geom_density(aes(x = value1), color = blog_color) +
  geom_density(aes(x = value), fill = blog_color, alpha = 0.6, color = 'white') +
  geom_vline(xintercept = quantile(post_mu, .95), color = blog_color) +
  geom_vline(xintercept = quantile(post_mu, .05), color = blog_color) +
  geom_text(data = tibble(x = c(quantile(post_mu, .05), quantile(post_mu, .95)), 
                          y = 1.5, 
                          label = c('5th percentile', '95th percentile')),
            aes(x = x, y = y, label = label), hjust = c(1, 0), nudge_x = c(-0.05, 0.05),
            family = 'Helvetica') +
  annotate(geom = 'text', x = 4000, y = 0.35,
           label = 'prior draws', hjust = 1) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(name = 'Posterior draws') +
  labs(title = "Density of posterior draws",
       subtitle = "4,000 samples",
       x = 'Predicted daily Citi Bike trips',
       y = NULL)
save_plot('posterior_draws')


# pairs plot --------------------------------------------------------------

Citibike %>%
  mutate(Weekday = as.factor(Weekday),
         Precipitation = as.factor(Precipitation)) %>%
  GGally::ggpairs(lower = list(continuous = GGally::wrap("smooth", colour = blog_color, alpha = 0.2))) +
  theme(axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1))
save_plot('pairs', height = 6.5)



scales::comma(mean(Citibike$Trip_count))
round(cor(Citibike$Trip_count, Citibike$Temp), 2)
round(cor(Citibike$Trip_count, Citibike$Gust_speed), 2)

# Evaluating the negative binomial model ----------------------------------
nb_loo <- loo(post_nb)

nb_loo$estimates %>%
  kable(digits = c(1, 2), format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))


colors <- c('low' = '#baccd9', 'medium' = '#7793a8', 'high' = '#03396c')
nb_loo$diagnostics$pareto_k %>% 
  enframe() %>% 
  mutate(color = case_when(
    value < 0.5 ~ 'low',
    value < 1 ~ 'medium',
    TRUE ~ 'high',
  )) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_point(alpha = 0.8, aes(color = color)) +
  # geom_hline(yintercept = 1, color = 'black') +
  # geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  scale_color_manual(values = colors, guide = FALSE) +
  labs(title = "PSIS diagnostic:  Negative binomial  model",
       x = 'Data point (index)',
       y = 'Pareto shape k')
save_plot('PSIS_ng')

pp_check(post_nb, type = "loo_intervals") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Negative binomial in-sample performance',
       subtitle = paste0('Training dataset of ', nrow(Citibike_train), ' observations'),
       y = 'Daily Citi Bike trips')
save_plot('NG_preds', height = 4.5)


# Predicting new data -----------------------------------------------------

nrow(Citibike_train)
nrow(Citibike_test)

nb_preds <- posterior_predict(post_nb, newdata = Citibike_test)
nb_means <- colMeans(nb_preds) %>% enframe()

colors <- c("Pred_fill" = "#d1e0eb", "Pred_stroke" = "#b3cddf", "Y_fill" = '#03396c', "Y_stroke" = '#011f4a')

gg_nb_pred <- as_tibble(nb_preds) %>%
  setNames(1:ncol(nb_preds)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = as.numeric(name)) %>%
  ggplot(aes(x = name, y = value, group = name)) +
  geom_boxplot(aes(color = "Pred_stroke"), alpha = 0.5, outlier.shape = NA) +
  geom_point(data = nb_means, aes(y = value, x = 1:ncol(nb_preds), fill = 'Pred_fill', color = "Pred_stroke"),
             size = 3, stroke = 2, shape = 21) +
  geom_point(data = Citibike_test, aes(y = Trip_count, x = 1:ncol(nb_preds), group = 1:ncol(nb_preds),
                                       fill = 'Y_fill',  color = 'Y_stroke'), size = 2, shape = 21) +
  scale_fill_manual(values = colors, guide = FALSE) +
  scale_colour_manual(values = colors, labels = c(expression('y'[rep]), "y")) +
  scale_y_continuous(limits = c(0, 180000), labels = scales::comma) +
  labs(title = "Negative binomial out-of-sample performance",
       subtitle = paste0('Test dataset of ', ncol(nb_preds), ' left out observations'),
       x = 'Data point (index)',
       y = 'Daily Citi Bike trips')

save_plot(name = 'NG_out_of_sample_preds', plot = gg_nb_pred, height = 4.5)



# An alternative model: Poisson -------------------------------------------

post_pois <- update(post_nb, family = poisson)

summary(post_pois)$fixed %>%
  kable(digits = c(2, 3, 2, 2, 3, 0, 0), format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))

pois_loo <- loo(post_pois)

sum(pois_loo$diagnostics$pareto_k > 0.5)
scales::percent(mean(pois_loo$diagnostics$pareto_k > 0.5))
sum(pois_loo$diagnostics$pareto_k > 1)
scales::percent(mean(pois_loo$diagnostics$pareto_k > 1))

cbind(
  pois_loo$estimates,
  nb_loo$estimates
) %>%
  `colnames<-`(c("Poisson\nEstimate", "Poisson\nSE",
                 "Negative binomial\nEstimate", "Negative binomial\nSE")) %>%
  kable(digits = c(0, 0, 1, 3), format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))

colors <- c('low' = '#baccd9', 'medium' = '#7793a8', 'high' = '#03396c')
pois_loo$diagnostics$pareto_k %>% 
  enframe() %>% 
  mutate(color = case_when(
    value < 0.5 ~ 'low',
    value < 1 ~ 'medium',
    TRUE ~ 'high',
  )) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_point(alpha = 0.8, aes(color = color)) +
  geom_hline(yintercept = 1, color = 'black') +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  scale_color_manual(values = colors, guide = FALSE) +
  labs(title = "PSIS diagnostic: Poisson model",
       x = 'Data point (index)',
       y = 'Pareto shape k')
save_plot('PSIS_pois')

pp_check(post_pois, type = "loo_intervals") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Poisson in-sample performance',
       subtitle = paste0('Training dataset of ', nrow(Citibike_train), ' observations'),
       y = 'Daily Citi Bike trips')
save_plot(name = 'Pois_preds', plot = gg_nb_pred, height = 4.5)




# Prediction comparison ---------------------------------------------------


pois_preds <- posterior_predict(post_pois, newdata = Citibike_test)
pois_means <- colMeans(pois_preds) %>% enframe()

# print RMSE for each model
rbind(
  RMSE(nb_means$value, Citibike_test$Trip_count),
  RMSE(pois_means$value, Citibike_test$Trip_count)
) %>%
  scales::comma() %>%
  as.data.frame() %>%
  'colnames<-'('RMSE') %>%
  'rownames<-'(c('Negative binomial', 'Poisson')) %>%
  kable(digits = 0, format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))

  
colors <- c("Pred_fill" = "#d1e0eb", "Pred_stroke" = "#b3cddf", 
            "Y_fill" = '#03396c', "Y_stroke" = '#011f4a')

# plot the predictions similar to loo_intervals
gg_pois_pred <- as_tibble(pois_preds) %>%
  setNames(1:ncol(pois_preds)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = as.numeric(name)) %>%
  ggplot(aes(x = name, y = value, group = name)) +
  geom_boxplot(aes(color = "Pred_stroke"), alpha = 0.5, outlier.shape = NA) +
  geom_point(data = pois_means, aes(y = value, x = 1:ncol(pois_preds), fill = 'Pred_fill', color = "Pred_stroke"),
             size = 3, stroke = 2, shape = 21) +
  geom_point(data = Citibike_test, aes(y = Trip_count, x = 1:ncol(pois_preds), group = 1:ncol(pois_preds),
                                       fill = 'Y_fill',  color = 'Y_stroke'), size = 2, shape = 21) +
  scale_fill_manual(values = colors, guide = FALSE) +
  scale_colour_manual(values = colors, labels = c(expression('y'[rep]), "y")) +
  scale_y_continuous(limits = c(0, 180000), labels = NULL) +
  labs(title = "",
       subtitle = "",
       x = 'Data point (index)',
       y = NULL) +
  theme(plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.3, 0.1), "cm"))
  

# renew plot title
gg_nb_pred <- gg_nb_pred +
  labs(title = "Neg. binom. (L) vs. poisson (R) out-of-sample performance",
       x = 'Data point (index)',
       y = 'Daily Citi Bike trips') +
  theme(plot.background = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 0.3, 0.5), "cm"))

plot <- gridExtra::grid.arrange(gg_nb_pred, gg_pois_pred, ncol = 2)
save_plot(name = 'Pred_comparison', plot = plot)
rm(plot)


# conclusion --------------------------------------------------------------

scales::comma(mean(Citibike$Trip_count))
scales::comma(var(Citibike$Trip_count))
scales::comma(RMSE(nb_means$value, Citibike_test$Trip_count))

# Frequentist -------------------------------------------------------------

nb_freq <- MASS::glm.nb(Trip_count ~ Weekday + Precipitation + Temp + Gust_speed, 
                        data = Citibike_train)

summary(nb_freq)$coef %>%
  rbind(matrix(c(nb_freq$theta, NA, NA, NA), 
               nrow = 1, 
               dimnames = list('shape', c('Estimate', 'Std. Error', 'z value', 'Pr(>|z|)')))) %>% 
  kable(digits = c(2, 2, 2, 4), format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))

nb_freq_oos_preds <- predict(nb_freq, newdata = Citibike_test, type = 'response', se.fit = TRUE) %>% 
  as_tibble() %>% 
  mutate(lower = fit - (3 * se.fit),
         upper = fit + (3 * se.fit))

# plot the predictions 
nb_freq_oos_preds %>%
  mutate(name = 1:nrow(nb_freq_oos_preds),
         Y = Citibike_test$Trip_count) %>% 
  ggplot(aes(x = name)) +
  geom_point(aes(y = fit, fill = 'Pred_fill', color = "Pred_stroke"),
             size = 3, stroke = 2, shape = 21) +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = colors['Pred_stroke']) +
  geom_point(aes(y = Y, fill = 'Y_fill',  color = 'Y_stroke'), size = 2, shape = 21) +
  scale_fill_manual(values = colors, guide = FALSE) +
  scale_colour_manual(values = colors, labels = c(expression(hat(y)), "y")) +
  scale_y_continuous(limits = c(0, 180000), labels = scales::comma) +
  labs(title = "Frequentist negative binomial out-of-sample performance",
       subtitle = "Error bars represent +/- 3 standard error",
       x = 'Data point (index)',
       y = 'Daily Citi Bike trips')
save_plot('Freq_preds', height = 4.5)

# print RMSE for each model
rbind(
  RMSE(nb_freq_oos_preds$fit, Citibike_test$Trip_count),
  RMSE(nb_means$value, Citibike_test$Trip_count),
  RMSE(pois_means$value, Citibike_test$Trip_count)
) %>%
  scales::comma() %>%
  as.data.frame() %>%
  'colnames<-'('RMSE') %>%
  'rownames<-'(c('Negative binomial - Frequentist', 'Negative binomial - Bayesian', 'Poisson - Bayesian')) %>%
  kable(digits = 0, format.args = list(big.mark = ",", scientific = FALSE)) %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"))



# perks of bayesian -------------------------------------------------------

# Posterior probability of treatment effect > 0
(prob_25 <- round(hypothesis(post_nb, "Weekday > 0.25")[["hypothesis"]][["Post.Prob"]], 2))

p <- post_nb %>%
  as_tibble() %>% 
  ggplot(aes(x = b_Weekday)) +
  geom_density()
d <- ggplot_build(p)$data[[1]]
vline <- 0.25
p + 
  geom_area(data = subset(d, x > vline), aes(x = x, y = y), fill = blog_color, alpha = 0.6) +
  annotate('text', x = 0.245, y = 5, label = prob_25, hjust = -1, family = 'Helvetica') +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = NULL) + 
  labs(title = 'Distribution of posterior weekday estimates',
       x = NULL,
       y = NULL)
save_plot(name = 'weekday_probability')
rm(p, d)


# preds example
preds_example <- posterior_predict(post_nb,
                                   newdata = tibble(
                                     Weekday = 0,
                                     Precipitation = 1,
                                     Temp = 60,
                                     Gust_speed = 10
                                   )) %>% as_tibble()
p <- preds_example %>%
  ggplot(aes(x = V1)) +
  geom_density()
d <- ggplot_build(p)$data[[1]]
vline <- 40000
p + 
  geom_area(data = subset(d, x > vline), aes(x = x, y = y), fill = blog_color, alpha = 0.6) +
  annotate('text', x = 41500, y = 0.000005, label = round(mean(preds_example$V1 > vline), 2), hjust = 0, family = 'Helvetica') +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = NULL) + 
  labs(title = 'Density of samples from the posterior distribution',
       subtitle = 'Rainy, 60F weekend day with a gust speed of 10mph',
       x = 'Predicted daily Citi Bike trips',
       y = NULL)
save_plot(name = 'probability')
rm(p, d)

# se -> quantiles
se1 <- 0.001
se2 <- se1 + 0.021
se3 <- se2 + 0.136  

# plot of relative errors
as_tibble(nb_preds) %>% 
  setNames(1:ncol(nb_preds)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(type = 'Negative binomial - Bayesian') %>% 
  bind_rows(as_tibble(pois_preds) %>% 
              setNames(1:ncol(nb_preds)) %>%
              pivot_longer(cols = everything()) %>% 
              mutate(type = 'Poisson - Bayesian'),
            ) %>% 
  group_by(type, name) %>% 
  summarize(fit = mean(value),
            lower3 = quantile(value, se1),
            lower2 = quantile(value, se2),
            lower1 = quantile(value, se3),
            upper3 = quantile(value, 1 - se1),
            upper2 = quantile(value, 1 - se2),
            upper1 = quantile(value, 1 - se3)) %>% 
  ungroup() %>% 
  mutate(name = as.integer(name)) %>% 
  arrange(name) %>% 
  bind_rows(nb_freq_oos_preds %>% 
              mutate(
                lower3 = fit - (3*se.fit),
                lower2 = fit - (2*se.fit),
                lower1 = fit - (1*se.fit),
                upper3 = fit + (3*se.fit),
                upper2 = fit + (2*se.fit),
                upper1 = fit + (1*se.fit),
                name = 1:nrow(nb_freq_oos_preds),
                type = 'Negative binomial - Frequentist'
              ) %>% 
              select(type, name, fit, lower3, lower2, lower1, upper3, upper2, upper1)) %>% 
  arrange(type, name) %>% 
  mutate(Y = rep(Citibike_test$Trip_count, 3),
         fit = fit / Y - 1,
         lower3 = lower3 / Y - 1,
         lower2 = lower2 / Y - 1,
         lower1 = lower1 / Y - 1,
         upper3 = upper3 / Y - 1,
         upper2 = upper2 / Y - 1,
         upper1 = upper1 / Y - 1,
         type = factor(type, levels = c('Negative binomial - Frequentist', 'Negative binomial - Bayesian', 'Poisson - Bayesian'))) %>%
  ggplot(aes(x = reorder(name, fit))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower3, ymax = upper3), color = blog_color, alpha = 0.3) +
  geom_errorbar(aes(ymin = lower2, ymax = upper2), color = blog_color, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower1, ymax = upper1), color = blog_color, alpha = 0.9) +
  geom_point(aes(y = fit), fill = blog_color) +
  scale_x_discrete(label = NULL) +
  facet_wrap(~type, ncol = 1) +
  coord_flip(ylim = c(-1, 3)) +
  labs(title = 'Relative prediction error of each method',
       subtitle = 'Frequentist range represents +/- 1, 2, 3 standard error\nBayesian range represents the 68.4%, 95.6%, 99.8% credible interval',
       x = 'Individual predictions',
       y = expression(paste(plain('Relative error ('),
                            hat(y),
                            ' ',
                            plain(' / y - 1)')))) +
  theme(panel.grid.major.y = element_line(color = NA),
        panel.grid.minor.x = element_line(color = 'grey95'))
save_plot(name = 'relative_error', height = 10)

