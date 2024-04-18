# Inspired from https://lindeloev.github.io/mcp/ and https://www.niamhcahill.com/post/cptutorial/

# Load necessary libraries
library(mcp)
library(ggplot2)
library(runjags)
library(coda)
library(gridExtra)
library(extrafont)

# Import fonts and ensure they are loaded
font_import(pattern = "lmroman*") 
loadfonts()

# Set the seed for reproducibility
set.seed(2024)

# JAGS model for a simple linear change-point scenario
sim_level <- "
model {
  for (i_ in 1:N) {
    # Min and segment calculations
    X_1_[i_] = min(x[i_], cp_1)
    X_2_[i_] = min(x[i_], cp_2) - cp_1
    
    # Fitted value as piecewise function
    y_[i_] = (x[i_] >= cp_0) * (x[i_] < cp_1) * int_1 + 
             (x[i_] >= cp_1) * int_2 

    # Fitted standard deviation
    sigma_[i_] = max(0, (x[i_] >= cp_0) * sigma_1 )

    # Data likelihood
    y[i_] ~ dnorm((y_[i_]), 1 / sigma_[i_]^2)  # SD as precision
  }
}
"

# Data setup for the simple linear model
N <- 80
x_values <- sort(runif(N, 0, 100))
data_list_level <- list(x = x_values, N = N, cp_0 = min(x_values), cp_2 = max(x_values), cp_1 = 50,
                        int_1 = 0, int_2 = 20, sigma_1 = 1)

# Run the JAGS model
results_level <- run.jags(sim_level, data = data_list_level, monitor = c("y"), n.chains = 1, sample = 1, summarise = TRUE)
sim_dat_level <- as.mcmc(results_level)

# Prepare data for plotting
df_level = data.frame(x = x_values, y = as.numeric(sim_dat_level))
plot_level = ggplot(df_level, aes(x = x, y = y)) + geom_point() + geom_line() + theme_minimal() + 
  labs(x = "Distance", y = "Measurement") + theme(text = element_text(size = 12, family = "LM Sans 10"))

#####################
# Define JAGS model for lines
sim_lines <- "
model {
  # Min and segment calculations
  for (i_ in 1:N) {
    X_1_[i_] = min(x[i_], cp_1)
    X_2_[i_] = min(x[i_], cp_2) - cp_1
    X_3_[i_] = min(x[i_], cp_3) - cp_2
    
    # Fitted value
    y_[i_] = 
      
      # Segment 1: y ~ 1
      (x[i_] >= cp_0) * int_1 + 
      
      # Segment 2: y ~ 1 ~ 0 + x
      (x[i_] >= cp_1) * x_2 * X_2_[i_] + 
      
      # Segment 3: y ~ 1 ~ 0
      0 
    
    # Fitted standard deviation
    sigma_[i_] = max(0, 
                     (x[i_] >= cp_0) * sigma_1 )
    
    # Likelihood 
    y[i_] ~ dnorm((y_[i_]), 1 / sigma_[i_]^2)  # SD as precision
  }
}
"

# Data setup for the lines model
N <- 80
x_values <- sort(runif(N, 0, 100))
data_list_lines <- list(x = x_values, N = N,
                        cp_0 = min(x_values), cp_3 = max(x_values),
                        cp_1 = 30, cp_2 = 70,
                        int_1 = 0,
                        x_2 = 0.5,
                        sigma_1 = 1)

# Run the JAGS model for lines
results_lines <- run.jags(sim_lines, 
                          data = data_list_lines,
                          monitor = c("y"),
                          n.chains = 1, 
                          sample = 1,
                          summarise = TRUE)
sim_dat_lines <- coda::as.mcmc(results_lines)

# Prepare data for plotting lines model
df_lines = data.frame(x=x_values, y = as.numeric(sim_dat_lines))
plot_lines = ggplot(df_lines, aes(x=x, y=y)) + 
  geom_point() + geom_line(color = "black") + 
  theme_minimal() + 
  labs(x = "distance") +
  scale_y_continuous(sec.axis = dup_axis(name = "measurement")) +
  theme(text = element_text(size=12, family="LM Sans 10"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(color = "black"),
        #axis.ticks.y.right = element_line(color = "black"),
        axis.title.y.right = element_text(color = "black", angle = 270, vjust = 0.5))

#############################

# Define JAGS model for variance changes
sim_variance <- "
model {
  # Min and segment calculations
  for (i_ in 1:N) {
    X_1_[i_] = min(x[i_], cp_1)
    X_2_[i_] = min(x[i_], cp_2) - cp_1
    X_3_[i_] = min(x[i_], cp_3) - cp_2
    
    # Fitted value
    y_[i_] = 
    
      # Segment 1: y ~ 1
      (x[i_] >= cp_0) * (x[i_] < cp_2) * int_1 + 
    
      # Segment 2: y ~ 1 ~ 0 + sigma(1)
      0 + 
    
      # Segment 3: y ~ 1 ~ 1 + sigma(1)
      (x[i_] >= cp_2) * int_3 
    
    # Fitted standard deviation
    sigma_[i_] = max(0, 
      (x[i_] >= cp_0) * (x[i_] < cp_1) * sigma_1 + 
      (x[i_] >= cp_1) * (x[i_] < cp_2) * sigma_2 + 
      (x[i_] >= cp_2) * sigma_3 )

    # Likelihood 
    y[i_] ~ dnorm((y_[i_]), 1 / sigma_[i_]^2)  # SD as precision
  }
}
"

# Data setup for the variance model
N <- 80
x_values <- sort(runif(N, 0, 100))
data_list_variance <- list(x = x_values, N = N,
                           cp_0 = min(x_values), cp_3 = max(x_values),
                           cp_1 = 30, cp_2 = 70,
                           int_1 = 30, int_3 = 30,
                           sigma_1 = 1, sigma_2 = 10, sigma_3 = 1)

# Run the JAGS model for variance
results_variance <- run.jags(sim_variance, 
                             data = data_list_variance,
                             monitor = c("y"),
                             n.chains = 1, 
                             sample = 1,
                             summarise = TRUE)
sim_dat_variance <- coda::as.mcmc(results_variance)

# Prepare data for plotting variance model
df_variance = data.frame(x=x_values, y = as.numeric(sim_dat_variance))
plot_variance = ggplot(df_variance, aes(x=x, y=y)) + 
  geom_point() + geom_line(color = "black") + 
  theme_minimal() + 
  labs(x = "distance", y = "measurement") +
  theme(text = element_text(size=12, family="LM Sans 10"))

#############################

# Define JAGS model for changing variance
sim_ch.variance <- "
model {
  # Min and segment calculations
  for (i_ in 1:N) {
    X_1_[i_] = min(x[i_], cp_1)
    X_2_[i_] = min(x[i_], cp_2) - cp_1
    X_3_[i_] = min(x[i_], cp_3) - cp_2
    
    # Fitted value
    y_[i_] = 
    
      # Segment 1: y ~ 1
      (x[i_] >= cp_0) * (x[i_] < cp_2) * int_1 + 
    
      # Segment 2: y ~ 1 ~ 0 + sigma(1 + x)
      0 + 
    
      # Segment 3: y ~ 1 ~ 1 + sigma(1)
      (x[i_] >= cp_2) * int_3 
    
    # Fitted standard deviation
    sigma_[i_] = max(0, 
      (x[i_] >= cp_0) * (x[i_] < cp_1) * sigma_1 + 
      (x[i_] >= cp_1) * (x[i_] < cp_2) * sigma_2 + 
      (x[i_] >= cp_1) * (x[i_] < cp_2) * sigma_x_2 * X_2_[i_] + 
      (x[i_] >= cp_2) * sigma_3 )

    # Likelihood 
    y[i_] ~ dnorm((y_[i_]), 1 / sigma_[i_]^2)  # SD as precision
  }
}
"

# Data setup for the changing variance model
N <- 200
x_values <- sort(runif(N, 0, 100))
data_list_ch.variance <- list(x = x_values, N = N,
                              cp_0 = min(x_values), cp_3 = max(x_values),
                              cp_1 = 30, cp_2 = 60, int_1 = 300, int_3 = 300,
                              sigma_1 = 1, sigma_x_2 = 1.1,
                              sigma_2 = 1.1, sigma_3 = 1)

# Run the JAGS model for changing variance
results_ch.variance <- run.jags(sim_ch.variance, 
                                data = data_list_ch.variance,
                                monitor = c("y"),
                                n.chains = 1, 
                                sample = 1,
                                summarise = TRUE)
sim_dat_ch.variance <- coda::as.mcmc(results_ch.variance)

# Prepare data for plotting changing variance model
df_ch.variance = data.frame(x=x_values, y = as.numeric(sim_dat_ch.variance))
plot_ch_variance = ggplot(df_ch.variance, aes(x=x, y=y)) + 
  geom_point() + geom_line(color = "black") + 
  theme_minimal() + 
  labs(x = "distance") +
  scale_y_continuous(sec.axis = dup_axis(name = "measurement")) +
  theme(text = element_text(size=12, family="LM Sans 10"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(color = "black"),
        #axis.ticks.y.right = element_line(color = "black"),
        axis.title.y.right = element_text(color = "black", angle = 270, vjust = 0.5))

####################

# Define JAGS model for trigonometric functions
sim_trigo <- "
model {
  for (i_ in 1:N) {
    # Min and segment calculations
    X_1_[i_] = min(x[i_], cp_1)
    X_2_[i_] = min(x[i_], cp_2) - cp_1
    X_3_[i_] = min(x[i_], cp_3) - cp_2
    
    # Fitted value
    y_[i_] = 
      
      # Segment 1: y ~ 1 + sin(x)
      (x[i_] >= cp_0) * (x[i_] < cp_1) * int_1 + 
      (x[i_] >= cp_0) * (x[i_] < cp_1) * x_1_sin * sin(5*X_1_[i_]) + 
      
      # Segment 2: y ~ 1 ~ 1 + sin(x)
      (x[i_] >= cp_1) * (x[i_] < cp_2) * int_2 + 
      (x[i_] >= cp_1) * (x[i_] < cp_2) * x_2_sin * sin(0.5*X_2_[i_]) + 
      
      # Segment 3: y ~ 1 ~ 1 + sin(x)
      (x[i_] >= cp_2) * int_3 + 
      (x[i_] >= cp_2) * x_3_sin * sin(5*X_3_[i_]) 
    
    # Fitted standard deviation
    sigma_[i_] = max(0, 
                     (x[i_] >= cp_0) * sigma_1 )
    
    # Data likelihood
    y[i_] ~ dnorm(y_[i_], 1 / sigma_[i_]^2);
  }
}
"

# Data setup for the trigonometric model
N <- 200
x_values <- sort(runif(N, 0, 100))
data_list_trigo <- list(x = x_values, N = N,
                  cp_0 = min(x_values), cp_3 = max(x_values),
                  cp_1 = 20, cp_2 = 80,
                  int_1 = 10, int_2 = 10, int_3 = 10,
                  x_1_sin = 3, x_2_sin = 6, x_3_sin = 3,
                  sigma_1 = 0.1)

# Run the JAGS model for trigonometric functions
results_trigo <- run.jags(sim_trigo, 
                          data = data_list_trigo,
                          monitor = c("y"),
                          n.chains = 1, 
                          sample = 1,
                          summarise = TRUE)
sim_dat_trigo <- coda::as.mcmc(results_trigo)

# Prepare data for plotting trigonometric functions model
df_trigo = data.frame(x=x_values, y = as.numeric(sim_dat_trigo))
plot_trigo = ggplot(df_trigo, aes(x=x, y=y)) + 
  geom_point() + geom_line(color = "black") + 
  theme_minimal() + 
  labs(x = "distance", y = "measurement") +
  theme(text = element_text(size=12, family="LM Sans 10"))

####################

# Define JAGS model for autoregressive model
sim_ar <- "
model {
  # Apply autoregression to the residuals
  resid_arma_[1] = 0
  resid_arma_[2] = ar1_[2 - 1] * resid_abs_[2 - 1]
  for (i_ in 3:N) {
    resid_arma_[i_] = 0 + 
      ar1_[i_] * resid_abs_[i_ - 1] + 
      ar2_[i_] * resid_abs_[i_ - 2]
  }
  
  # Min and segment calculations
  for (i_ in 1:N) {
    X_1_[i_] = min(x[i_], cp_1)
    X_2_[i_] = min(x[i_], cp_2) - cp_1
    X_3_[i_] = min(x[i_], cp_3) - cp_2
    
    # Fitted value
    y_[i_] = 
    
      # Segment 1: price ~ 1 + ar(1)
      (x[i_] >= cp_0) * int_1 + 
    
      # Segment 2: price ~ 1 ~ 0 + x + ar(2)
      (x[i_] >= cp_1) * x_2 * X_2_[i_] + 
    
      # Segment 3: price ~ 1 ~ 0 + ar(1)
      0 
    
    # Fitted standard deviation
    sigma_[i_] = max(0, 
      (x[i_] >= cp_0) * sigma_1 )
    
    # Autoregressive coefficient for all AR(1)
    ar1_[i_] = 
      (x[i_] >= cp_0) * (x[i_] < cp_1) * ar1_1 + 
      (x[i_] >= cp_1) * (x[i_] < cp_2) * ar1_2 + 
      (x[i_] >= cp_2) * ar1_3 
    
    # Autoregressive coefficient for all AR(2)
    ar2_[i_] = 
      (x[i_] >= cp_1) * ar2_2 

    # Likelihood 
    price[i_] ~ dnorm((y_[i_] + resid_arma_[i_]), 1 / sigma_[i_]^2)  # SD as precision
    resid_abs_[i_] = (price[i_])  - y_[i_]  # Residuals represented by sigma_ after ARMA
  }
}
"

# Data setup for the autoregressive model
N <- 200
x_values <- sort(runif(N, 0, 100))
data_list_ar <- list(x = x_values, N = N,
                     cp_0 = min(x_values), cp_3 = max(x_values),
                     cp_1 = 30,
                     cp_2 = 70,
                     int_1 = 50,
                     x_2 = -0.3,
                     ar1_1 = 0.2, ar1_2 = 0.8, ar2_2 = -0.5, ar1_3 = 0.2, 
                     sigma_1 = 2)

# Run the JAGS model for ar model
results_ar <- run.jags(sim_ar, 
                       data = data_list_ar,
                       monitor = c("price"),
                       n.chains = 1, 
                       sample = 1,
                       summarise = TRUE)
sim_dat_ar <- coda::as.mcmc(results_ar)

# Prepare data for plotting trigonometric functions model
df_ar = data.frame(x=x_values, y = as.numeric(sim_dat_ar))
plot_ar = ggplot(df_ar, aes(x=x, y=y)) + 
  geom_point() + geom_line(color = "black") + 
  theme_minimal() + 
  labs(x = "distance") +
  scale_y_continuous(sec.axis = dup_axis(name = "measurement")) +
  theme(text = element_text(size=12, family="LM Sans 10"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(color = "black"),
        #axis.ticks.y.right = element_line(color = "black"),
        axis.title.y.right = element_text(color = "black", angle = 270, vjust = 0.5))

####################

# Annotate plots with labels
plot_level <- plot_level + 
  annotate("text", x = Inf, y = Inf, label = "a", hjust = 1.1, vjust = 1.1, size = 5, color = "black")

plot_lines <- plot_lines + 
  annotate("text", x = Inf, y = Inf, label = "b", hjust = 1.1, vjust = 1.1, size = 5, color = "black")

plot_variance <- plot_variance + 
  annotate("text", x = Inf, y = Inf, label = "c", hjust = 1.1, vjust = 1.1, size = 5, color = "black")

plot_ch_variance <- plot_ch_variance + 
  annotate("text", x = Inf, y = Inf, label = "d", hjust = 1.1, vjust = 1.1, size = 5, color = "black")

plot_trigo <- plot_trigo + 
  annotate("text", x = Inf, y = Inf, label = "e", hjust = 1.1, vjust = 1.1, size = 5, color = "black")

plot_ar <- plot_ar + 
  annotate("text", x = Inf, y = Inf, label = "f", hjust = 1.1, vjust = 1.1, size = 5, color = "black")

# Arrange and display all plots in a grid 
grid.arrange(plot_level, plot_lines, plot_variance, plot_ch_variance, plot_trigo, plot_ar, ncol = 2, nrow = 3)
