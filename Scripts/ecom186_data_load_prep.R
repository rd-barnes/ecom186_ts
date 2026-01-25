# Load Packages

library(here)
library(data.table)
library(ggplot2)

library(urca)
library(tseries)
library(zoo)
library(vars)
library(sovereign)

### Load Data

wd <- here()
var_ts <- fread(paste0(wd, "/Data/ecom186_ts_data.csv"))
var_ts <- var_ts[, .(`date`, `Target`, `Path`, `QE`, `bank_rate`, `gdp`, `cpi`, `ftse_all_share`, `neer`, `m4_lending`, `ew_scot_insol_rate`)]
setcolorder(var_ts, "date")
var_ts$date <- as.Date(var_ts$date, format = "%d/%m/%Y")

### Add Covid dummy
timeseries_dates <- seq(as.Date("2000-01-01"), as.Date("2025-12-01"), by = "month")
covid_dates <- ifelse(timeseries_dates >= as.Date("2020-03-01") & timeseries_dates <= as.Date("2022-08-01"), 1, 0)
var_ts$covid_dummy <- covid_dates
var_ts <- na.omit(var_ts)

### Visualise variables

var_ts_plot <- melt(var_ts, id.vars = "date")

ggplot(data = var_ts_plot, aes(x = `date`, y = `value`)) +
  geom_line(stat = "identity", aes(group = `variable`)) +
  ggplot2::theme(axis.title.x.top = element_text(hjust = 0, vjust = 1),
                 legend.box.just = "left",
                 legend.box.margin = margin(-5, 0, -10, 0),
                 plot.margin = margin(10, 10, 10, 10),
                 legend.text.align = 0,
                 legend.justification = "left",
                 legend.position = "top",
                 strip.text = element_text(size = 9),
                 
                 plot.background = element_rect(fill = "white", colour = "white"),
                 legend.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white"),
                 strip.background = element_rect(fill= "white"),
                 panel.grid.major.y = element_line(colour = "#C4C9CE"),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.ticks = element_line(color = "white", linewidth = 0.5),
                 
                 legend.text = element_text(colour = "black", family = "sans", size = 11),
                 axis.text = element_text(colour = "black", family = "sans", size = 11),
                 strip.text.x = element_text(colour = "black", family = "sans"),
                 plot.title = element_text(colour = "black", family = "sans", face = "bold"),
                 plot.subtitle = element_text(colour = "black", family = "sans", face = "bold"),
                 plot.caption = element_text(colour = "black", family = "sans"),
                 axis.title = element_text(colour = "black", family = "sans", size = 11)
  ) +
  ggplot2::labs(y = NULL,
                x = NULL,
                title = "Key VAR variables (Jan 2000 - Sept 2025)",
                subtitle = NULL,
                caption = NULL) +
  facet_wrap(~variable, scales = "free")

### Variable transformations

# BoE Paper includes everything in log levels...
# I think I need another brain on what the appropriate transformations are for this case...

var_ts$log_gdp <- log(var_ts$gdp)
var_ts$log_cpi <- log(var_ts$cpi)
var_ts$log_ftse <- log(var_ts$ftse_all_share)
var_ts$log_insol <- log(var_ts$ew_scot_insol_rate)
var_ts$log_neer <- log(var_ts$neer)
var_ts$log_m4 <- log(var_ts$m4_lending)

var_ts <- na.omit(var_ts)

### Implement SVAR

# basic_var <- var_ts[`date` < "2020-01-01", .(`date`, `bank_rate`, `log_gdp`, `log_neer`, `log_ftse`, `Target`)]

# I get RAM problems here, so going to need to find a workaround
var_model <- sovereign::VAR(
  data = basic_var,
  freq = "month",
  p = 6,
  type = "const",
  structure = "IV",
  instrument = "Target",
  instrumented = "bank_rate"
)

irf_out <- sovereign::var_irf(
  var_model,
  horizon = 36,
  c(0.1, 0.9)
)

ggplot(data = irf_out[,], aes(x = horizon, y = response)) +
  geom_line(aes(group = target)) +
  geom_ribbon(
    aes(ymin = response.lower, ymax = response.upper),
    fill = "steelblue",
    alpha = 0.25
  ) +
  facet_wrap(~target)


### Testing Instrument Strength

# Extract reduced-form residuals
U <- resid(var_model)

# Policy rate residual (instrumented equation)
u_mp <- U$H_1[, "bank_rate"]   # or U[, 1]

z <- basic_var$Target   # your HFI monetary policy shock

# Keep common sample
valid <- !is.na(z)
u_mp <- u_mp[valid]
z <- z[valid]

fs <- lm(u_mp ~ z)
summary(fs)

library(sandwich)
library(lmtest)

fs_robust <- coeftest(fs, vcov = vcovHC(fs, type = "HC1"))
fs_robust
waldtest(fs, vcov = vcovHC(fs, type = "HC1"))
R2 <- summary(fs)$r.squared
R2

plot(z, u_mp,
     xlab = "High-Frequency Monetary Policy Shock",
     ylab = "Policy Rate Residual",
     main = "First-Stage Relationship")
abline(fs, col = "blue", lwd = 2)

svar_proxy <- id.iv(
  x = var_model,
  z = proxy,
  shock = 3
)

##### Working - Obsolete and Testing Code ####

# var_ts$gdp_gr <- c(NA, diff(log(var_ts$gdp)*100))
# var_ts$cpi_gr <- c(NA, diff(log(var_ts$cpi)*100))
# var_ts$ftse_gr <- c(NA, diff(log(var_ts$ftse_all_share)*100))
# var_ts$log_insol_r <- log(var_ts$ew_scot_insol_rate)

#ts_data <- ts(var_ts[, -1], start = c(2000, 1), frequency = 12)