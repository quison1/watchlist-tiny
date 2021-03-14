func_load_packages <- function(pkg){
  # function to install and require packages
  # https://gist.github.com/stevenworthington/3178163
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c("tidyverse", "tidyquant")
func_load_packages(libs) # load packages

# datetime specification ----
today <- Sys.Date()
past_horizon_long <- Sys.Date() %m+% months(-36) #lubridate required
past_horizon_medium <- Sys.Date() %m+% months(-18) #lubridate required
past_horizon_short <- Sys.Date() %m+% months(-3) #lubridate required

# obtain ticket list, my-list ----

print(getwd())

filtered_list <- read.csv("rprog/simon-all.csv", header = F)[, 1] %>% as.character()

# download data from yahoo finance ----

yf_data <- filtered_list %>% 
  tq_get(get  = "stock.prices",
         from = past_horizon_long,
         to   = today) %>%
  drop_na()

temp2 <- bind_rows(yf_data)

glimpse(temp2)

FANG_macd <- temp2 %>% # yf_data %>% drop_na() %>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = SMA,
            percent    = F) %>%
  mutate(diff = macd - signal,
         signDiff = diff / abs(diff),
         signChange=c(NA, diff(signDiff)),
         # if signChange = -2 means sell, +2 means buy
         macd_buy = ifelse(signChange == 2 & macd < 0, 1, 0),
         macd_sell = ifelse(signChange == -2 & macd > 0, 1, 0)
  )

FANG_rsi <- temp2 %>% # yf_data %>% drop_na() %>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = RSI) %>%
  mutate(rsi_sell = ifelse(rsi > 70, 1, 0), # overbought signals sell
         rsi_buy = ifelse(rsi < 30, 1, 0) # oversold signals buy
  )

FANG <- left_join(FANG_macd, FANG_rsi)

plot_price <- function(chosen_ticker, starting_data) {
  FANG %>%
    filter(date >= as_date(starting_data)) %>%
    filter(symbol %in% chosen_ticker) %>%
    mutate(tradeSignal = macd_buy + macd_sell + rsi_buy + rsi_sell) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = close)) +
    # overlay with signals
    geom_vline(data = . %>% filter(macd_buy == 1), aes(xintercept = date),
               col = "maroon", alpha = 0.25, lty = 2, lwd = 1) +
    geom_vline(data = . %>% filter(macd_sell == 1), aes(xintercept = date),
               col = "darkgreen", alpha = 0.25, lty = 2, lwd = 1) +
    geom_vline(data = . %>% filter(rsi_buy == 1), aes(xintercept = date),
               col = "maroon", alpha = 0.25, lty = 1) +
    geom_vline(data = . %>% filter(rsi_sell == 1), aes(xintercept = date),
               col = "darkgreen", alpha = 0.25, lty = 1) +
    labs(y = "", x = "", color = "", title = chosen_ticker) +
    theme_tq() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
}

plot_macd <- function(chosen_ticker, starting_data) {
  FANG %>%
    filter(date >= as_date(starting_data)) %>%
    filter(symbol %in% chosen_ticker) %>%
    ggplot(aes(x = date)) + 
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(aes(y = macd), color = "orange") +
    geom_line(aes(y = signal), color = "blue") +
    # scale_x_date(date_labels = "%d", date_breaks = "1 week") +
    geom_vline(data = . %>% filter(macd_buy == 1), aes(xintercept = date),
               col = "maroon", alpha = 0.25, lty = 2, lwd = 1) +
    geom_vline(data = . %>% filter(macd_sell == 1), aes(xintercept = date),
               col = "darkgreen", alpha = 0.25, lty = 2, lwd = 1) +
    labs(y = "", x = "", color = "", title = chosen_ticker) +
    theme_tq() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
}

plot_rsi <- function(chosen_ticker, starting_date) {
  FANG %>%
    filter(date >= as_date(starting_date)) %>%
    filter(symbol %in% chosen_ticker) %>%
    ggplot(aes(x = date)) + 
    geom_line(aes(y = rsi), col="purple") +
    # scale_x_date(date_labels = "%d", date_breaks = "1 week") +
    geom_hline(yintercept = c(30, 70), color = "black") +
    geom_vline(data = . %>% filter(rsi_buy == 1), aes(xintercept = date),
               col = "maroon", alpha = 0.25, lty = 1) +
    geom_vline(data = . %>% filter(rsi_sell == 1), aes(xintercept = date),
               col = "darkgreen", alpha = 0.25, lty = 1) +
    labs(y = "", x = "", color = "", title = chosen_ticker) +
    coord_cartesian(ylim = c(10, 90)) +
    theme_tq() +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
}

export_pictures <- function(ticker, location = "") {
  ggsave(paste0(location, ticker, "-medium-price.png"), plot = plot_price(ticker, past_horizon_medium) + scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y"), width=16, height=8, units="cm", dpi = 120)
  ggsave(paste0(location, ticker, "-medium-macd.png"), plot = plot_macd(ticker, past_horizon_medium) + scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y"), width=16, height=8, units="cm", dpi = 120)
  ggsave(paste0(location, ticker, "-medium-rsi.png"), plot = plot_rsi(ticker, past_horizon_medium) + scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y"), width=16, height=8, units="cm", dpi = 120)
  ggsave(paste0(location, ticker, "-short-price.png"), plot = plot_price(ticker, past_horizon_short) + scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b"), width=16, height=8, units="cm", dpi = 120)
  ggsave(paste0(location, ticker, "-short-macd.png"), plot = plot_macd(ticker, past_horizon_short) + scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b"), width=16, height=8, units="cm", dpi = 120)
  ggsave(paste0(location, ticker, "-short-rsi.png"), plot = plot_rsi(ticker, past_horizon_short) + scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b"), width=16, height=8, units="cm", dpi = 120)
}

library(readr)

location_asset <- "_assets/"
files_asset <- list.files(location_asset)
sapply(paste0(location_asset, files_asset), file.remove)

location_project <- "_projects/"
files_project <- list.files(location_project)
sapply(paste0(location_project, files_project), file.remove)

for (ticker in filtered_list) {
  export_pictures(ticker, location_asset)
  
  temp <- read_file("rprog/template.md")
  temp <- str_replace_all(temp, "ticker", ticker)
  readr::write_lines(temp, path = paste0(location_project, ticker, ".md"))
}

