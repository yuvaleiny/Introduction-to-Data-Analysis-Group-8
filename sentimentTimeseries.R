required_packages <- c("tidyverse", "lubridate", "forecast", "ggplot2", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(gridExtra)

CSV_PATH     <- "C:/Users/mikab/OneDrive/Desktop/second year/4 semester/data analysis/FinalSentiment.csv"
FREQ         <- "week"
MIN_ARTICLES <- 3
SMOOTH_WEEKS <- 4

EVENTS <- list(
  "2023-10-07" = "7/10 start",
  "2023-11-24" = "hostages deal",
  "2024-04-14" = "Iran attack 1",
  "2024-09-27" = "Nasrallah",
  "2024-10-01" = "Iran attack 2",
  "2025-01-19" = "ceasefire"
)
OCT7 <- as.Date("2023-10-07")

LABEL_MAP <- c("positive" = 1, "pos" = 1, "חיובי" = 1,
               "neutral" = 0, "neu" = 0, "ניטרלי" = 0,
               "negative" = -1, "neg" = -1, "שלילי" = -1)

load_and_build_series <- function() {
  cat("[A] טוען נתונים...\n")
  df <- read_csv(CSV_PATH, locale = locale(encoding = "UTF-8"))
  
  
  date_col <- if ("date" %in% colnames(df)) "date" else colnames(df)[grep("date|Date|תאריך", colnames(df))[1]]
  source_col <- if ("source_name" %in% colnames(df)) "source_name" else colnames(df)[1]
  
  
  df_filtered <- df %>%
    mutate(
      _date = dmy(!!sym(date_col)), # תומך ב-dayfirst=True
      פוליטיקה_clean = trimws(as.character(פוליטיקה)),
      בטחון_clean = trimws(as.character(בטחון))
    ) %>%
    # סינון שורות פוליטיקה או ביטחון
    filter(פוליטיקה_clean == "כן" | בטחון_clean == "כן") %>%
    mutate(
      _val = LABEL_MAP[tolower(trimws(as.character(SentimentLabel)))],
      _conf = as.numeric(ConfidenceScore)
    ) %>%
    filter(!is.na(_date), !is.na(_val), !is.na(_conf)) %>%
    mutate(_conf = pmax(_conf, 0))
  
  df_weekly <- df_filtered %>%
    mutate(bucket = floor_date(_date, unit = FREQ)) %>%
    group_by_("_site" = source_col, "bucket") %>%
    summarise(
      vw = sum(_val * _conf, na.rm = TRUE),
      w = sum(_conf, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(sentiment = ifelse(n < MIN_ARTICLES, NA, vw / w))
  
  wide <- df_weekly %>%
    select(_site, bucket, sentiment) %>%
    spread(key = _site, value = sentiment)
  
  full_dates <- data.frame(bucket = seq(min(wide$bucket), max(wide$bucket), by = FREQ))
  wide <- full_dates %>% left_join(wide, by = "bucket")
  
  for(col in colnames(wide)[-1]) {
    wide[[col]] <- zoo::na.approx(wide[[col]], na.rm = FALSE)
    # ffill ו-bfill בסיסיים
    wide[[col]] <- zoo::na.locf(wide[[col]], na.rm = FALSE)
    wide[[col]] <- zoo::na.locf(wide[[col]], fromLast = TRUE, na.rm = FALSE)
  }
  
  return(wide)
}

add_events_to_plot <- function(p, x_min, x_max) {
  for(d_str in names(EVENTS)) {
    d <- as.Date(d_str)
    if(d >= x_min && d <= x_max) {
      p <- p + geom_vline(xintercept = as.numeric(d), linetype = "dashed", color = "grey", alpha = 0.7)
    }
  }
  return(p)
}

plot_overview <- function(wide) {
  wide_long <- wide %>%
    gather(key = "site", value = "sentiment", -bucket) %>%
    group_by(site) %>%
    mutate(smoothed = zoo::rollapply(sentiment, SMOOTH_WEEKS, mean, fill = NA, align = "right"))
  
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed, color = site)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Net sentiment over time per site (confidence-weighted)", y = "net tone", x = "Date")
  
  p <- add_events_to_plot(p, min(wide$bucket), max(wide$bucket))
  ggsave("01_overview.png", plot = p, width = 14, height = 6.5, dpi = 130)
  cat("[D] נשמר: 01_overview.png\n")
}

plot_per_site <- function(wide) {
  wide_long <- wide %>%
    gather(key = "site", value = "sentiment", -bucket) %>%
    group_by(site) %>%
    mutate(smoothed = zoo::rollapply(sentiment, SMOOTH_WEEKS, mean, fill = NA, align = "right"))
  
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed)) +
    geom_line(color = "blue", size = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.4) +
    facet_wrap(~site, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(title = "Sentiment per site", x = "Date", y = "net tone")
  
  p <- add_events_to_plot(p, min(wide$bucket), max(wide$bucket))
  ggsave("02_per_site.png", plot = p, width = 14, height = 8, dpi = 130)
  cat("[D] נשמר: 02_per_site.png\n")
}

plot_oct7_window <- function(wide) {
  start_date <- OCT7 %m-% months(6)
  end_date <- OCT7 %m+% months(6)
  
  win <- wide filter(bucket >= start_date & bucket <= end_date)
  
  wide_long <- win %>%
    gather(key = "site", value = "sentiment", -bucket) %>%
    group_by(site) %>%
    mutate(smoothed = zoo::rollapply(sentiment, SMOOTH_WEEKS, mean, fill = NA, align = "right"))
  
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed, color = site)) +
    geom_line(size = 1) +
    geom_vline(xintercept = as.numeric(OCT7), color = "red", size = 1.2) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Sentiment around Oct 7 (6m before -> 6m after)", x = "Date", y = "net tone")
  
  p <- add_events_to_plot(p, start_date, end_date)
  ggsave("03_oct7_window.png", plot = p, width = 13, height = 6, dpi = 130)
  cat("[D] נשמר: 03_oct7_window.png\n")
}

plot_arima_vs_reality <- function(wide, site, event_date_str) {
  event_date <- as.Date(event_date_str)
  start_date <- event_date %m-% months(5)
  end_date   <- event_date %m+% months(5)
  
  s_window <- wide %>%
    select(bucket, !!sym(site)) %>%
    filter(bucket >= start_date & bucket <= end_date) %>%
    rename(sentiment = !!sym(site))
  
  train_data <- s_window %>% filter(bucket < event_date)
  actual_post <- s_window %>% filter(bucket >= event_date)
  
  model <- Arima(train_data$sentiment, order = c(1, 0, 1))
  
  forecast_steps <- nrow(actual_post)
  fcast <- forecast(model, h = forecast_steps, level = 95)
  
  forecast_df <- data.frame(
    bucket = actual_post$bucket,
    forecast_mean = as.numeric(fcast$mean),
    lower = as.numeric(fcast$lower),
    upper = as.numeric(fcast$upper)
  )
  
  s_window$smoothed <- zoo::rollapply(s_window$sentiment, 2, mean, fill = NA, align = "right")
  s_window$smoothed[is.na(s_window$smoothed)] <- s_window$sentiment[is.na(s_window$smoothed)]
  
  p <- ggplot() +
    geom_line(data = s_window, aes(x = bucket, y = smoothed, group = 1, color = "המציאות בפועל (Actual Tone)"), size = 1.2) +
    geom_line(data = forecast_df, aes(x = bucket, y = forecast_mean, group = 1, color = "תחזית ARIMA ללא האירוע"), linetype = "dashed", size = 1.2) +
    geom_ribbon(data = forecast_df, aes(x = bucket, ymin = lower, ymax = upper, fill = "טווח טעות סטטיסטי (95% CI)"), alpha = 0.15) +
    geom_vline(xintercept = as.numeric(event_date), linetype = "dotted", color = "black", size = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.4) +
    scale_color_manual(values = c("המציאות בפועל (Actual Tone)" = "#1f77b4", "תחזית ARIMA ללא האירוע" = "#d62728")) +
    scale_fill_manual(values = c("טווח טעות סטטיסטי (95% CI)" = "#d62728")) +
    theme_minimal() +
    labs(
      title = paste("ARIMA Forecast vs Reality:", site, "around 7/10"),
      y = "Net Tone (-1 negative ... +1 positive)", x = "Date",
      color = "מקרא קווים", fill = "שטחי ביטחון"
    )
  
  filename <- paste0("04_arima_vs_reality_", gsub(" ", "_", site), ".png")
  ggsave(filename, plot = p, width = 12, height = 6, dpi = 130)
  cat("[ARIMA Graph] נשמר בהצלחה גרף מבוסס מודל:", filename, "\n")
}

plot_all_events_impact <- function(wide) {
  sites <- colnames(wide)[-1]
  plot_list <- list()
  
  for(date_str in names(EVENTS)) {
    event_name <- EVENTS[[date_str]]
    event_date <- as.Date(date_str)
    
    event_results <- data.frame()
    
    for(site in sites) {
      s_vals <- wide[[site]]
      post_dummy <- as.numeric(wide$bucket >= event_date)
      
      if(sum(post_dummy) < 5 || sum(post_dummy == 0) < 5) next
      
      try({
        model <- Arima(s_vals, order = c(1, 0, 1), xreg = post_dummy)
        coef_val <- model$coef["xreg"]
        se_val <- sqrt(model$var.coef["xreg", "xreg"])
        pval <- 2 * (1 - pnorm(abs(coef_val / se_val)))
        
        event_results <- rbind(event_results, data.frame(
          site = site,
          coef = coef_val,
          error = 1.96 * se_val,
          significant = ifelse(pval < 0.05, "Significant", "Not Significant")
        ))
      }, silent = TRUE)
    }
    
    if(nrow(event_results) == 0) next
    
    p_sub <- ggplot(event_results, aes(x = coef, y = site, color = significant)) +
      geom_vline(xintercept = 0, color = "black", alpha = 0.4) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = coef - error, xmax = coef + error), height = 0.2, size = 1) +
      scale_color_manual(values = c("Significant" = "#d62728", "Not Significant" = "#7f7f7f")) +
      theme_minimal() +
      labs(title = paste("Immediate Impact:", event_name, "(", date_str, ")"), x = "", y = "") +
      theme(legend.position = "none")
    
    plot_list[[event_name]] <- p_sub
  }
  
  combined_plot <- grid.arrange(grobs = plot_list, ncol = 1)
  ggsave("05_arima_events_impact_comparison.png", plot = combined_plot, width = 12, height = 2.5 * length(plot_list), dpi = 130)
  cat("[ARIMA Graph] נשמר בהצלחה גרף מרוכז: 05_arima_events_impact_comparison.png\n")
}

wide_data <- load_and_build_series()

plot_overview(wide_data)
plot_per_site(wide_data)
plot_oct7_window(wide_data)

first_site_name <- colnames(wide_data)[2] # האתר הראשון בטבלה
plot_arima_vs_reality(wide_data, site = first_site_name, event_date_str = "2023-10-07")
plot_all_events_impact(wide_data)

cat("\n--- All R visualizations generated successfully! ---\n")