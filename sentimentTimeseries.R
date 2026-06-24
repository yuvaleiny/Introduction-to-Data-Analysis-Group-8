required_packages <- c("tidyverse", "data.table", "lubridate")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(data.table)
library(lubridate)

CSV_PATH <- "C:/Users/mikab/OneDrive/Desktop/second year/4 semester/data analysis/project/DataFinished.csv"

FREQ <- "week"
MIN_ARTICLES <- 3
SMOOTH_WEEKS <- 4

EVENTS <- list(
  "2023-10-07" = "7_10 start",
  "2023-11-24" = "hostages deal",
  "2024-04-14" = "Iran attack 1",
  "2024-09-27" = "Nasrallah",
  "2024-10-01" = "Iran attack 2",
  "2025-01-19" = "ceasefire"
)

OCT7 <- as.Date("2023-10-07")

LABEL_MAP <- c(
  "positive" = 1, "pos" = 1, "חיובי" = 1,
  "neutral" = 0, "neu" = 0, "ניטרלי" = 0,
  "negative" = -1, "neg" = -1, "שלילי" = -1
)

na_approx_base <- function(x) {
  if (all(is.na(x))) return(x)
 
  nans <- is.na(x)
  idx <- seq_along(x)
 
  if (sum(!nans) > 1) {
    x[nans] <- approx(idx[!nans], x[!nans], xout = idx[nans])$y
  }
 
  return(x)
}

roll_mean_base <- function(x, k) {
  n <- length(x)
  res <- rep(NA_real_, n)
 
  for (i in seq_len(n)) {
    if (i >= k) {
      res[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
    }
  }
 
  return(res)
}

weighted_mean_safe <- function(value, weight) {
  s <- sum(weight, na.rm = TRUE)
 
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
 
  sum(value * weight, na.rm = TRUE) / s
}

parse_date_safe <- function(x) {
  parsed <- suppressWarnings(dmy(x))
 
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(ymd(x))
  }
 
  return(parsed)
}

load_and_build_series <- function() {
  cat("[A] טוען נתונים...\n")
 
  df <- read_csv(CSV_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
 
  date_col <- if ("date" %in% colnames(df)) {
    "date"
  } else {
    colnames(df)[grep("date|Date|תאריך", colnames(df))[1]]
  }
 
  source_col <- if ("source_name" %in% colnames(df)) {
    "source_name"
  } else {
    colnames(df)[1]
  }
 
  df_filtered <- df %>%
    rename(site = all_of(source_col)) %>%
    mutate(
      date_clean = parse_date_safe(as.character(.data[[date_col]])),
      political_flag = as.logical(Political),
      security_flag = as.logical(Security),
      val = unname(LABEL_MAP[tolower(trimws(as.character(SentimentLabel)))]),
      conf = as.numeric(ConfidenceScore),
      conf = replace_na(conf, 0),
      conf = pmax(conf, 0)
    ) %>%
    filter(
      political_flag == TRUE | security_flag == TRUE,
      !is.na(date_clean),
      !is.na(val)
    )
 
  df_weekly <- df_filtered %>%
    mutate(bucket = floor_date(date_clean, unit = FREQ)) %>%
    group_by(site, bucket) %>%
    summarise(
      n = n(),
      sentiment = weighted_mean_safe(val, conf),
      .groups = "drop"
    ) %>%
    mutate(sentiment = ifelse(n < MIN_ARTICLES, NA_real_, sentiment))
 
  wide <- df_weekly %>%
    select(site, bucket, sentiment) %>%
    pivot_wider(names_from = site, values_from = sentiment)
 
  full_dates <- data.frame(
    bucket = seq(min(wide$bucket, na.rm = TRUE), max(wide$bucket, na.rm = TRUE), by = FREQ)
  )
 
  wide <- full_dates %>%
    left_join(wide, by = "bucket")
 
  for (col in colnames(wide)[-1]) {
    wide[[col]] <- na_approx_base(wide[[col]])
  }
 
  wide <- wide %>%
    fill(everything(), .direction = "down") %>%
    fill(everything(), .direction = "up")
 
  return(wide)
}

add_events_to_plot <- function(p, x_min, x_max) {
  for (d_str in names(EVENTS)) {
    d <- as.Date(d_str)
   
    if (d >= x_min && d <= x_max) {
      p <- p +
        geom_vline(
          xintercept = as.numeric(d),
          linetype = "dashed",
          color = "grey",
          alpha = 0.7
        )
    }
  }
 
  return(p)
}

plot_overview <- function(wide) {
  wide_long <- wide %>%
    pivot_longer(cols = -bucket, names_to = "site", values_to = "sentiment") %>%
    group_by(site) %>%
    mutate(smoothed = roll_mean_base(sentiment, SMOOTH_WEEKS)) %>%
    ungroup()
 
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed, color = site)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = "Net sentiment over time per site (confidence-weighted)",
      y = "net tone",
      x = "Date"
    )
 
  p <- add_events_to_plot(p, min(wide$bucket), max(wide$bucket))
 
  ggsave("01_overview.png", plot = p, width = 14, height = 6.5, dpi = 130)
  cat("[D] נשמר: 01_overview.png\n")
}

plot_per_site <- function(wide) {
  wide_long <- wide %>%
    pivot_longer(cols = -bucket, names_to = "site", values_to = "sentiment") %>%
    group_by(site) %>%
    mutate(smoothed = roll_mean_base(sentiment, SMOOTH_WEEKS)) %>%
    ungroup()
 
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.4) +
    facet_wrap(~site, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(
      title = "Sentiment per site",
      x = "Date",
      y = "net tone"
    )
 
  p <- add_events_to_plot(p, min(wide$bucket), max(wide$bucket))
 
  ggsave("02_per_site.png", plot = p, width = 14, height = 8, dpi = 130)
  cat("[D] נשמר: 02_per_site.png\n")
}

plot_oct7_window <- function(wide) {
  start_date <- OCT7 %m-% months(6)
  end_date <- OCT7 %m+% months(6)
 
  win <- wide %>%
    filter(bucket >= start_date & bucket <= end_date)
 
  wide_long <- win %>%
    pivot_longer(cols = -bucket, names_to = "site", values_to = "sentiment") %>%
    group_by(site) %>%
    mutate(smoothed = roll_mean_base(sentiment, SMOOTH_WEEKS)) %>%
    ungroup()
 
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed, color = site)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = as.numeric(OCT7), color = "red", linewidth = 1.2) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = "Sentiment around Oct 7 (6m before -> 6m after)",
      x = "Date",
      y = "net tone"
    )
 
  p <- add_events_to_plot(p, start_date, end_date)
 
  ggsave("03_oct7_window.png", plot = p, width = 13, height = 6, dpi = 130)
  cat("[D] נשמר: 03_oct7_window.png\n")
}

plot_arima_vs_reality <- function(wide, site, event_date_str) {
  event_date <- as.Date(event_date_str)
  start_date <- event_date %m-% months(5)
  end_date <- event_date %m+% months(5)
 
  s_window <- wide %>%
    select(bucket, all_of(site)) %>%
    filter(bucket >= start_date & bucket <= end_date) %>%
    rename(sentiment = all_of(site)) %>%
    filter(!is.na(sentiment))
 
  train_data <- s_window %>%
    filter(bucket < event_date)
 
  actual_post <- s_window %>%
    filter(bucket >= event_date)
 
  if (nrow(train_data) < 8 || nrow(actual_post) < 2) {
    cat("[ARIMA] אין מספיק נתונים לגרף ARIMA עבור", site, "\n")
    return(NULL)
  }
 
  model <- arima(train_data$sentiment, order = c(1, 0, 1))
 
  forecast_steps <- nrow(actual_post)
  fcast <- predict(model, n.ahead = forecast_steps)
 
  forecast_df <- data.frame(
    bucket = actual_post$bucket,
    forecast_mean = as.numeric(fcast$pred),
    lower = as.numeric(fcast$pred) - 1.96 * as.numeric(fcast$se),
    upper = as.numeric(fcast$pred) + 1.96 * as.numeric(fcast$se)
  )
 
  s_window$smoothed <- roll_mean_base(s_window$sentiment, 2)
  s_window$smoothed[is.na(s_window$smoothed)] <- s_window$sentiment[is.na(s_window$smoothed)]
 
  p <- ggplot() +
    geom_line(
      data = s_window,
      aes(x = bucket, y = smoothed, group = 1, color = "Actual Tone"),
      linewidth = 1.2
    ) +
    geom_line(
      data = forecast_df,
      aes(x = bucket, y = forecast_mean, group = 1, color = "ARIMA forecast without event"),
      linetype = "dashed",
      linewidth = 1.2
    ) +
    geom_ribbon(
      data = forecast_df,
      aes(x = bucket, ymin = lower, ymax = upper, fill = "95% CI"),
      alpha = 0.15
    ) +
    geom_vline(
      xintercept = as.numeric(event_date),
      linetype = "dotted",
      color = "black",
      linewidth = 1
    ) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.4) +
    theme_minimal() +
    labs(
      title = paste("ARIMA Forecast vs Reality:", site, "around 7/10"),
      y = "Net Tone (-1 negative ... +1 positive)",
      x = "Date",
      color = "Lines",
      fill = "Interval"
    )
 
  filename <- paste0("04_arima_vs_reality_", gsub(" ", "_", site), ".png")
 
  ggsave(filename, plot = p, width = 12, height = 6, dpi = 130)
  cat("[ARIMA Graph] נשמר בהצלחה:", filename, "\n")
}

plot_all_events_impact <- function(wide) {
  sites <- colnames(wide)[-1]
  event_results <- data.frame()
 
  for (date_str in names(EVENTS)) {
    event_name <- EVENTS[[date_str]]
    event_date <- as.Date(date_str)
   
    for (site in sites) {
      s_vals <- wide[[site]]
      post_dummy <- as.numeric(wide$bucket >= event_date)
     
      if (sum(post_dummy) < 5 || sum(post_dummy == 0) < 5) next
      if (sum(!is.na(s_vals)) < 10) next
     
      try({
        model <- arima(s_vals, order = c(1, 0, 1), xreg = post_dummy)
       
        coef_val <- model$coef["post_dummy"]
        se_val <- sqrt(model$var.coef["post_dummy", "post_dummy"])
        pval <- 2 * (1 - pnorm(abs(coef_val / se_val)))
       
        event_results <- rbind(
          event_results,
          data.frame(
            event = event_name,
            site = site,
            coef = coef_val,
            error = 1.96 * se_val,
            significant = ifelse(pval < 0.05, "Significant", "Not Significant")
          )
        )
      }, silent = TRUE)
    }
  }
 
  if (nrow(event_results) == 0) {
    cat("[ARIMA] לא נמצאו מספיק תוצאות לאירועים.\n")
    return(NULL)
  }
 
  p <- ggplot(event_results, aes(x = coef, y = site, color = significant)) +
    geom_vline(xintercept = 0, color = "black", alpha = 0.4) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = coef - error, xmax = coef + error), height = 0.2, linewidth = 1) +
    scale_color_manual(values = c("Significant" = "#d62728", "Not Significant" = "#7f7f7f")) +
    facet_wrap(~event, scales = "free_x", ncol = 1) +
    theme_minimal() +
    labs(
      title = "Immediate Impact of Events Across All Sites",
      x = "Effect Size Coefficient",
      y = ""
    ) +
    theme(legend.position = "bottom")
 
  ggsave(
    "05_arima_events_impact_comparison.png",
    plot = p,
    width = 10,
    height = 3 * length(unique(event_results$event)),
    dpi = 130
  )
 
  cat("[ARIMA Graph] נשמר בהצלחה: 05_arima_events_impact_comparison.png\n")
}

wide_data <- load_and_build_series()

plot_overview(wide_data)
plot_per_site(wide_data)
plot_oct7_window(wide_data)

first_site_name <- colnames(wide_data)[2]

plot_arima_vs_reality(
  wide_data,
  site = first_site_name,
  event_date_str = "2023-10-07"
)

plot_all_events_impact(wide_data)

cat("\n--- All R visualizations generated successfully ---\n")
