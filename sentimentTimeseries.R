הבנתי, המגבלה הזו אפילו מקלה עלינו. אם מותר לך להשתמש אך ורק ב-`tidyverse` (שכולל את `dplyr`, `tidyr`, `ggplot2`, `readr`, `lubridate`) וב-`data.table`, נסיר גם את החבילות החיצוניות האחרות שהיו בקוד: `forecast` ו-`gridExtra`.

מאחר שאי אפשר להשתמש ב-`forecast` עבור מודל ה-ARIMA וב-`gridExtra` לחיבור הגרפים, נבצע את השינויים הבאים:

1. **חיזוי ARIMA:** נשתמש בפונקציה המובנית `arima()` של Base R, ונחשב את רווח הסמך ($95\%$) ידנית בעזרת השגיאה הסטנדרטית (Standard Error).
2. **חיבור גרפים:** במקום `gridExtra`, נשמור כל גרף אירוע כקובץ נפרד (או נשתמש ב-`facet_wrap` המובנה של `ggplot2` שמגיע מ-`tidyverse`).

הנה הקוד המעודכן, המבוסס **אך ורק** על החבילות שהגדרת:

```R
required_packages <- c("tidyverse", "data.table")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(data.table)

CSV_PATH     <- "C:/Users/mikab/OneDrive/Desktop/second year/4 semester/data analysis/FinalSentiment.csv"
FREQ         <- "week"
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

LABEL_MAP <- c("positive" = 1, "pos" = 1, "חיובי" = 1,
               "neutral" = 0, "neu" = 0, "ניטרלי" = 0,
               "negative" = -1, "neg" = -1, "שלילי" = -1)

# --- פונקציות עזר מבוססות Base R (ללא zoo) ---

# אינטרפולציה ליניארית
na_approx_base <- function(x) {
  if (all(is.na(x))) return(x)
  nans <- is.na(x)
  idx <- 1:length(x)
  if(sum(!nans) > 1) {
    x[nans] <- approx(idx[!nans], x[!nans], xout = idx[nans])$y
  }
  return(x)
}

# ממוצע נע
roll_mean_base <- function(x, k) {
  n <- length(x)
  res <- rep(NA, n)
  for(i in k:n) {
    res[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
  }
  return(res)
}

# --------------------------------------------

load_and_build_series <- function() {
  cat("[A] טוען נתונים...\n")
  df <- read_csv(CSV_PATH, locale = locale(encoding = "UTF-8"))
  
  date_col <- if ("date" %in% colnames(df)) "date" else colnames(df)[grep("date|Date|תאריך", colnames(df))[1]]
  source_col <- if ("source_name" %in% colnames(df)) "source_name" else colnames(df)[1]
  
  df_filtered <- df %>%
    mutate(
      _date = dmy(!!sym(date_col)), 
      פוליטיקה_clean = trimws(as.character(פוליטיקה)),
      בטחון_clean = trimws(as.character(בטחון))
    ) %>%
    filter(פוליטיקה_clean == "כן" | בטחון_clean == "כן") %>%
    mutate(
      _val = LABEL_MAP[tolower(trimws(as.character(SentimentLabel)))],
      _conf = as.numeric(ConfidenceScore)
    ) %>%
    filter(!is.na(_date), !is.na(_val), !is.na(_conf)) %>%
    mutate(_conf = pmax(_conf, 0))
  
  df_weekly <- df_filtered %>%
    mutate(bucket = floor_date(_date, unit = FREQ)) %>%
    group_by(across(all_of(c(source_col, "bucket")))) %>% 
    rename(_site = !!sym(source_col)) %>% 
    summarise(
      vw = sum(_val * _conf, na.rm = TRUE),
      w = sum(_conf, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(sentiment = ifelse(n < MIN_ARTICLES, NA, vw / w))
  
  wide <- df_weekly %>%
    select(_site, bucket, sentiment) %>%
    pivot_wider(names_from = _site, values_from = sentiment)
  
  full_dates <- data.frame(bucket = seq(min(wide$bucket), max(wide$bucket), by = FREQ))
  wide <- full_dates %>% left_join(wide, by = "bucket")
  
  for(col in colnames(wide)[-1]) {
    wide[[col]] <- na_approx_base(wide[[col]])
  }
  
  wide <- wide %>% 
    fill(everything(), .direction = "down") %>% 
    fill(everything(), .direction = "up")
  
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
    pivot_longer(cols = -bucket, names_to = "site", values_to = "sentiment") %>%
    group_by(site) %>%
    mutate(smoothed = roll_mean_base(sentiment, SMOOTH_WEEKS))
  
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed, color = site)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Net sentiment over time per site (confidence-weighted)", y = "net tone", x = "Date")
  
  p <- add_events_to_plot(p, min(wide$bucket), max(wide$bucket))
  ggsave("01_overview.png", plot = p, width = 14, height = 6.5, dpi = 130)
  cat("[D] נשמר: 01_overview.png\n")
}

plot_per_site <- function(wide) {
  wide_long <- wide %>%
    pivot_longer(cols = -bucket, names_to = "site", values_to = "sentiment") %>%
    group_by(site) %>%
    mutate(smoothed = roll_mean_base(sentiment, SMOOTH_WEEKS))
  
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed)) +
    geom_line(color = "blue", linewidth = 1) +
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
  
  win <- wide %>% filter(bucket >= start_date & bucket <= end_date)
  
  wide_long <- win %>%
    pivot_longer(cols = -bucket, names_to = "site", values_to = "sentiment") %>%
    group_by(site) %>%
    mutate(smoothed = roll_mean_base(sentiment, SMOOTH_WEEKS))
  
  p <- ggplot(wide_long, aes(x = bucket, y = smoothed, color = site)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = as.numeric(OCT7), color = "red", linewidth = 1.2) +
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
  
  # שימוש ב-arima של Base R במקום forecast
  model <- arima(train_data$sentiment, order = c(1, 0, 1))
  forecast_steps <- nrow(actual_post)
  
  # חיזוי ידני מבוסס Base R
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
    geom_line(data = s_window, aes(x = bucket, y = smoothed, group = 1, color = "המציאות בפועל (Actual Tone)"), linewidth = 1.2) +
    geom_line(data = forecast_df, aes(x = bucket, y = forecast_mean, group = 1, color = "תחזית ARIMA ללא האירוע"), linetype = "dashed", linewidth = 1.2) +
    geom_ribbon(data = forecast_df, aes(x = bucket, ymin = lower, ymax = upper, fill = "טווח טעות סטטיסטי (95% CI)"), alpha = 0.15) +
    geom_vline(xintercept = as.numeric(event_date), linetype = "dotted", color = "black", linewidth = 1) +
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
  event_results <- data.frame()
  
  for(date_str in names(EVENTS)) {
    event_name <- EVENTS[[date_str]]
    event_date <- as.Date(date_str)
    
    for(site in sites) {
      s_vals <- wide[[site]]
      post_dummy <- as.numeric(wide$bucket >= event_date)
      
      if(sum(post_dummy) < 5 || sum(post_dummy == 0) < 5) next
      
      try({
        # שימוש ב-arima של Base R
        model <- arima(s_vals, order = c(1, 0, 1), xreg = post_dummy)
        coef_val <- model$coef["post_dummy"]
        se_val <- sqrt(model$var.coef["post_dummy", "post_dummy"])
        pval <- 2 * (1 - pnorm(abs(coef_val / se_val)))
        
        event_results <- rbind(event_results, data.frame(
          event = event_name,
          site = site,
          coef = coef_val,
          error = 1.96 * se_val,
          significant = ifelse(pval < 0.05, "Significant", "Not Significant")
        ))
      }, silent = TRUE)
    }
  }
  
  if(nrow(event_results) == 0) return(NULL)
  
  # במקום gridExtra - משתמשים ב-facet_wrap של ggplot2 כדי לרכז את כל האירועים בגרף אחד מובנה
  p <- ggplot(event_results, aes(x = coef, y = site, color = significant)) +
    geom_vline(xintercept = 0, color = "black", alpha = 0.4) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = coef - error, xmax = coef + error), height = 0.2, linewidth = 1) +
    scale_color_manual(values = c("Significant" = "#d62728", "Not Significant" = "#7f7f7f")) +
    facet_wrap(~event, scales = "free_x", ncol = 1) +
    theme_minimal() +
    labs(title = "Immediate Impact of Events Across All Sites", x = "Effect Size (Coefficient)", y = "") +
    theme(legend.position = "bottom")
  
  ggsave("05_arima_events_impact_comparison.png", plot = p, width = 10, height = 3 * length(unique(event_results$event)), dpi = 130)
  cat("[ARIMA Graph] נשמר בהצלחה גרף מרוכז: 05_arima_events_impact_comparison.png\n")
}

# הרצה ראשית
wide_data <- load_and_build_series()

plot_overview(wide_data)
plot_per_site(wide_data)
plot_oct7_window(wide_data)

first_site_name <- colnames(wide_data)[2] 
plot_arima_vs_reality(wide_data, site = first_site_name, event_date_str = "2023-10-07")
plot_all_events_impact(wide_data)

cat("\n--- All R visualizations generated successfully using ONLY tidyverse! ---\n")

```
