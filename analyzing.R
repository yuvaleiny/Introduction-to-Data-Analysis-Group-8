library(tidyverse)
library(lubridate)


FILE_PATH <- "C:\\Users\\rifre\\Desktop\\DataFinished.csv"
SMOOTH_WEEKS <- 4
MIN_ARTICLES <- 3

events <- tribble(
  ~date, ~label,
  "2023-10-07", "7/10 start",
  "2023-11-24", "hostages deal",
  "2024-04-14", "Iran attack 1",
  "2024-09-27", "Nasrallah",
  "2024-10-01", "Iran attack 2",
  "2025-01-19", "ceasefire"
) %>%
  mutate(
    date = ymd(date),
    display_label = paste(row_number(), label, sep=". ")
  )


df <- read_csv(FILE_PATH, locale = locale(encoding = "UTF-8"))

get_sentiment_val <- function(label) {
  val <- tolower(str_trim(label))
  case_when(
    val %in% c("positive", "pos", "חיובי") ~ 1,
    val %in% c("neutral", "neu", "ניטרלי") ~ 0,
    val %in% c("negative", "neg", "שלילי") ~ -1,
    TRUE ~ NA_real_
  )
}

df_clean <- df %>%
  mutate(
    date_parsed = dmy(date),
    
    is_pol = suppressWarnings(as.logical(Political)),
    is_sec = suppressWarnings(as.logical(Security)),
    
    val = get_sentiment_val(SentimentLabel),
    conf = as.numeric(ConfidenceScore)
  ) %>%
  mutate(
    is_pol = replace_na(is_pol, FALSE),
    is_sec = replace_na(is_sec, FALSE)
  ) %>%
  filter((is_pol | is_sec) & !is.na(val) & !is.na(conf) & !is.na(date_parsed)) %>%
  mutate(conf = pmax(conf, 0))

my_rollmean <- function(x, k) {
  n <- length(x)
  res <- rep(NA, n)
  if(n >= k){
    for (i in k:n) {
      res[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
    }
  }
  return(res)
}

df_weekly <- df_clean %>%
  mutate(week = floor_date(date_parsed, "week")) %>%
  group_by(source_name, week) %>%
  summarise(
    n = n(),
    vw = sum(val * conf),
    w = sum(conf),
    .groups = "drop"
  ) %>%
  mutate(sentiment = ifelse(n >= MIN_ARTICLES, vw / w, NA))

df_ts <- df_weekly %>%
  select(week, source_name, sentiment) %>%
  pivot_wider(names_from = source_name, values_from = sentiment) %>%
  arrange(week) %>%
  fill(-week, .direction = "down") %>%
  fill(-week, .direction = "up") %>%
  mutate(across(-week, ~ my_rollmean(., k = SMOOTH_WEEKS))) %>%
  pivot_longer(cols = -week, names_to = "source_name", values_to = "smooth_sentiment") %>%
  drop_na(smooth_sentiment)

plot_overview <- ggplot(df_ts, aes(x = week, y = smooth_sentiment, color = source_name)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_vline(data = events, aes(xintercept = date), color = "grey", linetype = "dashed", alpha = 0.7) +
  geom_text(data = events, aes(x = date, y = max(df_ts$smooth_sentiment, na.rm = TRUE), label = display_label),
            angle = 90, vjust = -0.5, hjust = 1, size = 3, color = "dimgrey", inherit.aes = FALSE) +
  theme_minimal() +
  labs(
    title = "Net sentiment over time per site (confidence-weighted)",
    x = "Date",
    y = "net tone (-1 neg ... +1 pos)",
    color = "Site"
  ) +
  theme(legend.position = "bottom")

ggsave("01_overview_R.png", plot_overview, width = 14, height = 6.5, dpi = 130)

plot_facets <- ggplot(df_ts, aes(x = week, y = smooth_sentiment, color = source_name)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_vline(data = events, aes(xintercept = date), color = "grey", linetype = "dashed", alpha = 0.5) +
  facet_wrap(~source_name, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Sentiment per site",
    x = "Date",
    y = "net tone"
  ) +
  theme(legend.position = "none", strip.text = element_text(size = 12, face = "bold"))

ggsave("02_per_site_R.png", plot_facets, width = 14, height = 8, dpi = 130)

df_before_after <- df_clean %>%
  mutate(period = factor(ifelse(date_parsed < ymd("2023-10-07"), "before 7/10", "after 7/10"),
                         levels = c("before 7/10", "after 7/10"))) %>%
  group_by(source_name, period) %>%
  summarise(mean_tone = weighted.mean(val, conf, na.rm = TRUE), .groups = "drop")

plot_before_after <- ggplot(df_before_after, aes(x = source_name, y = mean_tone, fill = period)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("before 7/10" = "#74add1", "after 7/10" = "#d73027")) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("Δ=%+.2f", mean_tone)), position = position_dodge(width = 0.8), vjust = 1.5, size = 3, color = "dimgrey") +
  theme_minimal() +
  labs(
    title = "Average tone per site: before vs after Oct 7",
    x = "Site",
    y = "mean net tone",
    fill = "Period"
  )

ggsave("07_before_after_oct7_R.png", plot_before_after, width = 10, height = 6, dpi = 130)

analyze_event_impact <- function(data, event_name, event_date_str, window_days = 30) {
  e_date <- ymd(event_date_str)
  start_date <- e_date - days(window_days)
  end_date <- e_date + days(window_days)
  
  df_event <- data %>%
    filter(date_parsed >= start_date & date_parsed <= end_date) %>%
    mutate(period = factor(ifelse(date_parsed < e_date, "before", "after"), levels = c("before", "after")))
  
  n_before <- sum(df_event$period == "before")
  n_after <- sum(df_event$period == "after")
  
  cat("\n======================================================\n")
  cat(sprintf("--- בדיקת מובהקות: %s (%s) ---\n", event_name, event_date_str))
  
  if(n_before < 30 | n_after < 30) {
    cat("אין מספיק נתונים לבדיקה סטטיסטית תקינה סביב תאריך זה.\n")
    return(NULL)
  }
  
  t_res <- t.test(val ~ period, data = df_event)
  mean_before <- mean(df_event$val[df_event$period == "before"])
  mean_after <- mean(df_event$val[df_event$period == "after"])
  
  cat(sprintf("טווח זמנים: %d ימים לפני ואחרי\n", window_days))
  cat(sprintf("תקופה 1 (לפני): ממוצע %.3f | N = %d\n", mean_before, n_before))
  cat(sprintf("תקופה 2 (אחרי): ממוצע %.3f | N = %d\n", mean_after, n_after))
  cat(sprintf("הפרש ממוצעים: %.3f\n", mean_before - mean_after))
  cat("----------------------------------------\n")
  
  if(t_res$p.value < 0.001) {
    cat("P-value: < 0.001 (מובהק סטטיסטית ברמה גבוהה מאוד!)\n")
  } else if (t_res$p.value < 0.05) {
    cat(sprintf("P-value: %.4f (מובהק סטטיסטית)\n", t_res$p.value))
  } else {
    cat(sprintf("P-value: %.4f (לא מובהק סטטיסטית)\n", t_res$p.value))
  }
  cat(sprintf("Confidence Interval (95%%): [%.3f, %.3f]\n", t_res$conf.int[1], t_res$conf.int[2]))
  cat("======================================================\n")
}

analyze_event_impact(df_clean, "עסקת חטופים", "2023-11-24", window_days = 30)
analyze_event_impact(df_clean, "מתקפה איראנית ראשונה", "2024-04-14", window_days = 30)
analyze_event_impact(df_clean, "הפסקת אש", "2025-01-19", window_days = 30)