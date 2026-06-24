library(tidyverse)
library(lubridate)

CSV_PATH <- "C:/Users/mikab/OneDrive/Desktop/second year/4 semester/data analysis/FinalSentiment.csv"
SMOOTH_WEEKS <- 4
OCT7 <- ymd("2023-10-07")

# --- פונקציית עזר להחלפת zoo::rollmeanr (ממוצע נע ימני) ---
roll_mean_base <- function(x, k) {
  n <- length(x)
  res <- rep(NA, n)
  if (n < k) return(res)
  for(i in k:n) {
    res[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
  }
  return(res)
}
# --------------------------------------------------------

events <- tibble(
  date = ymd(c("2023-10-07", "2023-11-24", "2024-04-14", "2024-09-27", "2024-10-01", "2025-01-19")),
  label = c("7/10 start", "hostages deal", "Iran attack 1", "Nasrallah", "Iran attack 2", "ceasefire")
) %>% 
  arrange(date) %>%
  mutate(num = row_number(),
         full_label = paste0(num, ". ", label))

add_events_layer <- function(gg) {
  gg + 
    geom_vline(data = events, aes(xintercept = date), color = "grey", linetype = "dashed", alpha = 0.6) +
    geom_text(data = events, aes(x = date, y = Inf, label = num), 
              vjust = 1.5, hjust = -0.2, color = "dimgrey", size = 3, fontface = "bold", inherit.aes = FALSE)
}

df <- read_csv(CSV_PATH, show_col_types = FALSE) %>%
  rename(source_name = 1) %>% 
  mutate(
    date = dmy(date), 
    _val = case_when(
      tolower(SentimentLabel) %in= c("positive", "pos", "חיובי") ~ 1,
      tolower(SentimentLabel) %in= c("neutral", "neu", "ניטרלי") ~ 0,
      tolower(SentimentLabel) %in= c("negative", "neg", "שלילי") ~ -1,
      TRUE ~ NA_real_
    ),
    ConfidenceScore = replace_na(ConfidenceScore, 0),
    _conf = ifelse(ConfidenceScore < 0, 0, ConfidenceScore)
  ) %>%
  filter((פוליטיקה == "כן" | בטחון == "כן"), !is.na(date), !is.na(_val))

weekly_df <- df %>%
  mutate(week_bucket = floor_date(date, "week")) %>%
  group_by(source_name, week_bucket) %>%
  summarise(
    n_articles = n(),
    sentiment = sum(_val * _conf) / sum(_conf),
    .groups = "drop"
  ) %>%
  filter(n_articles >= 3) %>%
  group_by(source_name) %>%
  mutate(smoothed_sentiment = roll_mean_base(sentiment, k = SMOOTH_WEEKS)) %>%
  ungroup()

plot_deviation <- function(data) {
  mean_weekly <- data %>%
    group_by(week_bucket) %>%
    summarise(mean_all = mean(sentiment, na.rm = TRUE), .groups = "drop")
  
  data_dev <- data %>%
    left_join(mean_weekly, by = "week_bucket") %>%
    mutate(deviation = sentiment - mean_all) %>%
    group_by(source_name) %>%
    mutate(smooth_dev = roll_mean_base(deviation, k = SMOOTH_WEEKS)) %>%
    ungroup()
  
  p <- ggplot(data_dev, aes(x = week_bucket, y = smooth_dev, color = source_name)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 1, alpha = 0.6) +
    geom_line(linewidth = 1) +
    add_events_layer() +
    labs(title = "Systematic bias: each site minus the cross-site average",
         y = "Deviation", x = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("05_deviation_R.png", plot = p, width = 14, height = 6.5, dpi = 130)
}

plot_heatmap <- function(data_raw) {
  heat_data <- data_raw %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(source_name, month) %>%
    summarise(n = n(), sentiment = sum(_val * _conf) / sum(_conf), .groups = "drop") %>%
    filter(n >= 3)
  
  max_val <- max(abs(heat_data$sentiment), na.rm = TRUE)
  
  p <- ggplot(heat_data, aes(x = month, y = source_name, fill = sentiment)) +
    geom_tile(color = "white") +
    scale_fill_distiller(palette = "RdYlGn", direction = 1, limits = c(-max_val, max_val)) +
    labs(title = "Net tone heatmap (site x month)", x = "Month", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("06_heatmap_R.png", plot = p, width = 12, height = 5, dpi = 130)
}

plot_before_after <- function(data_raw) {
  ba_data <- data_raw %>%
    mutate(period = if_else(date < OCT7, "before 7/10", "after 7/10")) %>%
    mutate(period = factor(period, levels = c("before 7/10", "after 7/10"))) %>%
    group_by(source_name, period) %>%
    summarise(mean_tone = sum(_val * _conf) / sum(_conf), .groups = "drop")
  
  p <- ggplot(ba_data, aes(x = source_name, y = mean_tone, fill = period)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_hline(yintercept = 0, color = "black") +
    scale_fill_manual(values = c("before 7/10" = "#74add1", "after 7/10" = "#d73027")) +
    labs(title = "Average tone per site: before vs after Oct 7", y = "Mean Net Tone", x = "") +
    theme_minimal()
  
  ggsave("07_before_after_oct7_R.png", plot = p, width = 10, height = 6, dpi = 130)
}

plot_politics_vs_security <- function(data_raw) {
  pol_sec <- data_raw %>%
    mutate(week_bucket = floor_date(date, "week")) %>%
    group_by(week_bucket) %>%
    summarise(
      politics = sum(_val[_conf > 0 & פוליטיקה == "כן"] * _conf[_conf > 0 & פוליטיקה == "כן"], na.rm = TRUE) / 
        sum(_conf[_conf > 0 & פוליטיקה == "כן"], na.rm = TRUE),
      security = sum(_val[_conf > 0 & בטחון == "כן"] * _conf[_conf > 0 & בטחון == "כן"], na.rm = TRUE) / 
        sum(_conf[_conf > 0 & בטחון == "כן"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      smooth_pol = roll_mean_base(politics, k = SMOOTH_WEEKS),
      smooth_sec = roll_mean_base(security, k = SMOOTH_WEEKS)
    ) %>%
    pivot_longer(cols = c(smooth_pol, smooth_sec), names_to = "category", values_to = "tone")
  
  p <- ggplot(pol_sec, aes(x = week_bucket, y = tone, color = category)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("smooth_pol" = "#6a51a3", "smooth_sec" = "#e6550d"), 
                       labels = c("Politics", "Security")) +
    add_events_layer() +
    labs(title = "Tone: politics vs security articles (avg across sites)", y = "Net tone", x = "") +
    theme_minimal()
  
  ggsave("08_politics_vs_security_R.png", plot = p, width = 14, height = 6, dpi = 130)
}

plot_composition <- function(data_raw) {
  comp_data <- data_raw %>%
    mutate(
      week_bucket = floor_date(date, "week"),
      label = case_when(_val == 1 ~ "pos", _val == 0 ~ "neu", _val == -1 ~ "neg")
    ) %>%
    count(source_name, week_bucket, label) %>%
    group_by(source_name, week_bucket) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    group_by(source_name, label) %>%
    arrange(week_bucket) %>%
    mutate(smooth_prop = roll_mean_base(prop, k = SMOOTH_WEEKS)) %>%
    ungroup() %>%
    filter(!is.na(smooth_prop))
  
  p <- ggplot(comp_data, aes(x = week_bucket, y = smooth_prop, fill = factor(label, levels=c("pos", "neu", "neg")))) +
    geom_area(alpha = 0.8) +
    scale_fill_manual(values = c("pos"="#5cb85c", "neu"="#bdbdbd", "neg"="#d9534f")) +
    facet_wrap(~source_name, ncol = 2) +
    geom_vline(data = events, aes(xintercept = date), color = "black", linetype = "dashed", alpha = 0.4) +
    labs(title = "Label composition over time (share of pos/neu/neg)", x = "", y = "Proportion", fill="Label") +
    theme_minimal() +
    theme(strip.text = element_text(size = 11, face = "bold"))
  
  ggsave("09_composition_R.png", plot = p, width = 14, height = 8, dpi = 130)
}

plot_coverage <- function(data_weekly) {
  # חישוב הממוצע הנע בתוך ה-pipeline באמצעות הפונקציה החדשה
  data_smoothed <- data_weekly %>%
    group_by(source_name) %>%
    mutate(smoothed_articles = roll_mean_base(n_articles, k = SMOOTH_WEEKS)) %>%
    ungroup()

  p <- ggplot(data_smoothed, aes(x = week_bucket, y = smoothed_articles, color = source_name)) +
    geom_line(linewidth = 1) +
    add_events_layer() +
    labs(title = "Coverage volume (articles per week, smoothed)", y = "# articles / week", x = "") +
    theme_minimal()
  
  ggsave("11_coverage_volume_R.png", plot = p, width = 14, height = 6, dpi = 130)
}

# הרצה של הפונקציות
plot_deviation(weekly_df)
plot_heatmap(df)
plot_before_after(df)
plot_politics_vs_security(df)
plot_composition(df)
plot_coverage(weekly_df)

print("Finished! All plots saved as PNGs in the working directory.")
