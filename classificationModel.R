if (!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)
library(stringr)

securityWords <- c("פיגוע", "מלחמה", "טילים", "ביטחון", "מחבל", "נשק", "משטרה", "טרור", "חטופים", "צהל", "חיסל", "מפקד",
                   "הרוגים", "פצצה", "אש", "נהרגו", "מוסד", "אזעקה", "ניר דבורי", "יוסי יהושוע", "רועי שרון", "אור הלר",
                   "יואב זיתון", "חמאס", "חמא\"ס", "עזה", "איראן", "לבנון", "חזבאללה", "הפסקת אש", "חטוף", "בטחון",
                   "בטחוני")

politicsWords <- c("כנסת", "ממשלה", "בחירות", "חוק", "שר", "קואליציה", "אופוזיציה", "הצבעה", "מפלגה", "קבינט", "נתניהו",
                   "גנץ", "לפיד", "בגץ", "עמית סגל", "מיכאל שמש", "מורן אזולאי", "דפנה ליאל", "ירון אברהם", "ח\"כ",
                   "רה\"מ", "רוה\"מ", "ועדות", "ועדה", "ח\"כית", "עסקה", "מנהיגים", "מנהיג", "טראמפ", "ח\"כים", "גלנט",
                   "ביבי")

security_regex <- paste(securityWords, collapse = "|")
politics_regex <- paste(politicsWords, collapse = "|")

inputPath <- "C:/Users/mikab/OneDrive/Desktop/second year/4 semester/data analysis/Data.csv"
outputPath <- "C:/Users/mikab/OneDrive/Desktop/second year/4 semester/data analysis/FilteredData.csv"

df <- read_csv(inputPath, locale = locale(encoding = "UTF-8"))

fullTextToCheck <- paste(df$headline, df$author)

isPolitics <- str_detect(fullTextToCheck, politics_regex)
isSecurity <- str_detect(fullTextToCheck, security_regex)

isPolitics[is.na(isPolitics)] <- FALSE
isSecurity[is.na(isSecurity)] <- FALSE

df <- df %>%
  mutate(
    פוליטיקה = if_else(isPolitics, "כן", "לא"),
    בטחון = if_else(isSecurity, "כן", "לא")
  )

filtered_df <- df %>%
  filter(isPolitics | isSecurity)

total_rows <- nrow(df)
for (i in seq(1000, total_rows, by = 1000)) {
  saved_so_far <- sum(isPolitics[1:i] | isSecurity[1:i])
  cat(sprintf("scanned %d rows, saved %d articles\n", i, saved_so_far))
}

write_excel_csv(filtered_df, outputPath)

print("finished filtering with separate categories!")
