# Introduction-to-Data-Analysis-Group-8
This project analyzes the sentiment (positive, neutral, negative) of Israeli news websites during the October 7 war and other major security events.

The data pipeline works in three steps: First, it filters the articles using R. Then, it uses an AI model in Python to read the articles and predict their sentiment. Finally, it uses R scripts to create statistical models and visualizations.

## Project Files

To run this complete pipeline, you need these files:
* **`classificationModel.R`** - Filters the raw data to keep only articles about politics or security.
* **`sentimentModel.py`** - The Python script that uses an AI model (heBERT) to predict if an article is positive, negative, or neutral.
* **`analyzing.R`** - Cleans the final data, creates basic charts, and runs T-tests for events.
* **`sentimentTimeseries.R`** - find the real impact of events on the news.
* **`extraGraphs.R`** - Creates heatmaps, composition charts, and other visualizations.

## Prerequisites

* **R & RStudio**: You need R installed. The scripts use the following packages: `tidyverse`, `lubridate`, `zoo`, `forecast`, `ggplot2`, `gridExtra`.
* **Python**: You need Python installed (e.g., via PyCharm) with the `transformers` library.

## How to Run the Code

**Important:** Before you run any code, you must update the file paths! Look for variables like `inputPath`, `outputPath`, `FILE_PATH`, or `CSV_PATH` at the top of the scripts and change them to where the files are saved on **your** computer.

**Step 1: Filter the Data (R)**
1. Open **`classificationModel.R`** in RStudio.
2. Run it to filter the raw data. It will generate a new file called `FilteredData.csv`.

**Step 2: Get the Sentiment (Python)**
1. Open **`sentimentModel.py`** in your Python editor.
2. Run it to analyze the filtered text. It will generate a new CSV file with the sentiment scores (e.g., `DataFinished.csv` or `FinalSentiment.csv`).

**Step 3: Create the Charts and Statistics (R)**
1. Open the `.R` files in RStudio.
2. Open and run **`asalyzing.R`** to get the general overview and T-test results.
3. Open and run **`sentimentTimeseries.R`** to run the time series.
4. Open and run **`extraGraphs.R`** to get the rest of the visual analysis.

## Results
When the scripts finish running, you will see new `.png` image files in your folder. These are the final graphs! You will also see the statistical results (P-values) printed in the RStudio console.
