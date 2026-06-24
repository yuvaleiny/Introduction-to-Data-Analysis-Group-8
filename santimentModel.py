import csv
from transformers import pipeline

negativeWords = ["נהרג", "נהרגו", "הרוגים"]
positiveWords = ["הצלחה", "ניצחון", "זכיה"]


def hasKeywords(textToCheck, wordList):
    for word in wordList:
        if word in textToCheck:
            return True
    return False


print("loading sentiment model...")
sentimentPipeline = pipeline("sentiment-analysis", model="avichr/heBERT_sentiment_analysis",
                             tokenizer="avichr/heBERT_sentiment_analysis")
print("model ready!")

inputPath = r"C:\Users\mikab\OneDrive\Desktop\second year\4 semester\data analysis\FilteredData.csv"
outputPath = r"C:\Users\mikab\OneDrive\Desktop\second year\4 semester\data analysis\FinalSentiment.csv"

# open files
with open(inputPath, mode='r', encoding='utf-8-sig') as inFile, \
        open(outputPath, mode='w', encoding='utf-8-sig', newline='') as outFile:
    csvReader = csv.reader(inFile)
    csvWriter = csv.writer(outFile)

    headerRow = next(csvReader)
    headerRow.append("SentimentLabel")
    headerRow.append("ConfidenceScore")
    csvWriter.writerow(headerRow)

    analyzedRows = 0

    # loop through rows
    for row in csvReader:
        if len(row) >= 3:
            headlineText = row[1]
            authorName = row[2]

            combinedContext = "כתב: " + authorName + " כותרת: " + headlineText

            isManualNegative = hasKeywords(combinedContext, negativeWords)
            isManualPositive = hasKeywords(combinedContext, positiveWords)

            if isManualNegative:
                sentimentLabel = "negative"
                modelScore = 1.0
            elif isManualPositive:
                sentimentLabel = "positive"
                modelScore = 1.0
            else:
                shortContext = combinedContext[:500]
                modelResult = sentimentPipeline(shortContext)
                sentimentLabel = modelResult[0]['label']
                modelScore = modelResult[0]['score']

            row.append(sentimentLabel)
            row.append(round(modelScore, 3))

            csvWriter.writerow(row)
            analyzedRows += 1

            if analyzedRows % 50 == 0:
                print(f"analyzed {analyzedRows} articles")

print("finished sentiment analysis! check FinalSentiment.csv")
