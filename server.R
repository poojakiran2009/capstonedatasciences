load ("smaller.rData")
#load ('/home/kirana/coursera/swiftkey/final/en_US/googlebadwords.rData')
print ('loaded file')
library(shiny)
library (data.table)
library (caret)
library (ggplot2)
library (stringr)
library (wordcloud)
library (tm)
library (RWeka)
library(SnowballC)
library (slam)
library (quanteda)

cleantext1 <- function (x) {
  x <- tolower (x)
  x <- tokenize (x, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE)
  x <- removeFeatures (x, googlebadwords)
  x <- paste (x, collapse = " ")
  return (x)
}
mypredict <- function (x, tgt=-1) {
  x <- tolower (x)
  x <- tokenize (x, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE)
  x <- removeFeatures (x, googlebadwords)
  
  # Let us look for match in 4-gram, 3-gram, 2-gram
  mywords3 <- paste (tail (unlist (x), 3), collapse=" ")
  mywords2 <- paste (tail (unlist (x), 2), collapse=" ")
  mywords1 <- tail (unlist (x), 1)
  match4gram <- ngram4df [word1==mywords3,]
  match3gram <- ngram3df [word1==mywords2,]
  match2gram <- ngram2df [word1==mywords1,]
  
  # Is there a match in 4-gram
  if (nrow(match4gram) > 0) {
    match4gram [,prob := freq * (1-unique(match4gram$missing_prob_mass))/denominator]
    match3gram [,prob := freq * discount * unique(match4gram$missing_prob_mass)/sum(freq*discount) * (1- unique (match3gram$missing_prob_mass))]
    match2gram [,prob := freq * discount * (1-sum(match3gram$prob) - sum(match4gram$prob) )/sum(freq*discount)]
    temp <- rbind (match4gram, match3gram, match2gram)
    temp <- temp [order(-prob),]
    temp <-	temp [,list(prob=sum(prob)),by=target]
    if (tgt != -1) temp <- temp [target %in% tgt,]
    return (temp$target[1:min(15, nrow(temp))])
  }	
  
  else {
    if (nrow (match3gram) > 0) {
      match3gram [,prob := freq * (1-unique(match3gram$missing_prob_mass))/denominator]
      match2gram [,prob := freq * discount * unique(match3gram$missing_prob_mass)/sum(freq*discount)]
      temp <- rbind (match3gram, match2gram)
      temp <-	temp [,list(prob=sum(prob)),by=target]
      temp <- temp [order(-prob),]
      if (tgt !=  -1) temp <- temp [target %in% tgt,]
      return (temp$target[1:min(15, nrow(temp))])
    }
    else {
      if (nrow (match2gram) > 0) {
        match2gram [,prob := freq * (1-unique(match2gram$missing_prob_mass))/denominator]
        temp <- match2gram
        temp <- temp [order(-prob),]
        if (tgt !=  -1) temp <- temp [target %in% tgt,]
        return (temp$target[1:min(15, nrow(temp))])
      }
      else {
        return ("the")
      }
    }
  }
  
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  #output$nextword1 <- renderText (mypredict (input$mytext) [1])
  #output$nextword2 <- renderText (mypredict (input$mytext) [2])
  #output$nextword3 <- renderText (mypredict (input$mytext) [3])

  
  #reactivefunction <- reactive ({
  #  mytextcleaned <- cleantext (input$mytext)
   # output$mytextcleaned <- renderPrint (mytextcleaned)
    
   # nextword <- mypredict (input$mytext)[1]
    #nextword
  #})
  output$mytextcleaned <- renderText (cleantext1(input$mytext))
  output$nextword1 <-renderText (mypredict (input$mytext)[1])
  output$nextword2 <- renderText (mypredict (input$mytext) [2])
  output$nextword3 <- renderText (mypredict (input$mytext) [3])
  # Attach the predicted word to the NextWord element in Shiny UI.
  #output$NextWord <- renderText(nextWord())
  
  
})
