library(shiny)

shinyUI( pageWithSidebar(
  headerPanel("Input a phrase so that my algorithm can guess the next likely word"),
  
  sidebarPanel(
    textInput('mytext', label = h2("Enter your phrase:"), value="hello world")
    #,    actionButton("goButton", "Go!")
  ), 
  mainPanel(
    h3('Cleaned Text Vector in R & Removes Profanities:'),textOutput('mytextcleaned'),
    h3('Most likely next word'),textOutput ('nextword1'),
    h4 ('Below are additional guesses from my side'),
    h3('Second Most likely next word'),textOutput ('nextword2'),
    h3('Third Most likely next word'),textOutput ('nextword3')
  )
)
)