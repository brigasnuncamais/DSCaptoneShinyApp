#--------------------------------------------------
# R Server Code for the Capstone Project Shiny App
#--------------------------------------------------

# load libraries
# suppressWarnings(library(tm))
# suppressWarnings(library(stringr))
require(devtools)
require(tm)
require(stringr)
require(reshape)
require(nlme)
require(survival)
require(cluster)
require(class)
require(Rcpp)
require(data.table)
require(stylo)
require(plyr)
require(wordcloud)
require(RColorBrewer)
require(ggplot2)
require(grid)
require(shiny)
require(scales)

# In batch R process ----
dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
options(error = dump_and_quit)

# In a later interactive session ----
# load("last.dump.rda")
# debugger()

# initially
# > options("error")
# $error
# (function () 
# {
#   .rs.recordTraceback(TRUE)
# })()



# load One-Gram, Two-Gram, Three-Gram and Four-Gram Data frame files
# This data is already cleansed with n-Grams frequency in decending order
# The data was convert to lower case, punctuations removed, numbers removed, 
# white spaces removed, non print characters removed

# load prediction function: text_predict() and CleanInput()
source("predict.R")

SavLocale <- Sys.getlocale()
# Sys.setlocale("LC_ALL", "en_US.UTF-8")
Sys.setlocale("LC_ALL", "English")
# ls <- table(strsplit(Sys.getlocale(),";"))
# df = transform(df, ls = colsplit(ls, split = "\\:", names=c('category', 'locale')))

# mesg <- as.character(NULL);

#-------------------------------------------------
# This function "Clean up" the user input string 
# before it is used to predict the next term
#-------------------------------------------------
# CleanInputString <- function(inStr)
# {
  # Test sentence
  #inStr <- "This is. the; -  .   use's 12"
  
  # First remove the non-alphabatical characters
  # inStr <- iconv(inStr, "latin1", "ASCII", sub=" ");
  # inStr <- gsub("[^[:alpha:][:space:][:punct:]]", "", inStr);
  
  # Then convert to a Corpus
  # inStrCrps <- VCorpus(VectorSource(inStr))
  
  # Convert the input sentence to lower case
  # Remove punctuations, numbers, white spaces
  # non alphabets characters
  # inStrCrps <- tm_map(inStrCrps, content_transformer(tolower))
  # inStrCrps <- tm_map(inStrCrps, removePunctuation)
  # inStrCrps <- tm_map(inStrCrps, removeNumbers)
  # inStrCrps <- tm_map(inStrCrps, stripWhitespace)
  # inStr <- as.character(inStrCrps[[1]])
  # inStr <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inStr)
  
  # Return the cleaned resulting sentence
  # If the resulting string is empty return empty and string.
  # if (nchar(inStr) > 0) {
    # return(inStr); 
  # } else {
    # return("");
  # }
# }

#---------------------------------------
# Description of the Back Off Algorithm
#---------------------------------------
# To predict the next term of the user specified sentence
# 1. first we use a FourGram; the first three words of which are the last three words of the user provided sentence
#    for which we are trying to predict the next Token. The FourGram is already sorted from highest to lowest frequency
# 2. If no FourGram is found, we back off to ThreeGram (first two words of ThreeGram last two words of the sentence)
# 3. If no FourGram is found, we back off to TwoGram (first Token of TwoGram last Token of the sentence)
# 4. If no TwoGram is found, we back off to OneGram (the most common Token with highest frequency)
#
# PredNextTerm <- function(inStr)
# {
  # assign("mesg", "in PredNextTerm", envir = .GlobalEnv)
  
  # Clean up the input string and extract only the words with no leading and trailing white spaces
  # inStr <- CleanInputString(inStr);
  
  # Split the input string across white spaces and then extract the length
  # inStr <- unlist(strsplit(inStr, split=" "));
  # inStrLen <- length(inStr);
  
  # nxtTermFound <- FALSE;
  # predNxtTerm <- as.character(NULL);
  # mesg <<- as.character(NULL);
  # 1. First test the Four Gram using the four gram data frame
  # if (inStrLen >= 3 & !nxtTermFound)
  # {
    # Assemble the terms of the input string separated by one white space each
    # inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
    
    # Subset the Four Gram data frame 
    # searchStr <- paste("^",inStr1, sep = "");
    # fDF4Temp <- fDF4[grep (searchStr, fDF4$terms), ];
    
    # Check to see if any matching record returned
    # if ( length(fDF4Temp[, 1]) > 1 )
    # {
      # predNxtTerm <- fDF4Temp[1,1];
      # nxtTermFound <- TRUE;
      # mesg <<- "Next Token is predicted using 4-gram."
    # }
    # fDF4Temp <- NULL;
  # }
  
  # 2. Next test the Three Gram using the three gram data frame
  # if (inStrLen >= 2 & !nxtTermFound)
  # {
    # Assemble the terms of the input string separated by one white space each
    # inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");
    
    # Subset the Three Gram data frame 
    # searchStr <- paste("^",inStr1, sep = "");
    # fDF3Temp <- fDF3[grep (searchStr, fDF3$terms), ];
    
    # Check to see if any matching record returned
    # if ( length(fDF3Temp[, 1]) > 1 )
    # {
      # predNxtTerm <- fDF3Temp[1,1];
      # nxtTermFound <- TRUE;
      # mesg <<- "Next Token is predicted using 3-gram."
    # }
    # fDF3Temp <- NULL;
  # }
  
  # 3. Next test the Two Gram using the three gram data frame
  # if (inStrLen >= 1 & !nxtTermFound)
  # {
    # Assemble the terms of the input string separated by one white space each
    # inStr1 <- inStr[inStrLen];
    
    # Subset the Two Gram data frame 
    # searchStr <- paste("^",inStr1, sep = "");
    # fDF2Temp <- fDF2[grep (searchStr, fDF2$terms), ];
    
    # Check to see if any matching record returned
    # if ( length(fDF2Temp[, 1]) > 1 )
    # {
      # predNxtTerm <- fDF2Temp[1,1];
      # nxtTermFound <- TRUE;
      # mesg <<- "Next Token is predicted using 2-gram.";
    # }
    # fDF2Temp <- NULL;
  # }
  
  # 4. If no next term found in Four, Three and Two Grams return the most
  #    frequently used term from the One Gram using the one gram data frame
  # if (!nxtTermFound & inStrLen > 0)
  # {
    # predNxtTerm <- fDF1$terms[1];
    # mesg <- "No next Token found, the most frequent Token is selected as next Token."
  # }
  
  # nextTerm <- Token(predNxtTerm, -1);
  
  # if (inStrLen > 0){
    # dfTemp1 <- data.frame(nextTerm, mesg);
    # return(dfTemp1);
  # } else {
    # nextTerm <- "";
    # mesg <-"";
    # dfTemp1 <- data.frame(nextTerm, mesg);
    # return(dfTemp1);
  # }
# }

shinyServer(function(input, output, session) {

  # process input
  output$input_value <- renderText({ input$text_input })
  
  # prediction 
  output$prediction_best <- renderText({ KneserNey(CleanInput(input$text_input))[1]$Token })
  # data table
  output$mytable <- renderDataTable({ RANK <- c(1:10)
  datatable <- head( KneserNey(input$text_input), 10)
  table <- cbind(RANK, datatable)
  table},  options = list( pageLength = 10,
                           paging = FALSE,
                           searching = FALSE) )
  # plot
  output$myplot <- renderPlot({
    data <- KneserNey(input$text_input)[1:10]
   
    data$P2 <- round(data$log10Prob, 3)
    plot <- ggplot(data, aes(x=reorder(Token,log10Prob), y=log10Prob, fill=log10Prob) ) +
      geom_bar(stat='identity', alpha=0.9) +
      coord_flip() +
      labs(y = "\n log10Prob, Probability Score") +
      labs(x = "Token") +
      scale_fill_gradient2(high = '#08519c') +
      theme_minimal(base_size = 15) +
      guides(fill=FALSE) +
      theme(axis.ticks.x=element_blank()) +
      theme(axis.ticks.y=element_blank()) +
      theme(panel.grid.major.y = element_blank()) +
      theme(panel.grid.major = element_line(color = "black")) +
      geom_text(aes(label=P2), hjust=-0.1 ,vjust=0.4) +
      theme(plot.margin = unit(c(0.5,2,0.5,0.5), "cm")) 
    gt <- ggplot_gtable(ggplot_build(plot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)
  })
  
  # wordcloud
  output$wordcloud <- renderPlot({ 
                                  KneserNeyDT <- KneserNey(CleanInput(input$text_input))
                                  MaxWord <- 20
                                  KneserNeyDT[,Prob:=as.integer(10^(-log10Prob))]
                                  wordcloud( KneserNeyDT[,Token][1:MaxWord], 
                                              KneserNeyDT[,Prob][1:MaxWord], 
                                              max.words=20, 
                                              random.order=F,
                                              random.color=F,
                                              scale=c(10,.5),
                                              use.r.layout=F,
                                              colors=brewer.pal(7,"BrBG"),
                                              rot.per = 0.5 ) 

  })
  # Sys.setlocale("LC_ALL", SavLocale)
  # Sys.setlocale("LC_ALL", "French")
 }

)