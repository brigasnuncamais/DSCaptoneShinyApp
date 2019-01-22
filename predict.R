# load ngram tables column must be named c("Token<i>", "Cnt")
# load("ngrams_limit.Rdata")
# load ngram tables column must be named c("Token<i>", "Cnt")
load("ngramsDataTable.RData")

# FUNCTION CleanInput
CleanInput <- function(inptext){
  # remove punctuation, preserving compounded words with dashes (e.g.:money-making)
  # convert to lowercase, and split into words
  text_words <- txt.to.words.ext(inptext, language="English.all", preserve.case = F)
  # replace contraction symbol 
  TextCorrectedWords <- gsub("\\^", "'", text_words)
  # only use last 3 words if inptext is more than 3 words
  InputTextSize <- length(TextCorrectedWords)
  if (InputTextSize >= 3) text <- TextCorrectedWords[(InputTextSize-2):InputTextSize]
  else text <- TextCorrectedWords
  text
}
  
# FUNCTION Recursive KneserNey Cnt-gram with highest log10Prob
# n : current order during recursive loop
# m : maximum order beginning recursive loop
# CntProbs : how many probs are required by caller (completed by n1grams at worst)
KneserNey <- function(text,n=4, m=4, CntProbs=10){
  # 4-ngrams for Kneser-Ney method

  if ( n==m) {
    # clean and split the input string on first run (n set to max=m)
    text <- CleanInput(text)
  }
  # adapt n and m to text length that can be smaller
  n <- min(n,length(text)+1)
  m <- min(m,length(text)+1)
  if (n>1) {
      # point n and n-1 gram according to current n level
      ngramsDT <- get(sprintf(fmt="n%dgramsDT",n))
      n_1gramsDT <- get(sprintf(fmt="n%dgramsDT",n-1))

      # join text words(1,n) to filter data.table, keep dt Token log10Prob and log10BackOffWeight
      KneserNey_n <-
        ngramsDT[as.list(text[(m-n+1):(min(m-1,length(text)))]),c(1,n+1), with=F][order(-log10Prob)]
      # if we found a valuable ngram
      if (!(is.na(KneserNey_n[1,log10Prob]))) {
        KneserNeyA <- KneserNey_n
        setnames(KneserNeyA,1:2,c("log10Prob", "Token"))
      } else {
        if (n>2) {
            # join text words(1,n-1) to filter data.table, keep dt(log10Prob,last Token,log10BackOffWeight)
            KneserNeyN_1 <-
              n_1gramsDT[as.list(text[(m-n+2):(min(m-1,length(text)))]),c(1,n,n+1), with=F][order(-log10Prob)]
            # if we found a valuable ngram
            if (!(is.na(KneserNeyN_1[1,log10Prob]))) {
              # keep dt(log10BackOffWeight,last Token)
              KneserNey_all <- KneserNeyN_1[,c(3,2), with=F]
              # rename it as log10Prob
              setnames(KneserNey_all,1:2,c("log10Prob", "Token"))
              KneserNeyN_1 <- KneserNey(text,n-1)
              KneserNey_m <- merge(KneserNey_all, KneserNeyN_1, by="Token")
              KneserNey_m[,log10Prob:=log10Prob.x+log10Prob.y]
              KneserNeyA <- KneserNey_m[,c('log10Prob.x', 'log10Prob.y') := NULL][order(-log10Prob)]
            } else KneserNeyA <- KneserNey(text,n-1)
        # when n=2, keep DT of log10Prob+log10BackOffWeight of n1
        } else {
          KneserNeyA <- n1gramsDT[order(-log10Prob)][,, with=F]
          KneserNeyA[,log10Prob:=log10Prob+log10BackOffWeight]
          KneserNeyA <- KneserNeyA[,c('log10BackOffWeight') := NULL]
          }
      }
      if (nrow(KneserNeyA)<CntProbs) {
        KneserNeyN_1 <- KneserNey(text,n-1)
        KneserNey_m <- merge(KneserNeyA, KneserNeyN_1, by="Token", all=T)
        for (i in c('log10Prob.x','log10Prob.y')) KneserNey_m[is.na(get(i)),i:=-1000.,with=F]
        KneserNey_m[,log10Prob:=max(log10Prob.x,log10Prob.y), by="Token"]
        KneserNeyA <- KneserNey_m[,c('log10Prob.x', 'log10Prob.y') := NULL][order(-log10Prob)]
      }
  } else {
    # if none matches, use top 100 1-ngrams
    KneserNeyA <- n1gramsDT[order(-log10Prob)][,c(1,n+1), with=F]
  }
  # remove NAs 
  KneserNeyA <- KneserNeyA[complete.cases(KneserNeyA),]
  # remove <unk> </s> and <s>
  setkey(KneserNeyA,Token)
  # subset(KneserNeyA, -(Token %in% c("<s>","unk","</s")))
  # return data.table with best matches
  KneserNeyA[order(-log10Prob)]
}

# FUNCTION katz back-off Cnt-gram with highest freqency model
KatzBackOff <- function(inptext){
    # remove punctuation, preserving compounded words with dashes (e.g.:money-making)
    # convert to lowercase, and split into words
    text_words <- txt.to.words.ext(inptext, language="English.all", preserve.case = F)
    # replace contraction symbol 
    TextCorrectedWords <- gsub("\\^", "'", text_words)
    # only use last 3 words if inptext is more than 3 words
    InputTextSize <- length(TextCorrectedWords)
    if (InputTextSize >= 3) text <- TextCorrectedWords[(InputTextSize-2):InputTextSize] else text <- TextCorrectedWords
    # 4-ngrams for katz back-off method
    if (InputTextSize >= 3) {
        # make a join of text words to filter data.table, keep last dt word and count
        KatzBackOff_4 <- n_4[J(text[1],text[2],text[3])][order(-Cnt)][,4:5, with=F]
        KatzBackOff_3 <- n_3[J(text[2],text[3])][order(-Cnt)][,3:4, with=F]
        KatzBackOff_2 <- n_2[J(text[3])][order(-Cnt)][,2:3, with=F]
        # add Probability column, unify Token column name through n-grams
        KatzBackOff_4 <- mutate(KatzBackOff_4, Prob=Cnt/sum(Cnt)); setnames(KatzBackOff_4,"Token4", "Token")
        KatzBackOff_3 <- mutate(KatzBackOff_3, Prob=Cnt/sum(Cnt)); setnames(KatzBackOff_3,"Token3", "Token")
        KatzBackOff_2 <- mutate(KatzBackOff_2, Prob=Cnt/sum(Cnt)); setnames(KatzBackOff_2,"Token2", "Token")
        # add 1grams to found n-grams and order by decreasing probability
        KatzBackOff_all <- rbind(KatzBackOff_4, KatzBackOff_3, KatzBackOff_2, n_1, fill=T)[order(-Prob)]
        # Calculating multiple aggregations by data
        KatzBackOff <- KatzBackOff_all[,lapply(.SD,max),by="Token"]
    # 3-ngrams for katz back-off method
    } else if (InputTextSize == 2) {
        KatzBackOff_3 <- n_3[J(text[1],text[2])][order(-Cnt)][,3:4, with=F]
        KatzBackOff_2 <- n_2[J(text[2])][order(-Cnt)][,2:3, with=F]
        KatzBackOff_3 <- mutate(KatzBackOff_3, Prob=Cnt/sum(Cnt)); setnames(KatzBackOff_3,"Token3", "Token")
        KatzBackOff_2 <- mutate(KatzBackOff_2, Prob=Cnt/sum(Cnt)); setnames(KatzBackOff_2,"Token2", "Token")
        KatzBackOff_all <- rbind(KatzBackOff_3, KatzBackOff_2, n_1, fill=T)[order(-Prob)][,c(1,3), with=F]
        KatzBackOff <- KatzBackOff_all[,lapply(.SD,max),by="Token"]
    # 2-ngrams for katz back-off method
    } else {
        KatzBackOff_2 <- n_2[J(text[1])][order(-Cnt)][,2:3, with=F]
        KatzBackOff_2 <- mutate(KatzBackOff_2, Prob=Cnt/sum(Cnt)); setnames(KatzBackOff_2,"Token2", "Token")
        KatzBackOff_all <- rbind(KatzBackOff_2, n_1, fill=T)[order(-Prob)][,c(1,3), with=F]
        KatzBackOff <- KatzBackOff_all[,lapply(.SD,max),by="Token"]
    }
    # if none matches, use top 100 1-ngrams
    if (is.na( KatzBackOff[1]$Token )) KatzBackOff <- n_1[order(-Prob)]
    # remove NAs 
    KatzBackOff <- KatzBackOff[complete.cases(KatzBackOff),]
    # return data.table with best matches
    KatzBackOff
}

# FUNCTION return inptext input filtered
text_return <- function(inptext){
    # remove punctuation, preserving compounded words with dashes (e.g.:money-making)
    # convert to lowercase, and split into words
    text_words <- txt.to.words.ext(inptext, language="English.all", preserve.case = F)
    # replace contraction symbol 
    TextCorrectedWords <- gsub("\\^", "'", text_words)
    # profanity filter
    profanity <- "fuck|shit|bitch|cunt|damn|asshole|\\<ass\\>"
    profanity_index <- grep(profanity, TextCorrectedWords)
    TextCorrectedWords[profanity_index] <-"PROFANITY_WORD"
    text_r <- paste(TextCorrectedWords, sep="", collapse=" ")
    # return filtered input
    text_r
}

