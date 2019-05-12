library(stringr)

#install.packages("arabicStemR")
library('arabicStemR')

#install.packages("tm")
library("tm")

#install.packages("corpus")
library(corpus)

#install.packages("readr")
library(readr)

#stopwords
stopwords1 <- read.csv("StopWords2.csv",header = FALSE, encoding = "UTF-8") 
stopwords1 <- as.character(stopwords1$V2)

#verb
Verbs <- read_csv("Verbs - Verbs.csv")
View(Verbs)
df1 <- Verbs[,c("unvocalized","normalized","root")]
typ <- "verb"
df1 <- cbind(df1, typ)

#noun
Noun <- read_csv(file="Nouns - Nouns.csv")
View(Noun)
Noun$root[1] <-  "اسم فاعل"
Noun$root[2] <-  "شذ"
library(zoo)

Noun$root <- na.locf(Noun$root)


df2 <- Noun[,c("unvocalized","normalized","root")]
typ <- "noun"
df2 <- cbind(df2,typ)


#nouns and verbs dataframe 
df <- rbind(df1,df2)


text_stem <- function(text88){
  
  text88 <- removeDiacritics(text88)
  text88 <- removePrefixes(text88, x1= 6)
  text88 <- str_replace_all(text88, "[\r/n]" , " ")
  text9 <-removeStopWords(text88, defaultStopwordList=FALSE, customStopwordList=stopwords1)$text
  
  
  split1 <- str_split(text9," ")
  
  split2 <- list()
  nounCount = 0
  verbCount = 0
  N <- length(split1[[1]])
  for(i in 1:N){
    m <- split1[[1]][i]
    k <- match(m, df$unvocalized)
    
    if (is.na(k)) {
      stem <- m
    } else {
      if(df$typ[k] == "noun"){
        stem <- df$root[[k]]
        nounCount = nounCount+1
      }
      else{
        stem <- df$root[[k]]
        verbCount = verbCount+1
        
      }
    }
    split2[[paste0(i)]] <-stem
    
  }
  
  
  newtext <- paste(split2,collapse=" ")
  my_list <- list("text_after_stemming" = newtext, "VerbCount" = verbCount, "NounCount" = nounCount)
  return(my_list)
}


#=================================================================================
#
##تجهيز النص 

text8 <- readLines("ScientificResearch.txt", encoding = "UTF-8")
text8 <- paste(text8, collapse="/n ")



#text <-  ""

newText <- text_stem(text8)

newText$text_after_stemming # لعرض النص بعد اخذ الجذر لكل كلمة

newText$VerbCount # لعرض عدد الافعال التي تم أخذ الجذر لها

newText$NounCount # لعرض عدد الاسماء التي تم أخذ الجذر لها 






text10 <- "وحدان واحد عامل"

newText <- text_stem(text10)

