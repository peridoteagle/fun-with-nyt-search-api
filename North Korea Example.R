#Please load the following libraries
library(XML)
library(rtimes)
library(stringr)
library(tm)
library(httr)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tibble)
library(quanteda)
library(wordcloud2)
library(ldatuning)
library(collapsibleTree)
library(reshape2)
library(htmlwidgets)

#Set the following NYT Key:
Sys.setenv(NYTIMES_AS_KEY = "4c9502e9f88547acbf32471755c635ac")

#Obtaining articles from the NYT Search API
#Search terms and start and end date can be adjusted
keywords <- "north korea"
startdate <- '20170101'
enddate <- '20171231'
#narticles is the number of articles you want
n <- 100
nover10 <- n/10

urllist<-c()
urls <- c()
headlabel <-c()
headline <- c()
kicklabel <- c()
kicker <- c()
seclabel <- c()
section <- c()
dpublabel <- c()
datepub <- c()
newslabel <- c()
newsdesk <- c()
typelabel <-c()
typematerial <- c()
bylinelabel <- c()
byline <- c()
i=0
for (i in 0:nover10){
  Sys.sleep(1)
  tenarticles <- as_search(q=keywords,start_date =startdate, end_date = enddate,page=i)
  urllist <- tenarticles$data$web_url
  print(length(urllist))
  urls <- c(urls,urllist)
  headlabel <-tenarticles$data$headline.main
  headline <- c(headline,headlabel)
  kicklabel <- tenarticles$data$headline.kicker
  kicker <- c(kicker,kicklabel)
  seclabel <- tenarticles$data$section_name
  section <- c(section,seclabel)
  bylinelabel <- tenarticles$data$byline.original
  byline <- c(byline,bylinelabel)
  dpublabel <- tenarticles$data$pub_date
  datepub <- c(datepub,dpublabel)
  newslabel <- tenarticles$data$new_desk
  newsdesk <- c(newsdesk,newslabel)
  typelabel <- tenarticles$data$type_of_material
  typematerial <- c(typematerial,typelabel)
}

#Creating a dataframe from the variables created in the for loop
alldata <-  data.frame(urls,headline,kicker,section,byline,datepub,newsdesk,typematerial)

#Selecting a subset of the articles
#Selecting only those from a specific section (example: "Politics")
#relevantarticles <- subset(alldata,section=="Corner Office")

#Selecting only those from a specific newsdesk (example: "Foreign")
#relevantarticles <- subset(alldata,newsdesk=="Foreign")

#Selecting only those from a specific typematerial (example: "News")
relevantarticles <- subset(alldata,typematerial=="News")

relevanturls <- as.character(relevantarticles$urls)

#Defining function to parse URL for article body
parseArticleBody <- function(artHTML) {
  xpath2try <- c('//div[@class="articleBody"]//p',
                 '//p[@class="story-body-text story-content"]',
                 '//p[@class="story-body-text"]'
  )
  for(xp in xpath2try) {
    bodyi <- paste(xpathSApply(htmlParse(artHTML), xp, xmlValue), collapse = "")
    if(nchar(bodyi)>0) break
  }
  return(bodyi)
}

#Getting the article body
articletext <- c()
j=1
for (j in 1:length(relevanturls)){
  p <- GET(relevanturls[j])
  html <- content(p, 'text')
  artBody <- parseArticleBody(html)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  articletext <- c(articletext,artBody)
}

#Creating the corpus and setting the variables in the corpus
docs2 <- VCorpus(VectorSource(articletext))
meta(docs2,tag="heading",type="local") <- as.character(relevantarticles$headline)
meta(docs2,tag="author",type="local") <- as.character(relevantarticles$byline)
meta(docs2,tag="datetimestamp",type="local") <- as.character(relevantarticles$datepub)
meta(docs2,tag="description",type="local") <- as.character(relevantarticles$urls)

docs <- docs2

#Convert symbols to spaces
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "—")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove common English stopwords
docs <- tm_map(docs, removeWords,c("a",	"about",	"above",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"below",	"between",	"both",	"but",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"few",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"is",	"isn't",	"it",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"no",	"nor",	"not",	"of",	"off",	"on",	"once",	"only",	"or",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"than",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"to",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves"))
#Removing contractions since they are generally made of stop words
docs <- tm_map(docs, removeWords,c("ain't",	"aren't",	"can't",	"can't've",	"'cause",	"could've",	"couldn't",	"couldn't've",	"didn't",	"doesn't",	"don't",	"hadn't",	"hadn't've",	"hasn't",	"haven't",	"he'd",	"he'd've",	"he'll",	"he'll've",	"he's",	"how'd",	"how'd'y",	"how'll",	"how's",	"I'd",	"I'd've",	"I'll",	"I'll've",	"I'm",	"I've",	"i'd",	"i'd've",	"i'll",	"i'll've",	"i'm",	"i've",	"isn't",	"it'd",	"it'd've",	"it'll",	"it'll've",	"it's",	"let's",	"ma'am",	"mayn't",	"might've",	"mightn't",	"mightn't've",	"must've",	"mustn't",	"mustn't've",	"needn't",	"needn't've",	"o'clock",	"oughtn't",	"oughtn't've",	"shan't",	"sha'n't",	"shan't've",	"she'd",	"she'd've",	"she'll",	"she'll've",	"she's",	"should've",	"shouldn't",	"shouldn't've",	"so've",	"so's",	"that'd",	"that'd've",	"that's",	"there'd",	"there'd've",	"there's",	"they'd",	"they'd've",	"they'll",	"they'll've",	"they're",	"they've",	"to've",	"wasn't",	"we'd",	"we'd've",	"we'll",	"we'll've",	"we're",	"we've",	"weren't",	"what'll",	"what'll've",	"what're",	"what's",	"what've",	"when's",	"when've",	"where'd",	"where's",	"where've",	"who'll",	"who'll've",	"who's",	"who've",	"why's",	"why've",	"will've",	"won't",	"won't've",	"would've",	"wouldn't",	"wouldn't've",	"y'all",	"y'all'd",	"y'all'd've",	"y'all're",	"y'all've",	"you'd",	"you'd've",	"you'll",	"you'll've",	"you're",	"you've"))

# Remove punctuation marks
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#The following code was written to remove the most common words after looking at the most common words after stemming that did not provide significant interpretation
docs <- tm_map(docs, removeWords,c(" ’s","“","“", "’ve"," ’ll"," ’d"," ’m","didn’t","don’t"," ’re","just","mean","like","yes","thanks","know","get","say","okay","first","mmhmm","well","’ve ","“","—","”","can’t ","â\u0080\u009c"))

#Creating a set of non-stemmed words for a word cloud
docsnostem <-docs

# Text stemming using Porter stemming
docs <- tm_map(docs, stemDocument)

################# TERM FREQUENCY ######################

#Term-document matrix for most frequent words NOT STEMMED:
tdm2 <- TermDocumentMatrix(docsnostem)
#Counting the Top 10 most frequent words
m2 <- as.matrix(tdm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)
#Wordcloud of these words
wordcloud <- wordcloud2(d2)
wordcloud

################ LDA FOR TOPIC ANALYSIS ######################

#Document Term Matrix for LDA
docterm <- DocumentTermMatrix(docs)

#Selecting only the rows greated than 0
rowTotals <- apply(docterm , 1, sum) #Find the sum of words in each Document
ttdm.new   <- docterm[rowTotals> 0, ] 

#Choosing number of topics to run for LDA
#This code can take A TON of time to run depending on the seq you use
#Recommendation: start broad (example: 2 to 30 counted by 10)
#Then narrow in (final try was 23 to 26 counted by 1)
result <- FindTopicsNumber(
  ttdm.new,
  topics = seq(from = 23, to = 26, by = 1),
  metrics = c("Griffiths2004"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
#In this example, 24 maximizes Griffiths

#Applying LDA to the documents
ap_lda2 <- LDA(ttdm.new, k = 24, control = list(seed = 1234))
chapter_topics <- tidy(ap_lda2, matrix = "beta")

#Finding the top terms per topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(3, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

options(tibble.print_max=Inf)
top_terms

toptermsbytopic <- as.data.frame(top_terms)

cTree<-collapsibleTree(
  toptermsbytopic,
  hierarchy = c("topic", "term"),
  width = 500, height = 500, zoomable = FALSE, tooltip = TRUE
)
cTree
saveWidget(cTree,file="ctree.html")

#Seeing which documents are most closely related to a specific topic
ap_documents <- tidy(ap_lda2, matrix = "gamma")

top_docs <- ap_documents %>%
  group_by(topic) %>%
  top_n(3, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

top_docs
#Example interpretation: Doc 55 is most closely related to Topic 1
#Doc 55 headline
docs2[[55]]$meta$heading

topdocs<- as.data.frame(top_docs)
topdocs

topdocs$heading <- c()
i=1
for (i in 1:length(topdocs$document)) {
  j <- topdocs[i,1]
  topdocs$heading[i] <- docs2[[j]]$meta$heading
}

concat <- toptermsbytopic %>%
  group_by(topic) %>%
  summarise(vector=paste(term, collapse=", "))

concatterms <- as.data.frame(concat)

mergetermsdocs <- merge(concatterms,topdocs,by='topic')

cTree<-collapsibleTree(
  mergetermsdocs,
  hierarchy = c("topic","vector","heading"),
  width = 1000, height = 500, zoomable = FALSE, tooltip = TRUE
)
cTree
saveWidget(cTree,file="ctreenorthkorea.html")
