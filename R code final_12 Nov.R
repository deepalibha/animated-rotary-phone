library(XML)
library(methods)
options(max.print=999999)

#code for xml extraction
i <- 1
# Defining string1 and string3 outside while loop, if we keep inside then same evaluation 48 times
String1 <- "http://api.indeed.com/ads/apisearch?publisher=8693092939388569&q=data+scientist&sort=&radius=&st=&jt=&start="
String3 <- "&limit=2000&fromage=&filter=&latlong=1&co=in&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2" 
df_total <- data.frame()
while(i <= 48)
{
  
  String2 <- as.character(1+25*(i-1))
  # Combine the strings 
  String <- paste(String1,String2,String3)
  # Parse the url
  results <- xmlParse(String, isURL=TRUE)
  print(results)
  #extract root nodes
  rootnode <- xmlRoot(results)
  rootsize <- xmlSize(rootnode)
  print(rootsize)
  jobtitle <- xmlToDataFrame(getNodeSet(results, c("//jobtitle"))) 
  company <- xmlToDataFrame(getNodeSet(results, c("//company"))) 
  city <- xmlToDataFrame(getNodeSet(results, c("//city"))) 
  state <- xmlToDataFrame(getNodeSet(results, c("//state"))) 
  country <- xmlToDataFrame(getNodeSet(results, c("//country"))) 
  formattedlocation <- xmlToDataFrame(getNodeSet(results, c("//formattedLocationFull"))) 
  source <- xmlToDataFrame(getNodeSet(results, c("//source"))) 
  date <- xmlToDataFrame(getNodeSet(results, c("//date"))) 
  snippet <- xmlToDataFrame(getNodeSet(results, c("//snippet"))) 
  expired <- xmlToDataFrame(getNodeSet(results, c("//expired"))) 
  combinenodes <- c("jobtitle","company","city","state", "country", "formattedlocation", "source", "date","snippet", "expired")
  print(combinenodes)
  indeed_df <- data.frame(jobtitle,company,city,state,country,formattedlocation,source,date,snippet,expired)
  print(indeed_df)
  colnames(indeed_df)
  library(plyr)
  rename(indeed_df, c("text"="jobtitle","text.1"="company","text.2"="city","text.3"="state","text.4"="country","text.5"="formattedLocationFull","text.6"="source","text.7"="date","text.8"="snippet","text.9"="expired" ))
  colnames(indeed_df)
  names(indeed_df)[names(indeed_df)=="text"] <- "jobtitle"
  names(indeed_df)[names(indeed_df)=="text.1"] <- "company"
  names(indeed_df)[names(indeed_df)=="text.2"] <- "city"
  names(indeed_df)[names(indeed_df)=="text.3"] <- "state"
  names(indeed_df)[names(indeed_df)=="text.4"] <- "country"
  names(indeed_df)[names(indeed_df)=="text.5"] <- "formattedLocationFull"
  names(indeed_df)[names(indeed_df)=="text.6"] <- "source"
  names(indeed_df)[names(indeed_df)=="text.7"] <- "date"
  names(indeed_df)[names(indeed_df)=="text.8"] <- "snippet"
  names(indeed_df)[names(indeed_df)=="text.9"] <- "expired"
  indeed_df
  df_total <- rbind(df_total,indeed_df) # append current 'df' in loop to master dataframe 
  i = i + 1 # this step is important otherwise we would end up in an infinite loop 
  
}
df_total
write.table(df_total, file = "indeed3.csv", sep = ",", col.names = TRUE,qmethod = "double")

# code to now extract string characters and append on existing dataframe.
if(grepl("Machine Learning Techniques", "df_total$snippet")) print("MLT") else print("not found")
library(plyr)
library(dplyr)
library(magrittr)

#import the indeed3 csv 
myfileindeed <-read.csv("C:/Users/deepa/OneDrive/Documents/RProject/indeed3_v2.csv")

#view the indeed file
View(myfileindeed)

#convert the indeed3 csv to df
df_total <-myfileindeed

#to not have any limits on data count
options(max.print=999999)

#test with filter
sql_df <- filter(df_total$snippet=="sql")#not worked

#trying Vishal code of grepl
keywords <-c("machine", "python", "sql")


strings<- df_total$snippet

if(keywords <-"machine")
  strings<- df_total$snippet
(matchesmachine<- sapply (keywords, grepl, strings, ignore.case= TRUE))
ifelse(keywords <-"python")
strings<- df_total$snippet
(matchespy<- sapply (keywords, grepl, strings, ignore.case= TRUE))
ifelse(keywords <-"sql")
strings<- df_total$snippet
(matchessql<- sapply (keywords, grepl, strings, ignore.case= TRUE))

dfmachine <- matchesmachine
dfpy <- matchespy
dfsql <- matchessql

final_total <- full_join(df_total, dfmachine)

library(plyr)
library(dplyr)


#try cbind and give the dataframe final_total name
final_total <- cbind(df_total,dfmachine, dfpy, dfsql)

#to export the dataframe into a file
write.table(final_total, file = "finalindeed.csv", sep = ",", col.names = TRUE,qmethod = "double")
library(plyr)
library(dplyr)
#make a new df that has only the following columns from the df and give it a new name as newdf
new_indeed <- final_total%>%select(jobtitle,company,city,state,country,formattedLocationFull,source,date,snippet,expired,python,machine,sql)


library(plyr)

#export new file into csv
write.table(new_indeed, file = "finalindeed1.csv", sep = ",", col.names = TRUE,qmethod = "double")

#To convert all the capitals in a dataframe into small words
dflower <- mutate_each(new_indeed, funs(tolower))

#attempt to find unique values in the dataframe(not successful)
data %>% group_by(dflower$company, dflower$jobtitle) %>% summarize(count=n())

#attempt to find unique values of company in the dataframelower(worked)
as.data.frame(table(dflower$company))

#convert the above table as an object
freqcompany <- as.data.frame(table(dflower$company))

#write the freqcompany data frame into csv
write.table(freqcompany, file = "frequencycompany.csv", sep = ",", col.names = TRUE,qmethod = "double")

#attempt 1 from fequcompany dataframe arrange the company frequency in ascending order
freqcompany %>% 
  group_by(Var1) %>% 
  summarise(cnt= Freq()) %>% 
  arrange(desc(cnt))

#Worked attempt 2 from frecompany to order the frequency

freqcompany %>%
  group_by(Var1) %>%
  tally()

#to convert the arranged freq into a dataframe
freqcompanyarranged <- freqcompany %>%
  group_by(Var1) %>%
  tally()

#attempt worked in arranging company names in ascending order
freqcompany %>%
  filter(Freq > 1) %>%
  arrange(desc(Freq))

# to convert the nicely arranged data into a datframe
finalcompanyfreq <- freqcompany %>%
  filter(Freq > 1) %>%
  arrange(desc(Freq))

#to write the finalfreqcompany into a csv
write.table(finalcompanyfreq, file = "finalarrangedfrequencycompany.csv", sep = ",", col.names = TRUE,qmethod = "double")

#To find unique values for states in the dataframelower(worked)
as.data.frame(table(dflower$state))

#To convert the state data into an object
freqstate <- as.data.frame(table(dflower$state))


# worked dplyr effort of renaming rename the columns in the freqstate as Frequencystate and states
       rename(freqstate, states = Var1,
              Frequencystates = Freq)
#to create a new dataframe that has new names of the state columns
freqstaterename <-  rename(freqstate, states = Var1,
                           Frequencystates = Freq)

#to write the freqstaterename into a csv
write.table(freqstaterename, file = "finalarrangedfrequencystate.csv", sep = ",", col.names = TRUE,qmethod = "double")

#to write the freq state in ascending order
freqstaterename %>%
  filter(Frequencystates > 0) %>%
  arrange(desc(Frequencystates))

#to write the above into a dataframe
finalfreqstates <- freqstaterename %>%
  filter(Frequencystates > 0) %>%
  arrange(desc(Frequencystates))

#to write the final freq state into the csv
write.table(finalfreqstates, file = "finalarrangedfrequencystate1.csv", sep = ",", col.names = TRUE,qmethod = "double")

#To slice the character column from the dataframe for textmining
charactercolumn <- dflower ["snippet"]

#to write the character column into a csv
write.table(charactercolumn, file = "charactercolumn.csv", sep = ",", col.names = TRUE,qmethod = "double")

#choose cran before textmining
chooseCRANmirror(50)

#install relevant packages for textmining
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    
50

#using corpus to store our text
corpuscharacter<- Corpus(VectorSource(charactercolumn))

##remove punctuation from the corpus
corpuscharacter <- tm_map(corpuscharacter, removePunctuation)

#To see what stopwords are automatically inbuilt
stopwords("english") [1:30]

#To remove the english inbilt words but also the word "data" as this is all about apple anyways
corpuscharacter <- tm_map(corpuscharacter, removeWords, c("data", "scienti","work", "are", "looking","if", "must", "have", "profile", "to", "description", stopwords("english")))

#To see the stem document
corpuscharacter <- tm_map(corpuscharacter, stemDocument)

#to treat preprocessed documents as text documents, only be done after preprocessing done
corpuscharacter <- tm_map(corpuscharacter, PlainTextDocument) 

#To make a matrix of text
frequenciescharacter <- DocumentTermMatrix(corpuscharacter)
frequenciescharacter

inspect(frequenciescharacter)

#see the popular terms
frequentcharacters <- findFreqTerms(frequenciescharacter, lowfreq= 10) #lowfreq means, list the popular terms which have repeated atleast 20 times
frequentcharacters

#plot a colorful wordcloud of corpuscharacter(worked)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2") 
wordcloud(corpuscharacter, max.words = 20, random.order = FALSE, colors=dark2)

# save the image in png format
png("datascientistwc1.png", width=20, height=10, units="in", res=300)
wordcloud(corpuscharacter, max.words = 100, random.order = FALSE, colors=dark2)
dev.off()

inspect(corpuscharacter)
View(corpuscharacter)

corpuscharacter$content

#to write the corpuscharacter into the csv
write.table(corpuscharacter, file = "corpuscharacter1.docx", sep = ",", col.names = TRUE,qmethod = "double")


getwd()

library(plyr)
library(dplyr)

#since I cannot find my dflower dataframe in env, I will convert a csv in the dflower dataframe again

View(dflower)
#Use pipe to find answers to which companies need machine, sql and Python (technical skills)
technicalskills <- filter(dflower, python== "true"& sql== "true" & machine== "true")

#to write the technical skills into a csv
write.table(technicalskills, file = "technicalskills1.csv", sep = ",", col.names = TRUE,qmethod = "double")

#to answer the question what cities are hiring data scientists in India
as.data.frame(table(dflower$city))

#convert the above city table as an object
freqcity <- as.data.frame(table(dflower$city))

#Worked arrange from frecompany to order the frequency

freqcity %>%
  group_by(Var1) %>%
  tally()


# worked dplyr effort of renaming rename the columns (var1) in the freqcity cities
freqcityrenamed <- rename (freqcity, cities = Var1,
                          Frequencycities = Freq)

#arrange freq city renamed in ascending order
finalfreqcity <- freqcityrenamed %>%
  filter(Frequencycities > 0) %>%
  arrange(desc(Frequencycities))


#to write the final freq city into a csv
write.table(finalfreqcity, file = "finalfreqcity.csv", sep = ",", col.names = TRUE,qmethod = "double")



