library(lubridate)
library(stringr)


date<-today()
date

date

as.Date("09/28/2008", format = "%m / %d / %Y")

mdy("09/28/2008")


format(mdy("09/28/2008"), "%A, %d.%m.%Y")

####
#Calculate the average passengers count by day of the week

temp<-df %>% 
  rename("pickup"="tpep_pickup_datetime") %>% 
  select (passenger_count,pickup ) %>% 
  mutate(pickup_weekday=format(pickup,"%A")) %>% 
  head()

temp

format(temp$pickup,"%A")

df %>% 
  rename("pickup"="tpep_pickup_datetime") %>% 
  select (passenger_count,pickup ) %>% 
  mutate(pickup_weekday=ymd(pickup)) %>% 
  group_by(pickup_weekday) %>% 
  summarise(max_number_of_passengers=max(passenger_count, na.rm = TRUE),
            sum_of_passengers=sum(passenger_count, na.rm = TRUE),
            average_number_of_passengers=mean(passenger_count, na.rm=TRUE))



#Use as.Date() and an appropriate format to convert “08,30,1930” to a date (it is in the form of “month,day,year”)
#Use as.Date() and an appropriate format to convert “Aug 30,1930” to a date
#Use as.Date() and an appropriate format to convert “30aug1930” to a date

mdy("08,30,1930")
mdy("Aug 30,1930")
dmy("30aug1930")

as.Date("Aug 30,1930", format="%b %d,%Y")
as.Date("30aug1930", format="%d%B%Y")

### splitting text into abbr
#This requires combining str_split(), str_sub() and str_c().
names<-c("Sophia Abbe", "Olivia Abbett", "Emma Abbey", "Ava Abbitt", "Isabella Abbot", "Mia Abbott", "Aria Abbs", "Riley Abby", "Zoe Abdon", "Amelia Able", "Layla Abner", "Charlotte Abney", "Aubrey Aborn", "Lily Abrahams", "Chloe Abram", "Harper Abram", "Evelyn Ace", "Adalyn Acey", "Emily Acker", "Abigail Ackerley", "Madison Ackerly", "Aaliyah Ackerman", "Avery Ackers", "Ella Ackert", "Scarlett Ackland", "Maya Ackland", "Mila Ackley", "Nora Acklin", "Camilla Ackroyd", "Arianna Acock", "Eliana Acomb", "Hannah Acomb", "Leah Acors", "Ellie Acre", "Kaylee Acreman", "Kinsley Acres", "Hailey Acton", "Madelyn Acuff", "Paisley Acy", "Elizabeth Adams", "Addison Adcock", "Isabelle Adcox", "Anna Addams", "Sarah Adderley", "Brooklyn Adderly", "Mackenzie Addicott", "Victoria Addington", "Luna Addy", "Penelope Ade", "Grace Ades")

m<-str_split(names, pattern=" ", simplify = TRUE)
firstName<-m[,1]
lastName<-m[,2]

m<-str_split(names, pattern=" ", simplify = TRUE)
str_c(str_sub(m[,1],1,1),". ",m[,2], sep="")

## regular expressions
string <- c("car","bus")
pattern <- "car"
grep(pattern, string)




string <- c("car", "cars", "in a car", "truck", "car's trunk","ddsdscarfklfklfk")
pattern <- "car"
grepl(pattern, string)


string <- c("","car.", "ca.rs", "in a car", "tru\ck", "ca$r's trunk","c@rčvddlvdlč","chrglglgl")
grepl("^c..r",string)

grepl("^c..$",string)


grepl("[\\$]",string)

string<-c("Test","test one","% #","Hello")
grepl("\\w", string)
grepl("\\W", string)

grepl("\\S", c(" ", "a", "1", "A", "%", "\t"))

grepl("\\d", c(" ", "a", "1", "A", "%", "\t"))


df1<-data.frame(id=c(1,2,"d",4),name=c("John","Jane","Jim","Kate"))
df1

df1 %>% 
  dplyr::filter(grepl("\\D",id)) %>% 
  select(name)

## possible characters
grepl("^[abc]\\w\\w\\W", c("car", "bus", "no", "cars","b11 "))

grepl("^[abcC]\\w\\w$", c("Car","car", "bus", "no", "cars"))


grepl("^[A-Za-z][a-z][a-z]$", c("Car", "Cars", 
                            "cars","car", "no", "three:", "tic", "tac"))

grepl("^[1-9]\\d$", c("1", "20", "0", "zero", "it is 100%", "09","10today","today10s"))


string <- c("b","a", "ab", "acb", "accb", "acccb", "accccb","acacb","aaaacccccb","aaaabcccccb")

grepl("(ac)+b", string)

string <- c("a", "ab", "acb", "accb", "acccb", "accccb","abc")
grepl("ac?b", string)


## number of repetitions


string <- c("a", "ab", "acb","acacb", "accb", "acccb", "accccb")

grepl("(ac){1,4}b",string)


## repetitions

grepl("^[a-z]{3,5}$", c("words", "words or sentences",
                       "123 no", "Words"," word","word 123","hey"))


## gready. lazy

string<-"This is a <EM>first</EM> test>"
pattern<-"<.*(/EM>)"


r<-regexpr(pattern,string)
regmatches(string, r)

# TEXT
text<-"Yesterday I had 100 Euros, today I only have 45 Euros left."
pattern<-"\\d+"

list<-regmatches(text, gregexpr("\\d+",text))
length(unlist(list))


# tidytext
library(tidytext)
library(janeaustenr)


text<-austen_books() %>% select(text) %>% 
   unlist()

list<-regmatches(text, gregexpr("\\d+",text))
length(unlist(list))


list0<-austen_books() %>% 
  dplyr::select(text) %>% 
  dplyr::filter(text!="")%>% 
  unnest_tokens(words, text) %>% 
  dplyr::filter(grepl("^\\d+",words)) %>% 
  mutate(words=str_remove_all(words,"\\D")) %>% 
  unlist()

list1<-austen_books()$text
pattern<-"\\d+"

list1<-regmatches(list1, gregexpr("\\d+",v))
length(unlist(list1))



setdiff(list0, list)

list

text<-"I will order 2 drinks for the 2,nd time?Shut up!Hallo!"

df2<-data.frame(text)
df2

df2 %>% 
  unnest_tokens(words,text)



### select of tidyverse

mtcars %>% 
  relocate("wt",.before="mpg") %>% 
  select(where(is.numeric)) %>% 
  arrange(desc(wt)) %>% 
  head()


## advanced mutate
iris %>% 
  mutate(across(.cols=where(is.numeric),.fns = as.character)) %>% 
  mutate_if(is.factor, as.character) %>% 
   head() %>% str()


df %>% 
  select_all(toupper) %>% 
  clean_names() %>% 
  head()

#advanced renaming
#rename_with(.data, .fn, .cols = everything(), …)


colName <- function(x) {
  tmp <- stringr::str_sub(x, 1, 7)
  return(str_replace(tmp,"\\.", ""))
}


own_clean_names<-function(x){
  
  x1<-tolower(x)
  x2<-str_replace_all(x1,"\\.","_")
  
  return(x2)
}


v<-"Sepal.Length"
v1<-"Sepal(?*L"
str_replace_all(v1,"[[:punct:]]", "")

iris%>%rename_with(own_clean_names,everything())%>%head(3)


## difference between mutate and summarise

ageDF<-data.frame(age=c(34,54,67,23))

ageDF %>% 
  mutate(meanage=mean(age))

ageDF %>% 
  summarize(meanage=mean(age))

age=c(34,54,67,23)
mean(age)
