data(beavers)
apply(t(beaver1), 1, max)


apply(beaver1, 2, max)

apply(mtcars, 2, mean)

apply(iris, 2, is.character)


l = list(a = 1:10, b = 11:20)

lapply(l, mean)

for (element in 1:length(l)) {
  tmp <- l[[element]]
  
  res[[element]] <- mean(tmp)
  
  
  
}
res


l = list(a = 1:10, b = 11:20)  # mean of values using sapply
l

sapply(l, mean)
lapply(l, mean)


### tapply

str(mtcars)
unique(mtcars$cyl)

tapply(mtcars$wt, mtcars$cyl, mean)

#Excercise 1
#a. Get the following matrix of 5 rows and call it ‘mymatrix’:
#  mymatrix = matrix(data = c(6,34,923,5,0, 112:116, 5,9,34,76,2, 545:549), nrow = 5)

mymatrix = matrix(data = c(6, 34, 923, 5, 0, 112:116, 5, 9, 34, 76, 2, 545:549),
                  nrow = 5)



#b. Get the mean of each row
#expected result


apply(mymatrix, 1, mean)

[1] 167.00 175.50 404.50 186.00 166.75

#c. Get the mean of each column


apply(mymatrix, 2, mean)


#expected result

[1] 193.6 114.0 25.2 547.0


# Excercise 2

# Use ‘lapply’ on a data.frame ‘mtcars’

#a. Use three ‘apply’ family functions to get the minimum values of each column of the ‘mtcars’ dataset. Store each output in a separate object (‘l’, ‘s’, ‘m’)
#and get the outputs.

l <- lapply(mtcars, min)
s <- sapply(mtcars, min)
m <- apply(mtcars, 2, min)

#b. Put the three outputs ‘l’, ‘s’, ‘m’ in the list ‘listobjects’.

listobject <- list(l, m, s)
listobject

#c. Use a suitable ‘apply’ function to get the class of each of the three list elements in ‘listobjects’
sapply(listobject, class)


# functions

pow <- function(x = 3, y = 3) {
  # function to print x raised to the power y
  result <- x ^ y
  print(str_c(x, "raised to the power", y, "is", result, sep = " "))
}


pow(2, 5)


check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result)
}

check(1)



multi_return <- function() {
  my_list <- list(airquality, iris, mtcars)
  return(my_list)
}

a <- multi_return()

class(a)

df1<-a[[1]]
df1
df2<-a[[2]]
df2


df<-as_tibble(airquality)
df<-df[,-2]
colnames(df)[5]<-"month name"
df

clean_names(df)


## function to return the lenght of a word

length_of_word <- function(name) {
  len <- nchar(name)
  
  return(len)
  
}



length_of_word("James")

### nestig functions


convert_fahr_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

convert_kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}


convert_fahr_to_celcius<-function(temp) {

  kelvin<-convert_fahr_to_kelvin(temp)
  
  celsius<-convert_kelvin_to_celsius(kelvin)
  
  return(celsius)
}

convert_fahr_to_celcius(100)


## Function lab

dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
# function: outside(dry_principle)
# expected result: "Don't"  "others"


outside<-function(word_vector) {
  
  #getting first element
  fe<-word_vector[1]
  #last element
  le<-word_vector[length(word_vector)]
  
  result<-c(fe,le)
}

input<-c("Don't", "repeat", "yourself", "or", "others")

res1<-outside(input)

res1


dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
# function: outside(dry_principle)
# expected result: "Don't"  "others"
outside <- function(x){
  return1 <- x[1]
  return2 <- x[length(dry_principle)]
  print(c(return1, return2))
}

outside(dry_principle)


dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
outside<-function(dry_principle){
  
  result<-c(dry_principle[1],dry_principle[length(dry_principle)])
}
res1<-outside(input)

res1


# scope

inner_func <- function() {

  print(a)
  a <- 30
}

outer_func <- function() {

  inner_func()
  print(a)
  a <- 20
}
#################################
a <- 10
print(a)
outer_func()
print(a)



a<-10
outer_func <- function(){
  inner_func <- function(){
    a <<- 30
    print(a)
  }
  inner_func()
  print(a)
}

outer_func()
print(a)


a<-2
outside_function<-function() {
  
  #not defined
  print(a)
  a<-1
  print(a)
  
}
outside_function()

# outside
print(a)



read_files<-function(path, i) {
  for (file in path) {
    l[[i]] <<- read.csv2(file)
     i<-i+1
    }
}


l<-list()
i<-1
path<-list.files("./Mtcars/", full.names=TRUE)

read_files(path, i)

l[[1]]<-c(1,4)
l[[2]]<-iris
l[[3]]<-airquality


## anonymous function

multiply_function<-function(x) { 3 * x }


lapply(list(1,2,3), function(element) {3*element})


sapply(list(1,2,3), function(x,y) { 3 * x*2 })




#readfiles<-function
#for loop
# read all csv files from directory
## in each loop, assign a file to a list
## l[i]<<-read.csv(some file)


### tasks - functions

#Exercise 4

#Create a function that given a vector and an integer will return how many times 
#the integer appears inside the vector.

vector<-c(4,6,2,5,6,8,9,6)
integer<-6

sum(vector==6)


number_of_integers<-function(vector,integer) {
  
  res<-length(vector[vector==integer])
  
  
}

print(number_of_integers(vector,9))


##Exercise 5

#Create a function that given an integer will calculate 
#how many divisors it has (other than 1 and itself). Make the divisors appear on screen.


50/2



all_divisors <- function(input) {
  divisorV <- NULL
  number <- input / 2
  
  for (divisior in 2:(number + 1)) {
    if (input %% divisior == 0)
      divisorV <- c(divisorV, divisior)
  }
  
  print(divisorV)
}


b<-Sys.time()
all_divisors(200000000)
Sys.time()-b



## error handling

vector <-c(1:4,"a","b", 7:12, NA, NULL)
vector

for (element in vector) {
  
  try(sqrt(element))
  
  print("Hello!")
}


## try catch
for (element in vector) {
  tryCatch(
    expr = {
      sqrt(element)
    },
    error = function(e) {
      next
    }
  )
  print("Hello!")
}


for (element in c("Covid19")) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    print(sqrt((element))),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  print("Hello world!")
  
}


x <- c("2022-02-01", "Hello!", "2022-02-16")

dateConversion <- function(x) {
  return(tryCatch(as.Date(x), error=function(e) NA))
}

result <- lapply(x, dateConversion)
result


# dynamic evaluation

formula<-"2**3"
expression<-parse(text=formula)
expression

eval(expression)

colnames(mtcars[,c(1,3:11)])

for (column in colnames(mtcars[,c(1,3:11)])){
   
    expression<-parse(text=str_c("mtcars$",column, sep=""))

    print (column)    
    print(tapply(eval(expression),mtcars$cyl, mean))
  
}


summary(mtcars)

makeDataReport(mtcars, output="html", file="EDA.html")

report(mtcars, output_file="report1.html", output_dir=".")

skim(mtcars)



#### Lubridate

date<-"2019-05-05"
time<-"18:51:32"

datetime<-str_c(date,time, sep=" ")

dt<-as_datetime(datetime)

class(dt)

month(dt, label=TRUE, abbr = FALSE, locale="Swedish")

day(dt)

wday(dt, label=TRUE, abbr=FALSE, locale="French")

today()

ymd("2017-01-31")

current_locale<-Sys.getlocale()

Sys.setlocale(locale="English")

mdy("enero 31, 2017")

dmy("31-Jan-2017")

## [1] "2017-01-31"

mdy("10/17/2018")

nycDf<-read.csv("./Tasks/yellow_tripdata_2020-02.csv")
sapply(nycDf, class)

tempDF<-nycDf %>% 
  select(VendorID : passenger_count) %>%
  
  mutate(pickup=ymd_hms(tpep_pickup_datetime),
         drop=ymd_hms(tpep_dropoff_datetime),
         drive_duration=drop-pickup,
         pickup_day=wday(pickup, label=TRUE, abbr = FALSE)) 



tempDF %>% head()

res<-summary(tempDF$pickup_day)

res[order(res)]

sapply(tempDF, class)


## Day of the week you were born

date<-ymd("1975-09-11")
wday(date, label = TRUE, abbr=FALSE)


## extract letters name

vector<-"James"

str_split(vector, pattern="",simplify = TRUE)

for (letter in vector) 
  
for (i in 1:nchar(vector)) {  
  print(substr(vector,i,i))
}

today<-ymd(today())
wday(today, label=TRUE)

forty<-today+dyears(40)

wday(forty, label=TRUE)

