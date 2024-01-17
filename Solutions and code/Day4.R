# n() counting the number of rows

as_tibble(iris) %>% 
  summarise(numberOfRows=n())


as_tibble(iris) %>% 
  group_by(Species) %>% 
  summarise(averagePetal=mean(Petal.Length), 
            averageSepal=mean(Sepal.Width))



## across(.columns, .fns,.names)
as_tibble(mtcars) %>% 
  mutate(cyl=factor(cyl)) %>% 
  group_by(cyl) %>% 
  summarize(across(.cols=where(is.numeric),
                   .fns=list(MEAN=mean,
                             MEDIAN=median,
                             S=sum),
                   .names = ("{.fn}_{.col}")))


## iris

as_tibble(iris) %>% 
  summarize(across(.cols=where(is.numeric),
                   .fns=list(MEAN=mean,
                             MEDIAN=median,
                             S=sum),
                   .names = ("{.fn}_{.col}")))
  
## change all numeric to character
as_tibble(iris) %>% 
  mutate(across(.fns=as.character)) 



## slicers - slice_max

as_tibble(mtcars) %>% 
    slice_sample(prop=0.2)


## using count function

data<-read.csv("https://raw.githubusercontent.com/urosgodnov/datasets/master/Master.csv")

data %>% 
  count(birthYear) %>% 
  head(5)

data %>% 
  select(birthCountry, weight) %>% 
  mutate(weight=round(weight/2.2,0)) %>% 
  group_by(birthCountry) %>% 
  summarise(number=n(), average_weight=mean(weight, na.rm=TRUE)) %>% 
  dplyr::filter(number>5 & birthCountry!="")


## windowf functions

df<-iris%>%slice_sample(n=10)

df%>%mutate(groups=ntile(Species,5),
            minrank=min_rank(Species), denserank=dense_rank(Species))%>%
  select(-contains("Petal"),-contains("Sepal"))

## get me the 10th largest Virginica according to Sepal.Lenght

iris %>% 
  dplyr::filter(Species=="virginica") %>% 
  arrange(desc(Sepal.Length)) %>% 
  mutate(id=row_number()) %>% 
  dplyr::filter(id==10)
  

## pivot wider

df<-airquality%>%select(Month, Day, Temp)%>%
    dplyr::filter(Month %in% c(5,6) & Day<10)%>%
    pivot_wider(names_prefix = "Month ", names_from=Month, values_from=Temp)
df


## pivot longer

df<-data.frame(day=c("Monday", "Tuesday","Wednesday"),month_aug=c(46,76,32),
               month_sep=c(62,67,23), month_oct=c(43,NA,31))

df%>%pivot_longer(cols=month_aug:month_oct, names_to = "Month", 
                  values_to = "Temperature")

## user defined functions and tidyverse
### rowwise approach

as_tibble(airquality) %>% 
  rowwise() %>% 
  mutate(sum_month_day=mean(c(Temp,Month))+Day)


## second approach
noName<-function(x,y,z){
  
    res<-mean(c(x,y))+z
    return (res)
}



### vectorized function
noName_v<-Vectorize(noName)

as_tibble(airquality) %>% 
  mutate(sum_month_day=noName_v(Temp, Month, Day))


## mtcars

# write a function which will tranform mpg to l/100 kms and assign values to a new column consumption
# display mean and median for mpg, hp, consumption
# identify the worst and best economic car

mpg_to_l_per_km<-function(x) {
  
  res<-round(235.21/x,1)
  
}

mtcars %>% 
  mutate(consumption=mpg_to_l_per_km(mpg)) %>% 
  select(mpg, hp, consumption) %>% 
  mutate(worst=min(mpg),
         best=max(mpg)) %>% 
  dplyr::filter(mpg==worst | mpg==best)




mtcars %>% 
  select(mpg) %>% 
  summarize(min=min(mpg), max=max(mpg))



### join 

product<-data.frame(id=1:4, product=c("A","B","C","D"),
                    price=c(sample(100,4)))


sales<-data.frame(sale=c(1,1,2,3,2,3,5),quantity=sample(100,7))

product
sales

#### inner join
product %>% 
  inner_join(sales, by=c("id"="sale")) %>% 
  select(product, quantity) %>% 
  group_by(product) %>% 
  summarize(sum(quantity))


#### anti  join
product %>% 
  anti_join(sales, by=c("id"="sale")) 


#### semi  join
product %>% 
  semi_join(sales, by=c("id"="sale")) 



### case when
mtcars %>% 
  mutate(Type_of_a_car=case_when(mpg<20~"it drinks too much!",
                                 mpg<24~"it is so, so",
                                 TRUE~"economic car")) %>% 
  select(-c(cyl:carb)) %>% 
  as_tibble()



### complex tidyverse
library(broom)

models<-mtcars%>%select(cyl,mpg,wt)%>%
  nest(-cyl)%>%
  mutate(model=map(data,function(x) lm(formula=mpg~wt,data=x)))

models%>%mutate(tidyModel=map(model, tidy))%>%
  unnest(tidyModel)


lmmodel<-lm(mpg~wt+cyl+gear+am, data=mtcars)

tidy(lmmodel)

mpg=43.2-3.09*wt-1.57*cyl-1.08*gear+1.38*am


### representation

library(gt)
library(lubridate)

sp500 %>% 
  dplyr::filter(between(date,ymd("2015-12-24"),ymd("2015-12-31")))%>%
  select(-adj_close) %>% arrange(date) %>% gt() 
