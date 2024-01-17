## most basic plot

index<-data.frame(year=2007:2016, pop=sample(10000:20000, size=10))

plot(index$year,index$pop, type="l", main="Population by year",
     xlab="year",ylab="population")


# use mtcars dataset and create a line plot 
# where cyl must be on x axis and mean mpg on y axis

df<-mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean(mpg))
df

plot(df$cyl,df$`mean(mpg)`, type='b', xlab="number of cylinders",
     ylab="average mpg")


## load lattice
library(lattice)

dotplot(year~pop,data=index,main="Population by year",
                 ylab="year",xlab="population")

lattice::barchart(pop~factor(year),data=index,main="Population by year",
        xlab="year",ylab="population")


lattice::dotplot(mpg~wt|cyl,data=mtcars)


# use mtcars dataset and create a barchart 
# of mean mpg for number of gears per cyl
df<-mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(mean(mpg)) %>% 
  rename("mpg"="mean(mpg)")
df

lattice::barchart(mpg~factor(gear)|factor(cyl),data=df)


### ggplot2

## basic objects
faithful %>% 
  as_tibble()

#lattice::dotplot(eruptions~waiting, data=faithful)


ggplot(data=faithful,aes(x=waiting, y=eruptions))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)


ggplot(faithful) +
  aes(x = waiting, y = eruptions) +
  geom_point()


ggplot(data=faithful)+
  geom_point(mapping=aes(x=eruptions,
                         y=waiting))


faithful

## adding a colour

ggplot(faithful,aes(x=eruptions,y=waiting,
                        colour=eruptions<3))+
  geom_point()


mtcars %>% 
  ggplot(aes(x=wt,y=mpg, colour=mpg<20))+
  geom_point(size=3)+
  geom_label(aes(label=rownames(mtcars)))
  

## 2 different layers

ggplot(data=faithful,
       mapping=aes(x=eruptions,
                   y=waiting))+
  geom_density_2d()+
  geom_point()+
  geom_vline(aes(xintercept = 3, colour = "red", size=2))+
  geom_text(aes(x=2,y=90, label="Eruptions aren't joke!"))



## summary statistics
mpg %>% as_tibble()


ggplot(mpg,aes(class)) + 
  geom_bar()


mpg_counted <- mpg %>% 
  count(class, name = 'count')


ggplot(mpg_counted) + 
  geom_bar(aes(x = class, 
               y = count), 
           stat = 'identity')


ggplot(mpg,aes(x = class, y = hwy)) + 
  geom_boxplot(outlier.color = "blue", outlier.size = 4)+
  geom_jitter(width = 0.1)+
  stat_summary(aes(x = class, y = hwy),fun=mean, geom = "point", color="red",
               size=5)


ggplot(mpg,aes(x = displ, 
       y = hwy, 
       colour = class,
       label=model)) + 
  geom_point()+
  geom_label()

## ggrepel
library(ggrepel)

ggplot(mpg,aes(x = displ, 
               y = hwy, 
               colour = class,
               label=model)) + 
  geom_point()+
  geom_text_repel()


##scales

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, colour = class)) + 
  scale_colour_brewer(type = 'qual')


## transformations

ggplot(mpg,aes(x = displ, y = hwy)) + 
  geom_point(size=3)+
scale_x_continuous(breaks = seq(0,8,0.5)) 

  
## facet

# scale - fixed
ggplot(mpg,aes(x = displ, y = hwy)) + 
  geom_jitter(width = 0.3)+
  facet_grid(~cyl)

# scale - free
ggplot(mpg,aes(x = displ, y = hwy)) + 
  geom_jitter(width = 0.3)+
  facet_wrap(~cyl, scale="free")


##

ggplot(mpg,aes(y = manufacturer, fill=trans)) + 
  geom_bar()+
  facet_grid(class ~., scales = "free", space="free")+ 
  scale_fill_brewer(type = 'div')



mtcars %>% 
  as_tibble()

ggplot(mtcars, aes(wt,mpg))+
  geom_point(size=2.5)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(cyl~., scales="free")


lmmodel<-lm(mpg~wt+cyl+wt*cyl, data=mtcars)

interact_plot(lmmodel, pred = wt, modx = cyl)
summary(lmmodel)


library(interactions)


## coordinates

ggplot(mpg[mpg$class %in% c("suv","compact"),],aes(x = class)) + 
  geom_bar() +
  geom_hline(yintercept = 45, color="red")+ 
  coord_cartesian(ylim = c(45, 60))


ggplot(index,aes(factor(year),pop))+
  geom_bar(stat="identity", width = 0.3)+ 
  coord_cartesian(ylim = c(9000, 20000))


## themes

plot<-ggplot(mpg) + 
  geom_bar(aes(x = class)) +
  geom_hline(yintercept = 20, linetype ="dotted", color="red", size=2)+
  geom_text(aes(x=1,y=23, label="price=year*3.2"))

plot1<-plot + theme(axis.text.x = element_text(
    colour = "antiquewhite", angle = 35))

mpg

## ggrepel

ggplot(mtcars, aes(x=wt, y=mpg, label= row.names(mtcars)))+
  geom_point()+
  geom_label()

## ggforce

ggplot(iris, aes(Petal.Length, Sepal.Length))+
  geom_point(size=2)+
  facet_wrap(Species~., scales = "free")


## patcwork
library(patchwork)

p1<-ggplot(mtcars, aes(x=wt, y=mpg, label= row.names(mtcars)))+
  geom_point()+
  geom_label_repel()

p2<-ggplot(iris, aes(Petal.Length, Sepal.Length))+
  geom_point(size=2)+
  facet_wrap(Species~., scales = "free")

p3<-ggplot(mpg) + 
  geom_bar(aes(x = class)) +
  geom_hline(yintercept = 20, linetype ="dotted", color="red", size=2)+
  geom_text(aes(x=1,y=23, label="price=year*3.2"))


(p1  / 
  p2) |p3


## library(gganimate)

library(gganimate)

p5<-ggplot(economics, aes(x = date, y = unemploy)) + 
  geom_line()+
  theme_minimal()+
  transition_reveal(along = date)



ggplot(mpg) + 
  geom_bar(aes(x = factor(cyl))) + 
  labs(title = 'Number of cars in {closest_state} by number of cylinders') 
