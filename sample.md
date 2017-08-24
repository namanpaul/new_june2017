# knitcheck
namanpaul  
August 23, 2017  

# hw003
namanpaul  
October 5, 2015  

***Exploring dplyr***

---


***Loading the packages***



```r
library(reshape2)
library(gapminder)
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(ggplot2)
library(knitr)
```



***Task 1 : Get the maximum GDP per capita for all continents***

I used the piping here, inorder to retrieve data for the continents. 
The expression also groups the *gap_df* data by *continent*, summarizing the minimum and maximum GDP Per capita.



```r
gap_df <- tbl_df(gapminder)

#my piping attempt
#gap_df %>% glimpse
#a bit complicated at first, I should improve on this soon
min_max_GDP_cont <- gap_df %>% 
  group_by(continent) %>% 
  summarise(max_GDP = max(gdpPercap), min_GDP = min(gdpPercap))

#using kable
min_max_GDP_cont %>% kable(format = 'markdown')
```



|continent |   max_GDP|    min_GDP|
|:---------|---------:|----------:|
|Africa    |  21951.21|   241.1659|
|Americas  |  42951.65|  1201.6372|
|Asia      | 113523.13|   331.0000|
|Europe    |  49357.19|   973.5332|
|Oceania   |  34435.37| 10039.5956|

```r
#reporting using a ggplot
(ggplot(min_max_GDP_cont) + 
  geom_linerange(aes(x=continent, ymin=min_GDP, ymax=max_GDP, width = 1.0, color=continent)) + 
  xlab("Continents") + ylab("GDP Per Capita")) + ggtitle("GDP Per Capita across Continents")
```




---


***Task 2: GDP spread within the continents***

To analyze the spread of the GDP within the continents, I used the following:

- minimum GDP
- maximum GDP
- mean of GDP
- median of GDP
- standard deviation of GDP



```r
#Making the beautiful kable-table
gap_df %>% 
  group_by(continent) %>% 
  summarise(gdpPercap_min = min(gdpPercap),
            gdpPercap_max = max(gdpPercap),
            gdpPercap_mean = mean(gdpPercap),
            gdpPercap_median = median(gdpPercap),
            gdpPercap_sd = sd(gdpPercap)) %>% 
  kable(format='markdown')
```



|continent | gdpPercap_min| gdpPercap_max| gdpPercap_mean| gdpPercap_median| gdpPercap_sd|
|:---------|-------------:|-------------:|--------------:|----------------:|------------:|
|Africa    |      241.1659|      21951.21|       2193.755|         1192.138|     2827.930|
|Americas  |     1201.6372|      42951.65|       7136.110|         5465.510|     6396.764|
|Asia      |      331.0000|     113523.13|       7902.150|         2646.787|    14045.373|
|Europe    |      973.5332|      49357.19|      14469.476|        12081.749|     9355.213|
|Oceania   |    10039.5956|      34435.37|      18621.609|        17983.304|     6358.983|

```r
#Plotting this spread
(ggplot(gap_df) + aes(x=continent, y=gdpPercap, color=continent) + geom_boxplot(alpha=0.5) + ggtitle("GDP Spread across continents"))
```



```r
#just use this one
(ggplot(gap_df) + aes(x=log10(gdpPercap), fill=continent) +
  geom_density(alpha=0.5) + 
  xlab("log measure of GDP Per Capita") + 
  ylab("Density") + 
  ggtitle("GDP Per capita Density Plot"))
```



---


***Task 3: Vanilla task***

It was unclear to me, at the first place, what *Vanilla Mean* meant. 

Was enlightened by this particular [article.](http://www.mymarketresearchmethods.com/descriptive-inferential-statistics-difference/)




```r
#trimmed mean
mean_trimmed <- gap_df %>% 
  group_by(year) %>% 
  select(year,lifeExp,continent,gdpPercap) %>% 
  summarise(regular_mean= mean(lifeExp),
            trimmed_mean_lifeExp= mean(lifeExp, trim = 0.30),
            weighted_mean_lifeExp_cont= weighted.mean(lifeExp,continent),
            weighted_mean_lifeExp_gdpPercap= weighted.mean(lifeExp,gdpPercap))
            

mean_trimmed %>% kable(format='markdown')
```



| year| regular_mean| trimmed_mean_lifeExp| weighted_mean_lifeExp_cont| weighted_mean_lifeExp_gdpPercap|
|----:|------------:|--------------------:|--------------------------:|-------------------------------:|
| 1952|     49.05762|             46.83114|                   53.49164|                        57.50267|
| 1957|     51.50740|             49.85769|                   55.99311|                        59.97587|
| 1962|     53.60925|             52.40092|                   58.04491|                        62.05187|
| 1967|     55.67829|             55.15267|                   60.03400|                        63.92627|
| 1972|     57.64739|             57.87755|                   61.85470|                        65.79301|
| 1977|     59.57016|             60.41925|                   63.62624|                        67.47203|
| 1982|     61.53320|             62.81936|                   65.47769|                        69.48395|
| 1987|     63.21261|             65.00983|                   67.03623|                        71.45846|
| 1992|     64.16034|             66.50200|                   68.14683|                        72.85761|
| 1997|     65.01468|             67.69257|                   69.24829|                        74.05185|
| 2002|     65.69492|             68.90531|                   70.24060|                        75.04541|
| 2007|     67.00742|             70.27053|                   71.47271|                        75.96507|

```r
#library(reshape2), here I first used the melt function, to keep things easy
#melt
data_melt <- melt(mean_trimmed, id="year")

#plot trimmed and weighted mean
ggplot(data=data_melt, 
       aes(x=year, y=value, color=variable)) +
       geom_line()+
       geom_point()+
       ggtitle("Weighted and Trimmed Means variability")+
       xlab("Year")+
       ylab("Life Expectancy in years")
```




This was pretty interesting, how the trimmed and weighted means would yield different meanings altogether. In this case, these were some of my inferences:

- The regular mean of *Life Expectancy*, which we could easily calculate, might not provide sufficient information, to end up with a conclusion.
- However, if we have weighted the mean with either the continent, or the GDP, we get very different results.
- It does depend on what continent we live, and how much not us, but everybody earns; that determines our life expectancy. INTERESTING!



---


***Task 4: How is life expectancy changing over time in continents***

I used a weighted mean, to determine how the longitivity varies across continents, over time. 

I weighted the mean using:

- Population
- GDP Per capita



```r
cont_lifeExp <- gap_df %>% 
  group_by(year,continent) %>% 
  select(year, lifeExp,continent, pop, gdpPercap) %>% 
  summarise(w_mean= weighted.mean(lifeExp,continent,pop))


#plotting
ggplot(cont_lifeExp, 
       aes(x=year, y=w_mean, color=continent)) +
       geom_line()+
       geom_point()+
       xlab("Years")+
       ylab("Weighted Mean Life Expectancy")+
       ggtitle("Life Expectancy variation over time across continents")
```




---

The following can be inferred:

- Europe, has been consistently a dwelling continent with higher longetivity. Nothing has changed, and its on a steady rise.
- Africa, was progressing well on the life expectancy front, but was slowed down by various factors after 1990s, a disturbing scenario. Reading [this](http://econsguide.blogspot.ca/2009/02/possible-reasons-for-uks-deficit-in-its.html) exposed the factors.

---



***Task 5: Relative abundance of countries with low life expectancy over time by continent***




```r
#Finding the highest and lowest lifeExp
min_max_lifeExp <- gap_df %>% 
  group_by(year) %>% 
  select(year,lifeExp,country) %>% 
  summarise(min_lifeExp= min(lifeExp),
            max_lifeExp= max(lifeExp))

min_max_lifeExp %>% kable(format='markdown')
```



| year| min_lifeExp| max_lifeExp|
|----:|-----------:|-----------:|
| 1952|      28.801|      72.670|
| 1957|      30.332|      73.470|
| 1962|      31.997|      73.680|
| 1967|      34.020|      74.160|
| 1972|      35.400|      74.720|
| 1977|      31.220|      76.110|
| 1982|      38.445|      77.110|
| 1987|      39.906|      78.670|
| 1992|      23.599|      79.360|
| 1997|      36.087|      80.690|
| 2002|      39.193|      82.000|
| 2007|      39.613|      82.603|

```r
#melting them
min_max_lifeExp_melt <- melt(min_max_lifeExp, id="year")

#plotting them
ggplot(min_max_lifeExp_melt, aes(x=year, y=value, color=variable))+
  geom_line()+
  geom_point()+
  xlab("Years")+
  ylab("Life Expectancy in years")+
  ggtitle("Difference between maximum and minimum Life Expectancy")
```



Incredible difference, between the minimum and maximum lifeExp values!

Let's explore it more

---



***Task 6: Interesting stories,***

I have performed these 2 analyses:

1. Life Expectancy difference between max and min.
2. Life Expectancy in North American countries.




```r
#just viewing Rwanda, compared to Japan what happened to the lifeExp there
#found out using the subset below

maxm <- subset(gap_df, gap_df$lifeExp==max(gap_df$lifeExp))
maxm %>% glimpse()
```

```
## Observations: 1
## Variables: 6
## $ country   (fctr) Japan
## $ continent (fctr) Asia
## $ year      (dbl) 2007
## $ lifeExp   (dbl) 82.603
## $ pop       (dbl) 127467972
## $ gdpPercap (dbl) 31656.07
```

```r
#list created with the country names, which I want to subset
rw_jpn <- c('Rwanda','Japan')

#subset
rwanda_japan <- subset(gap_df, gap_df$country==rw_jpn)

#printing as a kable table
kable(rwanda_japan, format = 'markdown')
```



|country |continent | year| lifeExp|       pop|  gdpPercap|
|:-------|:---------|----:|-------:|---------:|----------:|
|Japan   |Asia      | 1957|  65.500|  91563009|  4317.6944|
|Japan   |Asia      | 1967|  71.430| 100825279|  9847.7886|
|Japan   |Asia      | 1977|  75.380| 113872473| 16610.3770|
|Japan   |Asia      | 1987|  78.670| 122091325| 22375.9419|
|Japan   |Asia      | 1997|  80.690| 125956499| 28816.5850|
|Japan   |Asia      | 2007|  82.603| 127467972| 31656.0681|
|Rwanda  |Africa    | 1952|  40.000|   2534927|   493.3239|
|Rwanda  |Africa    | 1962|  43.000|   3051242|   597.4731|
|Rwanda  |Africa    | 1972|  44.600|   3992121|   590.5807|
|Rwanda  |Africa    | 1982|  46.218|   5507565|   881.5706|
|Rwanda  |Africa    | 1992|  23.599|   7290203|   737.0686|
|Rwanda  |Africa    | 2002|  43.413|   7852401|   785.6538|

```r
#ggplot
ggplot(rwanda_japan, aes(x=year, y=lifeExp, color=country))+
  geom_line()+ 
  geom_point()+
  xlab("Years")+
  ylab("Life Expectancy in years")+
  ggtitle("Rwanda V/S Japan Life Expectancy")
```



```r
#how the gdp looks between these two nations
ggplot(rwanda_japan, aes(x=year, y=gdpPercap, color=country))+ 
  geom_line()+ 
  geom_point()+
  xlab("Years")+
  ylab("GDP Per Capita")+
  ggtitle("Rwanda V/S Japan GDP Per capita")
```



*Japan* has been on a steady rise, building up on its GDP ever since. 

However, *Rwanda* has been struggling with difficult circumstances, which has prevented the nation to progress, whether in terms of GDP or increasing the life expectancy.


I remember watching the movie [Hotel Rwanda](https://en.wikipedia.org/wiki/Hotel_Rwanda) 7 years back; based on the theme of avenging clashes between Hutu and Tutsi tribes.


---



***North American Countries***

A comparison between the 3 North American countries: Canada, Mexico, and USA



```r
#comparing the north american countries
list_northam <- c('United States','Mexico', 'Canada')
northam <- subset(gap_df, gap_df$country==list_northam)

#kable
kable(northam, format('markdown'))
```



|country       |continent | year| lifeExp|       pop| gdpPercap|
|:-------------|:---------|----:|-------:|---------:|---------:|
|Canada        |Americas  | 1962|  71.300|  18985849| 13462.486|
|Canada        |Americas  | 1977|  74.210|  23796400| 22090.883|
|Canada        |Americas  | 1992|  77.950|  28523502| 26342.884|
|Canada        |Americas  | 2007|  80.653|  33390141| 36319.235|
|Mexico        |Americas  | 1957|  55.190|  35015548|  4131.547|
|Mexico        |Americas  | 1972|  62.361|  55984294|  6809.407|
|Mexico        |Americas  | 1987|  69.498|  80122492|  8688.156|
|Mexico        |Americas  | 2002|  74.902| 102479927| 10742.441|
|United States |Americas  | 1952|  68.440| 157553000| 13990.482|
|United States |Americas  | 1967|  70.760| 198712000| 19530.366|
|United States |Americas  | 1982|  74.650| 232187835| 25009.559|
|United States |Americas  | 1997|  76.810| 272911760| 35767.433|

```r
#plot 
ggplot(northam, aes(x=year, y=lifeExp, color=country))+
  geom_line()+
  geom_point()+
  xlab("Years")+
  ylab("Life Expectancy in years")+
  ggtitle("Life Expectancy in North American countries")
```



People live longer in Canda, compared to USA, and Mexico! 

---


***Report***

This was an interesting homework. I feel confident about plotting, using ggplot mainly.
It was the first time, I experimented with piping, and to see it work was fabulous. 

***Done***

---