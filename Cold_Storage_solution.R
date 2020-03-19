
# Assignment 2 - Thangamani R
setwd("C:\\Users\\thang\\OneDrive\\DSBA\\Week7")
cold_storage_temp=read.csv("Cold_Storage_Temp_Data.csv");
summary(cold_storage_temp)

#print(subset(cold_storage_temp,Season=='Rainy'))
rainy_ds=subset(cold_storage_temp,Season=='Rainy')
summary(rainy_ds)
mean(rainy_ds[,'Temperature'])
#1.Mean Temp for rainy season
print(mean(subset(cold_storage_temp,Season=='Rainy')[,'Temperature']))

#2.Mean Temp for Winter season
print(mean(subset(cold_storage_temp,Season=='Winter')[,'Temperature']))

#3.Mean Temp for Summer season
print(mean(subset(cold_storage_temp,Season=='Summer')[,'Temperature']))

#4.Mean Temp for entire year
print(mean(cold_storage_temp[,'Temperature']))

#5.Standard deviation of Temp for whole year
print(sd(cold_storage_temp[,'Temperature']))

?pnorm
#6.Probability of temp being below 2 Degrees
pnorm(2,3.002466,0.4658319,lower.tail=T)

#7. Probability of temp being greater than 4 degress
1-pnorm(4,3.002466,0.4658319,lower.tail=T)

pnorm(4,3.002466,0.4658319,lower.tail=F)


0.01612076+0.01569904

require(ggplot2)
ggplot(cold_storage_temp,aes(x=Season,y=Temperature))+
  geom_boxplot()+scale_x_discrete()

sum_data=aov(Temperature~Season,data=cold_storage_temp)
summary(sum_data)

#P is lower than 0.05 then reject null hypothesis 
#status quo is temp are maintained, but this is rejected and the conclusion 
#is that temperaturs vary and are not between 2-4 always
tukey_result=TukeyHSD(sum_data)
tukey_result


#Part 2

#One tailed
# No standard deviation is known, so t-test
#alpha=0.1=> Confidence level=0.9
#mean =3.9
cold_storage_march=read.csv("Cold_Storage_Mar2018.csv");
summary(cold_storage_march)

t.test(cold_storage_march$Temperature,conf.level = 0.9,alternative = 'greater'
       ,paired=FALSE)
  