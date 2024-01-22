ggplot(data = mpg) +
  geom_boxplot(
    aes(fl, cty),
    fill = "steelblue",
    alpha = 0.5
  ) + 
  geom_jitter(
    aes(fl, cty),
    alpha = 0.5,
    colour = "steelblue"
  )+
  theme_bw()+
  labs(
    x = "Fuel Type",
    y = "City Miles (per gallon)",
    title = "City Miles (per gallon) of Cars by Fuel Type"
  ) # labels

ggplot(data = mpg)+ #specifying data
  geom_histogram(
    aes(x = displ)
  ) #geom and aesthetic


#DIY 1

yrbss_samp
ggplot(data = yrbss_samp)+ #specifying data
  geom_histogram(
    aes(x = height),fill="light green"
  ) #geom and aesthetic
ggplot(data=yrbss_samp)+
  geom_density(
    aes(x=weight)
  )
ggplot(data=yrbss_samp)+
  geom_density(
    aes(x=age)
  )


ggplot(data = mpg)+
  geom_bar(
    aes(x = drv)
  )  
#DIY 2
ggplot(data=yrbss_samp)+
  geom_bar(
    aes(x=gender)
  )
ggplot(data=yrbss_samp)+
  geom_bar(
    aes(x=hispanic), fill="light green"
  )
ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight )
  ) #adding  a layer of points

yrbss_samp %>%
  group_by(gender) %>%
  summarise(
    mean = mean(strength_training_7d)
  ) %>% # data wrangling
  ggplot() +
  geom_col(
    aes(gender,mean)
  )+
  theme_bw()+
  labs(
    title= "Difference between the mean days that males and females do strength training"
  )
) #adding a column

yrbss_samp%>%
  filter(gender=="males")%>%
  ggplot()+
  geom_col(
    aes(hispanic,height)
  )

yrbss_samp%>%
  group_by(gender="male")%>%
  summarise(mean height=mean(height))%>%
  ggplot()+
  geom_col(
    aes(hispanic,mean height)
  )

yrbss_samp %>%
  group_by(gender="male") %>%
  group_by(hispanic) %>%
  summarise(
    mean_hh = mean(height)
  ) %>% # data wrangling
  ggplot()+geom_col(aes(hispanic, mean_hh)) #adding a column

#DIY 4

ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight,
        colour = height)
  ) #adding  a layer of points

ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight,
        colour = gender)
  ) #adding  a layer of points


ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight,
        shape = gender)
  ) #adding  a layer of points


ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight,
        shape = gender),
    alpha = 0.5)
) #adding  a layer of points

#DIY 5
yrbss_samp|>
  mutate(
    strength_training_7d_category = case_when(
      strength_training_7d == 0 ~ "No Training",
      strength_training_7d >=1 &
        strength_training_7d <3 ~ "Low Training",
      strength_training_7d >=3 &
        strength_training_7d <5 ~ "Moderate Training",
      strength_training_7d >=5 ~ "High Training",+
        group_by(strength_training_7d) %>%
        summarise(
          mean_hh = mean(weight)
        )+ # data wrangling
        ggplot()+geom_bar(aes(mean_hh)) #adding a column)
    ))



str1<-yrbss_samp|>mutate(str_category=case_when(strength_training_7d==0~"No Training",strength_training_7d>=1&strength_training_7d<=3~"Low Training",strength_training_7d>3&strength_training_7d<=5~"Moderate Training",strength_training_7d>5~"High Training"))
select(str1,strength_training_7d,str_category)
#Bar Graph
str1 %>%
  group_by(str_category) %>%
  summarise(
    mean_st = mean(strength_training_7d)
  ) %>% # data wrangling
  ggplot()+geom_col(aes(str_category, mean_st))
#Reordering
str1 %>%
  group_by(str_category) %>%
  summarise(
    mean_st = mean(strength_training_7d)
  ) %>%
  ggplot()+geom_col(aes(x=reorder(str_category, mean_st),y=mean_st),width=0.5)
#Flipped the axis
str1 %>%
  group_by(str_category) %>%
  summarise(
    mean_st = mean(strength_training_7d)
  ) %>%
  ggplot()+geom_col(aes(x=reorder(str_category, mean_st),y=mean_st),width=0.8)+coord_flip()
#Formatting
str1 %>%
  group_by(str_category) %>%
  summarise(
    mean_st = mean(strength_training_7d)
  ) %>%
  ggplot()+geom_col(aes(x=reorder(str_category, mean_st),y=mean_st),width=0.8)+coord_flip()+labs(title="Strength Training Heirarchy",subtitle="Mean Strength vs Strength Category",
                                                                                                 x="Strenth Category",y="Mean Strength",caption="Data Source:YRBSS;Analysis by Sarthak Virmani")
#trying
