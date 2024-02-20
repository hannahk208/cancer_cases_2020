library(readxl)
library(dplyr)
library(ggplot2)
library (patchwork)

cancer_incidence_men <- read.csv('Global Cancer Incidence (men).csv')
cancer_incidence_women <- read.csv('Global Cancer Incidence (women).csv')


## some cleaning
names(cancer_incidence_men) <- c('rank','cancer','new_cases_2020','percent_all_cases')# rename columns
cancer_incidence_men <- cancer_incidence_men[-1,] #exclude the first row.
cancer_incidence_men$gender <- 'men' #adds a new column named gender,The values in the gender column are set to the string 'men'.
#This line effectively assigns the gender 'men' to all rows in the data frame.

names(cancer_incidence_women) <- c('rank','cancer','new_cases_2020','percent_all_cases')
cancer_incidence_women <- cancer_incidence_women[-1,]
cancer_incidence_women$gender <- 'women'

# combine two data frames by stacking them vertically, i.e., by appending one below the other
both_sex <- rbind(cancer_incidence_men, cancer_incidence_women)

## Summary stats

total_cases_gender <- both_sex%>%
  group_by(gender)%>%
  summarise(new_cases_2020 = sum(new_cases_2020))

total_cases_cancer <- both_sex%>%
  group_by(cancer)%>%
  summarise(new_cases_2020 = sum(new_cases_2020))

## plotting gender cases


# Plotting gender cases with specified colors
ggplot(total_cases_gender) +
  geom_bar(aes(x = gender, y = new_cases_2020, fill = gender), stat = 'identity') +
  scale_fill_manual(values = c("men" = "blue", "women" = "pink")) +  # Specify colors
  ylab('Number of cases in 2020')

## plotting cancer types and the cases in descending order
ggplot(total_cases_cancer)+
  geom_bar(aes(reorder(cancer, desc(new_cases_2020)),y =new_cases_2020, fill = cancer ), stat = 'identity')+
  ylab('number of cases in 2020')+ xlab('Cancer Types')+ theme(legend.position = 'none')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

cancer_types <-  (total_cases_cancer)


## Summary stats in Women
total_cases_cancer_women <- both_sex %>%
  filter(gender == 'women') %>%
  group_by(cancer) %>%
  summarise(new_cases_2020 = sum(new_cases_2020))

## Plotting cancer cases in women in descending order
ggplot(total_cases_cancer_women) +
  geom_bar(
    aes(reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer),
    stat = 'identity'
  ) +
  ylab('Number of Cases in 2020') +
  xlab('Cancer Types in Women') +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))





## Summary stats in Men
total_cases_cancer_men <- both_sex %>%
  filter(gender == 'men') %>%
  group_by(cancer) %>%
  summarise(new_cases_2020 = sum(new_cases_2020))

## Plotting cancer cases in men in descending order
ggplot(total_cases_cancer_men) +
  geom_bar(
    aes(reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer),
    stat = 'identity'
  ) +
  ylab('Number of Cases in 2020') +
  xlab('Cancer Types in Men') +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



## Plot unique cancer types for men
unique_cancer_men <- setdiff(total_cases_cancer_men$cancer, total_cases_cancer_women$cancer)
ggplot(filter(both_sex, gender == 'men' & cancer %in% unique_cancer_men)) +
  geom_bar(
    aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer),
    stat = 'identity'
  ) +
  ylab('Number of Cases in 2020') +
  xlab('Unique Cancer Types in Men') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(legend.position = 'none')  # Remove the legend to avoid confusion

## Plot unique cancer types for women
unique_cancer_women <- setdiff(total_cases_cancer_women$cancer, total_cases_cancer_men$cancer)
ggplot(filter(both_sex, gender == 'women' & cancer %in% unique_cancer_women)) +
  geom_bar(
    aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer),
    stat = 'identity'
  ) + 
  ylab('Number of Cases in 2020') +
  xlab('Unique Cancer Types in Women') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))







## Find the five most prevalent cancer types for each gender
most_prevalent_cancer_men <- both_sex %>%
  filter(gender == 'men') %>%
  group_by(cancer) %>%
  summarise(total_cases = sum(new_cases_2020)) %>%
  top_n(5, total_cases) %>%
  distinct()

most_prevalent_cancer_women <- both_sex %>%
  filter(gender == 'women') %>%
  group_by(cancer) %>%
  summarise(total_cases = sum(new_cases_2020)) %>%
  top_n(5, total_cases) %>%
  distinct()

# Bar plot for men
plot_men <- ggplot() +
  geom_bar(
    data = filter(both_sex, gender == 'men' & cancer %in% most_prevalent_cancer_men$cancer),
    aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020),
    stat = 'identity', position = 'identity', fill="blue"
  ) +
  ylab('Number of Cases in 2020') +
  xlab('Most Prevalent Cancer Types for Men') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Bar plot for women
plot_women <- ggplot() +
  geom_bar(
    data = filter(both_sex, gender == 'women' & cancer %in% most_prevalent_cancer_women$cancer),
    aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020),
    stat = 'identity', position = 'identity', fill="pink"
  ) +
  ylab('Number of Cases in 2020') +
  xlab('Most Prevalent Cancer Types for Women') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Arrange the two plots side by side
plot_men / plot_women