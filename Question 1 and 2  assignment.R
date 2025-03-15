library(tidyselect)
library(tibble)
library(tidyverse)
library(tidyverse)

View(graduate_survey_1_)
graduate_survey_1_ %>% select(Age)
# Select specific columns
data <- graduate_survey_1_ %>% select(
  Branch,
  Campus,
  EduLevel,
  ProgLang,
  Databases,
  Platform,
  WebFramework,
  Employment,
  AISearch,
  AITool,
  Industry,
  StudyField,
  Role
)
View(data)
#drop missing values justified in word
data <- na.omit(data)
#dis <-data %>% distinct(Campus)
# standardize categorical data
data <- data %>% mutate(
  Campus = case_when(
    Campus == 'Umhlanga Campus' ~ 'Durban Campus',
    Campus == 'Nelspruit Campus' ~ 'Mbombela Campus',
    TRUE ~ Campus
  )
)
data %>% count(Campus, sort = TRUE) %>% slice(1:3)
# check for the highest responses and visualize the top responses
View(data)

View(top_response)
top_response <- filter(data,
                       Campus == 'Durban Campus' |
                         Campus == 'Pretoria Campus' | Campus == 'Mbombela Campus')
View(top_response)
#Question 2
top_tools <- top_response %>% select(ProgLang, Databases, WebFramework, Platform, AITool, AISearch)

long_data <- top_tools %>%
  pivot_longer(
    cols = c(ProgLang, Databases, Platform, WebFramework, AISearch, AITool),
    names_to = "ToolType",
    values_to = "Tool"
  ) %>%
  separate_rows(Tool, sep = ";")

top_response %>% count(ProgLang, sort = TRUE)
freq_table <- top_response %>% pull(WebFramework) %>% str_split(pattern = ';') %>%
  unlist(list) %>% table()
freq_table
freq_table <- sort(freq_table, decreasing = TRUE)
top_8 <- head(freq_table, 8)
barplot(top_8, las = 2) + title(main = 'Top WebFrameWorks')

# Top DataBases

top_response %>% pull(Databases) %>% str_split(pattern = ';') %>% unlist(list)
top_response_clean <- top_response %>% mutate(across(
  c(ProgLang, Platform, WebFramework),
  ~ str_split(., pattern = ';')
))
top_response %>%
  mutate(across(ProgLang:Platform, ~ str_split(., pattern = ";")))
# Top Web Frame Works
freq_table <- top_response %>% pull(Databases) %>% str_split(pattern = ';') %>%
  unlist(list) %>% table()

#freq_table <-top_response %>% pull(Databases) %>% str_split(pattern = ';')%>%unlist(list)%>%table()
freq_table
freq_table <- sort(freq_table, decreasing = TRUE)
top_8 <- head(freq_table, 8)
barplot(top_8, las = 2) + title(main = 'Top Databases')

freq_table <- sort(freq_table, decreasing = TRUE)
freq_table
top_4 <- head(freq_table, 10)
barplot(top_4, las = 2) + title(main = 'Top Programming Language')
# using facet grid

# table(top_tools)
# top_response_clean <- mutate(top_tools,across(everything(),str_split(pattern=';')))%>%unlist()
# View(top_response_clean)
# pivot_longer(top_,cols=everything(),names_to = 'category',values_to = 'name')

cleaned <- top_tools %>% separate_rows(AISearch, sep = ";")#%>%separate_rows(AITool, sep = ";")%>%separate_rows(WebFramework, sep = ";")%>%separate_rows(Databases, sep = ";")%>%separate_rows(ProgLang, sep = ";")%>%separate_rows(Platform, sep = ";")
View(top_tools)
library(tidyverse)

# all at once

View(long_data)
long_data %>% group_by(ToolType, Tool) %>% summarise(n = n(), .groups =
                                                       'drop') %>% group_by(ToolType) %>% slice_max(order_by = n, n = 10) %>%
  ggplot(aes(
    x = reorder(Tool, n),
    y = n,
    fill = Tool
  )) + geom_col(show.legend = FALSE) + coord_flip() + facet_wrap( ~ ToolType, nrow = 3, scales =
                                                                    'free_y')
# top industries
graduates <- top_response %>% filter(str_detect(EduLevel, "^Bachelor|^Master|^Professional"))
View(graduates)
freq_table <- graduates %>% pull(Industry) %>% str_split(pattern = ';') %>%
  unlist(list) %>% table()


freq_table
freq_table <- sort(freq_table, decreasing = TRUE)
top_10 <- head(freq_table, 10)
industries <- data.frame(freq_table)
#
graduates <- top_response %>% filter(str_detect(EduLevel, "^Bachelor|^Master|^Professional"))

freq_table <- graduates %>% pull(Industry) %>% str_split(pattern = ';') %>%
  unlist(list) %>% table()


#freq_table
freq_table <- sort(freq_table, decreasing = TRUE)
top_10 <- head(freq_table, 10)
industries <- data.frame(freq_table)


industries %>% ggplot(aes(x = ., y = Freq, fill = .)) + geom_bar(stat = 'identity') +
  labs(title = 'Top Industries') +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank()
  )


#Roles
graduates <- top_response %>% filter(str_detect(EduLevel, "^Bachelor|^Master|^Professional"))
View(graduates)
freq_table <- graduates %>% pull(Role) %>% str_split(pattern = ';') %>% unlist(list) %>%
  table()


freq_table
freq_table <- sort(freq_table, decreasing = TRUE)
top_10 <- head(freq_table, 10)
industries <- data.frame(top_10)
View(top_10)

industries %>% ggplot(aes(x = ., y = Freq, fill = .)) + geom_bar(stat = 'Identity') +
  ggtitle('Top Roles') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


top_response %>% distinct(EduLevel)

#checking unemployed people
unemployed <- data %>% filter(str_detect(data$Employment, 'Not em'))
unemployed_col <- unemployed$Employment
prop.table(table(data$Employment)) * 100
cleaned_data <- data %>% mutate(Employment = case_when(
  str_detect(Employment, regex('^Not em', ignore_case = TRUE)) ~ 'Unemployed',
  str_detect(Employment, regex('^Employed | ^Developer', ignore_case = TRUE)) ~ 'Employed',
  TRUE ~ 'Employed'
  
))

unemployed_col <- unemployed$Employment
categories <- c('Unemployed', 'Employed')

table <- prop.table(table(cleaned_data$Employment)) * 100
count_table <- table(cleaned_data$Employment)
count_percentage <- round((count_table / sum(count_table)) * 100, 1)

labels <- paste(names(count_table), "(", count_percentage, "%)", sep = "")
pie_plot <- pie(
  count_table,
  labels = labels,
  col = rainbow(length(count_table)),
  main = 'Unemployment distribution'
)
employment <- data.frame(table)
employment %>% ggplot(aes(x = Var1, y = Freq)) + geom_col(stat = 'identity') +
  coord_polar(theta = 'y') + theme_void()
cleand_data$Employment * 100

# Web App
