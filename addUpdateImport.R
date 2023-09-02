library(activityinfo)
library(tidyverse)
# Installing activityinfo package
devtools::install_github("bedatadriven/activityinfo-R")
library(activityinfo)
activityInfoToken()
# adding a record in activityinfo

# Extracting records from ActivityInfo using the API

Practise <- queryTable("cvl07wrlkuwpqmz5n",
                       "FirstName" = "c77h6iblkuwpwuv5o",
                       "Married" = "ciu9dwslkuwq8xv5u",
                       "Location" = "cnrg62dlkuwqyl45w",
                       "Schools School Name" = "cfmjon5lkuy2ab32.cvygcdjlgxnxsh311",
                       "schools" = "schools")

# To view existing databases in ActivityInfo

# creating a new database

addDatabase("Kindoli")
getDatabases()

# Retrieving records from ActivityInfo using the getrecords() function

nuts <- getRecords("c4zvf8gll3g32b3q", style = minimalColumnStyle())|>
  collect()



#Adding a form to an existing database

fmSchema <- formSchema("cfxbif1ll28higt1", label="A demostration of R")|>
  addFormField(textFieldSchema(label="What is your name", code = "name", key = TRUE, 
                               required = TRUE))|>
  addForm()


eddie <- formSchema("cohcj69lk9qigic2", label = "adding Forms in R")|>
  addFormField(textFieldSchema("How old are you", code = "age", key = TRUE, required = T))|>
  addFormField(multipleSelectFieldSchema("Why do you want to learn R",
                                         options = c("for data visulaization",
                                                     "To import Records","Manipulate forms","Download data"), required = T))|>
  addFormField(singleSelectFieldSchema("Are you married",options = c("yes","No")))|>
  addFormField(textFieldSchema("Where do you live",required = T))|>
  addForm()

# Importing data from an external file into activityinfo


Test<- readxl::read_xlsx("D:/LWF/ActivityInfo training & Webinars/Test.xlsx")
#Test$id <- 1: nrow(Test)
#$Test$id

# Creating a form from scratch and adding records to the form.
import_test <- formSchema("cfxbif1ll28higt1", label="Adding a record")|>
  addFormField(textFieldSchema(label="FirstName", code = "name", key = TRUE, 
                               required = TRUE))|>
  addFormField(singleSelectFieldSchema("Married", options = c("yes", "no")))|>
  addFormField(textFieldSchema("Location")) %>% 
  addForm()

importRecords(formId = "ce3oftellb7up4jr", data = Test)


newDb <- addDatabase("A new ActivityInfo tutorial database!")
dlist <- getDatabases()




dbTree <- getDatabaseTree(databaseId = newDb$databaseId)

head(dbTree)

benefeciary_form_id <- "ce3oftellb7up4jr"
addRecord(formId = benefeciary_form_id, parentRecordId = NA_character_,
          list(name="Eddie", 
          Married="yes", 
          Location="Kampala"))

# Updating records in ActivityInfo

updateRecord(formId = benefeciary_form_id, recordId = "cxdmif0llb7yg4ee5e", 
             list(name="Alex"))


# Creating a look up function

SchoolsLookup<-schools$id




# Trial exercise for importing data from surveyCTO
ed_fg<-openxlsx::read.xlsx("KPSEA.xlsx")
glimpse(ed_fg)

ex_sch<-createFormSchemaFromData(x=ed_fg,
                         databaseId = "cfxbif1ll28higt1",
                         label = "KPSEA") %>% 
  addForm()

# viewing the id and other metadata of the newly created form
ex_sch$id
ex_sch$label

# Importing records from an external source such as xlsx file into activityinfo using R

importRecords(formId = "cdeldvdllb8dwt611", data = ed_fg)

#Deleting forms from a database
deleteForm("cfxbif1ll28higt1", formId = "cdeldvdllb8dwt611")
deleteForm("cfxbif1ll28higt1", formId = "ce3oftellb7up4jr")



# Adding a new form
ed<-formSchema("cfxbif1ll28higt1", label = "Marriage survey") %>% 
  addFormField(textFieldSchema("Where do you stay", code="stay",key=TRUE, required = TRUE)) %>% 
  addFormField(multipleSelectFieldSchema("What is your performance leve", code="Performance",
                                         options = c("Excellent","Poor","Average"), required = TRUE)) %>% 
  addFormField(multilineFieldSchema("Write some poems", code="poems",required = TRUE)) %>% 
  addFormField(quantityFieldSchema("How old are you",code="age",required = TRUE)) %>% 
  addForm()

# Adding a form using the createformfromdata
tz<- openxlsx::read.xlsx("KPSEA.xlsx")

# Exploring the tz data frame
glimpse(tz)

# transforming the character variables into factors for downstream analysis and exportation
tz$School.Name <- factor(tz$School.Name)
tz$Performance.Level <- factor(tz$Performance.Level)
tz$`Sub-County` <- factor(tz$`Sub-County`)
glimpse(tz)
# renaming the variable names of dataset tz
test<-createFormSchemaFromData(x=tz,
                               databaseId = "cfxbif1ll28higt1", 
                               label = "Kenya Performance") %>% 
  addForm()


test$id
test$label

#Importing records from an external source
importRecords(formId = "cq3wsnlll2946vbw", data = tz)


# creating schema from existing data
pen<- palmerpenguins::penguins %>% 
  glimpse()

peng_sch <- createFormSchemaFromData(x=pen,
                                     databaseId = "cfxbif1ll28higt1",
                                     label = "Penguins_data",
                                     requiredColumns = "species",
                                     upload = TRUE)

# Getting records from a form in ActivityInfo

lk<- getRecords("c1iqjewlj75toug3", style = prettyColumnStyle()) %>% 
  collect()

# Creating a schema from existing data into ActivityInfo

KPS <- createFormSchemaFromData(x=tz,
                                databaseId ="cfxbif1ll28higt1",
                                label = "Kenya Performance School Assessment",
                                requiredColumns = "Performance.Level",
                                upload = TRUE)

# Importing data from surveyCTO to R then to ActivityInfo
#Load the packages for surveyCTO and the data.table package

library(rsurveycto)
library(data.table)

# Using SurveyCTO server name, username and password to login to surveycto
# I created a text file that I imported directly to R
auth <- scto_auth("SurveyCTO_username_password.txt")

# I now read a dataset from surveycto to R
Kenya_impairement <- scto_read(auth, "visual_imparement")

# Cleaning the Kenya Impairment dataset for importation in ActivityInfo, 
#removing the unwanted variables

names(Kenya_impairement)

Kenya_impairement_cleaned <- Kenya_impairement[ ,-c("CompletionDate", "SubmissionDate", "starttime", "endtime",
                                                   "deviceid", "username","device_info","duration",
                                               "instanceID","formdef_version", "review_status","KEY")]

#Identifying variable in the Kenya_impairement-dataset that are characters and need to be converted
# to factors for downstream upload in ActivityInfo
glimpse(Kenya_impairement_cleaned)
names(Kenya_impairement_cleaned)

Kenya_impairement_cleaned$schoollevel <- factor(Kenya_impairement_cleaned$schoollevel) 
Kenya_impairement_cleaned$grade <- factor(Kenya_impairement_cleaned$grade)
Kenya_impairement_cleaned$preschname <- factor(Kenya_impairement_cleaned$preschname)
Kenya_impairement_cleaned$sex <- factor(Kenya_impairement_cleaned$sex)
Kenya_impairement_cleaned$age <- factor(Kenya_impairement_cleaned$age)
Kenya_impairement_cleaned$schlists <- factor(Kenya_impairement_cleaned$schlists)
Kenya_impairement_cleaned$pgrade_1 <- factor(Kenya_impairement_cleaned$pgrade_1)
Kenya_impairement_cleaned$page_1 <- factor(Kenya_impairement_cleaned$page_1)
Kenya_impairement_cleaned$SEX_1 <- factor(Kenya_impairement_cleaned$SEX_1)
Kenya_impairement_cleaned$number1_1 <- factor(Kenya_impairement_cleaned$number1_1)
Kenya_impairement_cleaned$page_2 <- factor(Kenya_impairement_cleaned$page_2)
Kenya_impairement_cleaned$pgrade_2 <- factor(Kenya_impairement_cleaned$pgrade_2)
Kenya_impairement_cleaned$page_2 <- factor(Kenya_impairement_cleaned$page_2)
Kenya_impairement_cleaned$SEX_2 <- factor(Kenya_impairement_cleaned$SEX_2)
Kenya_impairement_cleaned$number1_2 <- factor(Kenya_impairement_cleaned$number1_2)
Kenya_impairement_cleaned$pgrade_3 <- factor(Kenya_impairement_cleaned$pgrade_3)
Kenya_impairement_cleaned$page_3 <- factor(Kenya_impairement_cleaned$page_3)
Kenya_impairement_cleaned$SEX_3 <- factor(Kenya_impairement_cleaned$SEX_3)
Kenya_impairement_cleaned$number1_3 <- factor(Kenya_impairement_cleaned$number1_3)
Kenya_impairement_cleaned$grade_4 <- factor(Kenya_impairement_cleaned$grade_4)
Kenya_impairement_cleaned$page_4 <- factor(Kenya_impairement_cleaned$page_4)
Kenya_impairement_cleaned$SEX_4 <- factor(Kenya_impairement_cleaned$SEX_4)
Kenya_impairement_cleaned$number1_4 <- factor(Kenya_impairement_cleaned$number1_4)
Kenya_impairement_cleaned$pgrade_5 <- factor(Kenya_impairement_cleaned$pgrade_5)
Kenya_impairement_cleaned$SEX_5 <- factor(Kenya_impairement_cleaned$SEX_5)
Kenya_impairement_cleaned$number1_5 <- factor(Kenya_impairement_cleaned$number1_5)
Kenya_impairement_cleaned$pgrade_6 <- factor(Kenya_impairement_cleaned$pgrade_6)
Kenya_impairement_cleaned$page_6 <- factor(Kenya_impairement_cleaned$page_6)
Kenya_impairement_cleaned$SEX_6 <- factor(Kenya_impairement_cleaned$SEX_6)
Kenya_impairement_cleaned$number1_6 <- factor(Kenya_impairement_cleaned$number1_6)
Kenya_impairement_cleaned$pgrade_7 <- factor(Kenya_impairement_cleaned$pgrade_7)
Kenya_impairement_cleaned$page_7 <- factor(Kenya_impairement_cleaned$page_7)
Kenya_impairement_cleaned$SEX_7 <- factor(Kenya_impairement_cleaned$SEX_7)
Kenya_impairement_cleaned$number1_7 <- factor(Kenya_impairement_cleaned$number1_7)
Kenya_impairement_cleaned$pgrade_8 <- factor(Kenya_impairement_cleaned$pgrade_8)
Kenya_impairement_cleaned$page_8 <- factor(Kenya_impairement_cleaned$page_8)
Kenya_impairement_cleaned$SEX_8 <- factor(Kenya_impairement_cleaned$SEX_8)
Kenya_impairement_cleaned$number1_8 <- factor(Kenya_impairement_cleaned$number1_8)
Kenya_impairement_cleaned$pgrade_9 <- factor(Kenya_impairement_cleaned$pgrade_9)
Kenya_impairement_cleaned$page_9 <- factor(Kenya_impairement_cleaned$page_9)
Kenya_impairement_cleaned$SEX_9 <- factor(Kenya_impairement_cleaned$SEX_9)
Kenya_impairement_cleaned$number1_9 <- factor(Kenya_impairement_cleaned$number1_9)
Kenya_impairement_cleaned$pgrade_10 <- factor(Kenya_impairement_cleaned$pgrade_10)
Kenya_impairement_cleaned$page_10 <- factor(Kenya_impairement_cleaned$page_10)
Kenya_impairement_cleaned$SEX_10 <- factor(Kenya_impairement_cleaned$SEX_10)
Kenya_impairement_cleaned$number1_10 <- factor(Kenya_impairement_cleaned$number1_10)
Kenya_impairement_cleaned$pgrade_11 <- factor(Kenya_impairement_cleaned$pgrade_11)
Kenya_impairement_cleaned$page_11 <- factor(Kenya_impairement_cleaned$page_11)
Kenya_impairement_cleaned$SEX_11 <- factor(Kenya_impairement_cleaned$SEX_11)
Kenya_impairement_cleaned$number1_11 <- factor(Kenya_impairement_cleaned$number1_11)
Kenya_impairement_cleaned$pgrade_12 <- factor(Kenya_impairement_cleaned$pgrade_12)
Kenya_impairement_cleaned$page_12 <- factor(Kenya_impairement_cleaned$page_12)
Kenya_impairement_cleaned$SEX_12 <- factor(Kenya_impairement_cleaned$SEX_12)
Kenya_impairement_cleaned$number1_12 <- factor(Kenya_impairement_cleaned$number1_12)
Kenya_impairement_cleaned$pgrade_13 <- factor(Kenya_impairement_cleaned$pgrade_13)
Kenya_impairement_cleaned$page_13 <- factor(Kenya_impairement_cleaned$page_13)
Kenya_impairement_cleaned$SEX_13 <- factor(Kenya_impairement_cleaned$SEX_13)
Kenya_impairement_cleaned$number1_13 <- factor(Kenya_impairement_cleaned$number1_13)
Kenya_impairement_cleaned$pgrade_14 <- factor(Kenya_impairement_cleaned$pgrade_14)
Kenya_impairement_cleaned$page_14 <- factor(Kenya_impairement_cleaned$page_14)
Kenya_impairement_cleaned$SEX_14 <- factor(Kenya_impairement_cleaned$SEX_14)
Kenya_impairement_cleaned$number1_14 <- factor(Kenya_impairement_cleaned$number1_14)
Kenya_impairement_cleaned$pgrade_15 <- factor(Kenya_impairement_cleaned$pgrade_15)
Kenya_impairement_cleaned$page_15 <- factor(Kenya_impairement_cleaned$page_15)
Kenya_impairement_cleaned$SEX_15 <- factor(Kenya_impairement_cleaned$SEX_15)
Kenya_impairement_cleaned$number1_15 <- factor(Kenya_impairement_cleaned$number1_15)
Kenya_impairement_cleaned$pgrade_16 <- factor(Kenya_impairement_cleaned$pgrade_16)
Kenya_impairement_cleaned$page_16 <- factor(Kenya_impairement_cleaned$page_16)
Kenya_impairement_cleaned$SEX_16 <- factor(Kenya_impairement_cleaned$SEX_16)
Kenya_impairement_cleaned$number1_16 <- factor(Kenya_impairement_cleaned$number1_16)
Kenya_impairement_cleaned$grade_1 <- factor(Kenya_impairement_cleaned$grade_1)
Kenya_impairement_cleaned$age_1 <- factor(Kenya_impairement_cleaned$age_1)
Kenya_impairement_cleaned$sex_one <- factor(Kenya_impairement_cleaned$sex_one)
Kenya_impairement_cleaned$number_1 <- factor(Kenya_impairement_cleaned$number_1)
Kenya_impairement_cleaned$grade_2 <- factor(Kenya_impairement_cleaned$grade_2)
Kenya_impairement_cleaned$age_2 <- factor(Kenya_impairement_cleaned$age_2)
Kenya_impairement_cleaned$sex_2 <- factor(Kenya_impairement_cleaned$sex_2)
Kenya_impairement_cleaned$number_2 <- factor(Kenya_impairement_cleaned$number_2)
Kenya_impairement_cleaned$pgrade_17 <- factor(Kenya_impairement_cleaned$pgrade_17)
Kenya_impairement_cleaned$page_17 <- factor(Kenya_impairement_cleaned$page_17)
Kenya_impairement_cleaned$SEX_17 <- factor(Kenya_impairement_cleaned$SEX_17)
Kenya_impairement_cleaned$number1_17 <- factor(Kenya_impairement_cleaned$number1_17)
Kenya_impairement_cleaned$pgrade_18 <- factor(Kenya_impairement_cleaned$pgrade_18)
Kenya_impairement_cleaned$page_18 <- factor(Kenya_impairement_cleaned$page_18)
Kenya_impairement_cleaned$SEX_18 <- factor(Kenya_impairement_cleaned$SEX_18)
Kenya_impairement_cleaned$number1_18 <- factor(Kenya_impairement_cleaned$number1_18)
Kenya_impairement_cleaned$pgrade_19 <- factor(Kenya_impairement_cleaned$pgrade_19)
Kenya_impairement_cleaned$page_19 <- factor(Kenya_impairement_cleaned$page_19)
Kenya_impairement_cleaned$SEX_19 <- factor(Kenya_impairement_cleaned$SEX_19)
Kenya_impairement_cleaned$number1_19 <- factor(Kenya_impairement_cleaned$number1_19)
Kenya_impairement_cleaned$pgrade_20 <- factor(Kenya_impairement_cleaned$pgrade_20)
Kenya_impairement_cleaned$page_20 <- factor(Kenya_impairement_cleaned$page_20)
Kenya_impairement_cleaned$SEX_20 <- factor(Kenya_impairement_cleaned$SEX_20)
Kenya_impairement_cleaned$number1_20 <- factor(Kenya_impairement_cleaned$number1_20)
Kenya_impairement_cleaned$pgrade_21 <- factor(Kenya_impairement_cleaned$pgrade_21)
Kenya_impairement_cleaned$page_21 <- factor(Kenya_impairement_cleaned$page_21)
Kenya_impairement_cleaned$SEX_21 <- factor(Kenya_impairement_cleaned$SEX_21)
Kenya_impairement_cleaned$number1_21 <- factor(Kenya_impairement_cleaned$number1_21)
Kenya_impairement_cleaned$pgrade_22 <- factor(Kenya_impairement_cleaned$pgrade_22)
Kenya_impairement_cleaned$page_22 <- factor(Kenya_impairement_cleaned$page_22)
Kenya_impairement_cleaned$SEX_22 <- factor(Kenya_impairement_cleaned$SEX_22)
Kenya_impairement_cleaned$number1_22 <- factor(Kenya_impairement_cleaned$number1_22)
Kenya_impairement_cleaned$pgrade_23 <- factor(Kenya_impairement_cleaned$pgrade_23)
Kenya_impairement_cleaned$page_23 <- factor(Kenya_impairement_cleaned$page_23)
Kenya_impairement_cleaned$SEX_23 <- factor(Kenya_impairement_cleaned$SEX_23)
Kenya_impairement_cleaned$number1_23 <- factor(Kenya_impairement_cleaned$number1_23)
Kenya_impairement_cleaned$pgrade_24 <- factor(Kenya_impairement_cleaned$pgrade_24)
Kenya_impairement_cleaned$page_24 <- factor(Kenya_impairement_cleaned$page_24)
Kenya_impairement_cleaned$SEX_24 <- factor(Kenya_impairement_cleaned$SEX_24)
Kenya_impairement_cleaned$number1_24 <- factor(Kenya_impairement_cleaned$number1_24)
Kenya_impairement_cleaned$pgrade_25 <- factor(Kenya_impairement_cleaned$pgrade_25)
Kenya_impairement_cleaned$page_25 <- factor(Kenya_impairement_cleaned$page_25)
Kenya_impairement_cleaned$SEX_25 <- factor(Kenya_impairement_cleaned$SEX_25)
Kenya_impairement_cleaned$number1_25 <- factor(Kenya_impairement_cleaned$number1_25)
Kenya_impairement_cleaned$pgrade_26 <- factor(Kenya_impairement_cleaned$pgrade_26)
Kenya_impairement_cleaned$page_26 <- factor(Kenya_impairement_cleaned$page_26)
Kenya_impairement_cleaned$SEX_26 <- factor(Kenya_impairement_cleaned$SEX_26)
Kenya_impairement_cleaned$number1_26 <- factor(Kenya_impairement_cleaned$number1_26)
Kenya_impairement_cleaned$pgrade_27 <- factor(Kenya_impairement_cleaned$pgrade_27)
Kenya_impairement_cleaned$page_27 <- factor(Kenya_impairement_cleaned$page_27)
Kenya_impairement_cleaned$SEX_27 <- factor(Kenya_impairement_cleaned$SEX_27)
Kenya_impairement_cleaned$number1_27 <- factor(Kenya_impairement_cleaned$number1_27)
Kenya_impairement_cleaned$pgrade_28 <- factor(Kenya_impairement_cleaned$pgrade_28)
Kenya_impairement_cleaned$page_28 <- factor(Kenya_impairement_cleaned$page_28)
Kenya_impairement_cleaned$SEX_28 <- factor(Kenya_impairement_cleaned$SEX_28)
Kenya_impairement_cleaned$number1_28 <- factor(Kenya_impairement_cleaned$number1_28)

# Checking the dataset to verify if characters have been converted to factors

glimpse(Kenya_impairement_cleaned)


# Migrating the SurveyCTO dataset from R to ActivityInfo



glimpse(Kenya_impairement_cleaned)

# Using the dply function "rename" to rename one of the "SEX_1" column that has been repeated

Kenya_impairement_cleaned<- Kenya_impairement_cleaned %>% 
  rename(sex_one=sex_1)

glimpse(Kenya_impairement_cleaned)



# Several variable have been repeated, cleaning the dataset to remove repeated variables

glimpse(Kenya_impairement_cleaned)
Kenya_impairement_cleaned <- Kenya_impairement_cleaned[,-c("sex_2","sex_3","sex_4","sex_5","sex_6","sex_7","sex_8","sex_9","sex_10")]



# Re-uploading data into ActivityInfo


KI <- createFormSchemaFromData(x=Kenya_impairement_cleaned,
                               databaseId = "cfxbif1ll28higt1",
                               label = "Kenya Impairement",
                               requiredColumns = "grade",
                               upload = TRUE)


library(tidyverse)
library(gapminder)
# installing gapminder dataset and gganimate packages
install.packages("gapminder")
install.packages("gganimate")
library(gganimate)
library(ggplot2)

view(gapminder)

theme_set(theme-light())
edd<- gapminder::gapminder %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent))+
  geom_point(show.legend = FALSE, alpha=0.7)+
  scale_color_viridis_d()+
  scale_size(range = c(2,12))+
  scale_x_log10()+
  labs(x="GDP per Capita", y= "Life Expectancy")

k<- edd + transition_time(year)+
  labs(title = "Year: {frame_time}")

anim_save("Eddie.animation.gif", k)

# Installing the gganimate package.
devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggplot2)                              


# simpler example

p <- gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  transition_time(year)

gif <- p
gif
anim_save("gapminder_animation.gif", p)


# Example 3

Edward <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

anim_save("facets.gif", Edward)