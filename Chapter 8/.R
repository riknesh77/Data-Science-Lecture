#Dataframe Manipulation
library(dplyr)

student_data <- read.csv("C:/Users/User/Downloads/student_data.csv")

#1 intro
View(student_data)
summary(student_data)
head(student_data, 5)
tail(student_data)

#2 filter
Student_fail<- student_data %>% filter(final_exam_mark< 40)
View(Student_fail)

#3 arrange increasing order
mydata<- student_data%>% filter(final_exam_mark > 40) %>% arrange(final_exam_mark)
View(mydata)

#3 arrange decreasing order
mydata1<- student_data %>% filter(final_exam_mark > 40) %>% arrange(desc(final_exam_mark))
View(mydata1)

#4 select
mydata3 <- student_data%>% select(student_id,coursework_mark, final_exam_mark)
View(mydata3)
glimpse(mydata3)

#5 mutate
mydata4 = student_data%>% mutate(Total_Mark=(coursework_mark + final_exam_mark/200*100))
View(mydata4)

#5 mutate using cbind
mydata5 <- cbind(student_data , Total_Mark = (student_data $coursework_mark + student_data$final_exam_mark/200*100))
View(mydata5)

#Descriptive Analytics
data <- iris
View(data)

#structure
str(data)

#min function
min(data$Sepal.Length) #this produces 4.3

#max function
max(data$Sepal.Length) #this produces 7.9

#range function
range(data$Sepal.Length)

#summary
summary(data)

#boxplot quantiles
A<-c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort(A)
quantile(A,0.25)
quantile(A,0.75)

#boxplot interquantile range
IQR(A)

#Class Exercise
#Histogram
hist(iris$Sepal.Length, 
     main = "Histogram of Sepal Length", 
     xlab = "Sepal Length (cm)", 
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

#Boxplot
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species",
        xlab = "Species",
        ylab = "Sepal Length (cm)",
        col = c("lightgreen", "lightpink", "lightyellow"))

#Scatter plot
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Petal Length (cm)",
     col = as.numeric(iris$Species),
     pch = 19)

legend("topleft",
       legend = levels(iris$Species),
       col = 1:3,
       pch = 19)

#Outliers
dfplayers <- read.csv("C:/Users/User/Downloads/players.csv")
median_age <- median(dfplayers$Age, na.rm =TRUE)
dfplayers$Age[dfplayers$Age<18 | dfplayers$Age>38]<-median_age
View(dfplayers)

#Check for NA values
is.na(dfplayers)

#Boxplot
data3<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data3, main = "Boxplot")

#quantile values
first_q<-quantile(data3,0.25) #this is 26
third_q<-quantile(data3,0.75) #this is 31.75

#IQR
iqr<-IQR(data3) #this produces 5.75

#lower extreme value
le<-first_q - 1.5 * iqr #this produces 17.375

#upper extreme value
ue<-third_q + 1.5 * iqr #this produces 40.375

#drop values
data_new<-data3
data_new <- data_new[!data_new<le]
data_new <- data_new[!data_new>ue]
data_new

#replace value with mean
data_new <- data3
avg <- round(mean(data_new)) #for the purpose of example we round up value
data_new[data_new<le] <- avg
data_new[data_new>ue] <- avg
data_new

#manage values below le and above ue
data_new <- data
data_new[data_new<le] <- le
data_new[data_new>ue] <- ue
data_new
