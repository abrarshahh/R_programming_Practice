#simple Regression
library(caTools)

data <- read.csv("Salary_Data.csv")

View(data)

colSums(is.na(data))

barplot(data$YearsExperience,xlab = "People", ylab = "Year of Experience", main = "Experience Graph")

hist(data$YearsExperience,col = "skyblue")

#train/test split
set.seed(123)

split = sample.split(data$Salary,SplitRatio = 0.8)

train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

dim(train)
dim(test)

model <- lm(
  Salary~.,
  data = train,
  method = "qr"
)

summary(model)

y_pred <- predict(model,test) 
y_pred



plot(Salary~.,
     train)
