
# Load packages
library(caTools)
library(class)

# Import data
data <- read.csv('Social_Network_Ads.csv')
data = data[, 2:5]

# Encoding variables
# Changing 'male' and 'female' to '1' and '2'
data$Gender <- factor(data$Gender, 
                         levels = c("Male", "Female"), 
                         labels = c(0, 1))

# R not reading values as 1 and 0. Reenforcing the values here
data$Purchased = factor(data$Purchased,
                        levels = c(0, 1))

# Feature scaling
# Normalizing age and salery data
data[, 2] = scale(data[, 2])
data[, 3] = scale(data[, 3])

# Split data into training and test
set.seed(42)
split = sample.split(data$Purchased, SplitRatio = 0.75)
training_set = subset(data, split == FALSE)
test_set = subset(data, split == TRUE)

# kNN
y_predict = knn(train = training_set[, -4],
             test = test_set[, -4],
             cl = training_set[, 4],
             k = 5)

# Confusion Matrix
con_matrix = table (test_set[, 4], y_predict)
# @ k = 3 11.66%
# @ k = 5 11.33%
# @ k = 7 12%

# Prepare the training scheme
# Repeat the training process over and over
control = trainControl(method = 'repeatedcv',
                       number = 10,
                       repeats = 3)

# Train the model
model = train(Purchased~.,
              data = data,
              method = 'glm',
              preProcess = "scale",
              trControl = control)

# Estimate the importance
importance <- varImp(model, scale = FALSE)

# Summerize importance
print(importance)
plot(importance)
