library(caret)
library(data.table)

train_set <- fread(file = "pml-training.csv", header = T, sep = ",", na.strings = "")
test_set <- fread(file = "pml-testing.csv", header = T, sep = ",", na.strings = "")

#Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl
#in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front
#(Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing
#the hips to the front (Class E).

#Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes.
#Participants were supervised by an experienced weight lifter to make sure the execution complied to the manner they
#were supposed to simulate. The exercises were performed by six male participants aged between 20-28 years,
#with little weight lifting experience. We made sure that all participants could easily simulate the mistakes in a 
#safe and controlled manner by using a relatively light dumbbell (1.25kg).

###See if there are some exclusive columns for some of the datasets
setdiff(colnames(train_set), colnames(test_set))
setdiff(colnames(test_set), colnames(train_set))

###Remove extra columns
#Remove from training columns that don't have useful data for our model
filtered_train_set <- train_set[, -c("V1", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")]

###Remove columns with many NAs
NAs_in_columns <- sapply(filtered_train_set, function(x) sum(is.na(x)))

#Since the columns that have NAs have a lot of them, I'll just remove all of them
columns_too_keep <- names(NAs_in_columns[NAs_in_columns == 0])
filtered_train_set <- filtered_train_set[, columns_too_keep, with = F]

###Remove columns with little variance
little_var_columns <- nearZeroVar(filtered_train_set, saveMetrics = T)
dt_little_var_columns <- as.data.table(little_var_columns)
dt_little_var_columns$name <- rownames(little_var_columns)

columns_too_keep <- dt_little_var_columns[nzv == F]$name
filtered_train_set <- filtered_train_set[, columns_too_keep, with = F]

###Fix a few types
filtered_train_set$classe <- factor(filtered_train_set$classe, levels = c("A", "B", "C", "D", "E"))

###Add a tag to do a 5fold cross validation
set.seed(42)
filtered_train_set$k_tag <- sample(c(1:5), size = filtered_train_set[, .N], replace = T)
filtered_train_set[, .N, by = k_tag]

###Test the RF model using cross validation
accuracy <- c()
for (i in 1:5) {
  train_set_for <- filtered_train_set[k_tag != i, -c("k_tag")]
  validation_set_for <- filtered_train_set[k_tag == i, -c("k_tag")]
  
  model_rf <- train(classe ~ ., data = train_set_for, method = "rf", ntree = 10)
  
  prediction <- predict(model_rf, validation_set_for)
  model_rf_cm <- confusionMatrix(prediction, validation_set_for$classe)
  
  accuracy <- c(accuracy, model_rf_cm$overall[["Accuracy"]])
}
accuracy

###Since the model gave very good results, I'll create the final model and apply it to the test set
model_rf <- train(classe ~ ., data = filtered_train_set[, -c("k_tag")], method = "rf", ntree = 10)

prediction <- predict(model_rf, test_set)

test_set$predicted_classe <- prediction
test_set[, .(problem_id, predicted_classe)]
