#Support Vector Machine (SVM) example using iris dataset
data("iris")
head(iris) # inspecting the first six rows of the dataset
str(iris) # structure of the dataset
library(ggplot2)
library(e1071)
# we will seperate the Species based on the color and plot with Petal.Legth Vs Petal.Width
# We can clearly see the seperation of Setosa however, there is an overlappings in Versicolor and Virginica.
# Now we plot using the qplot() fuction, X=Petal.Length, Y = Petal.Width using the color separation respect to Species.
qplot(Petal.Length, Petal.Width, data=iris, color = Species)
help("svm")
svm_model1 <- svm(Species~., data = iris)
summary(svm_model1)
plot(svm_model1, data = iris, 
     Petal.Width~Petal.Length, slice = 
       list(Sepal.Width = 3, Sepal.Length = 4))
pred1 <- predict(svm_model1, iris)
# creating a table using the predicted one and the actual iris dataset
table1 <- table(Predicted = pred1, Actual = iris$Species)
table1
#We can calculate the model1 accuracy
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
# We can calcuate the missclassification rate 
Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate


svm_model2 <- svm(Species~., data = iris, kernel = "linear")
# Using the summary command to see the summary of our second model, we pass the svm_model2 to the 
# summary() function
summary(svm_model2)
plot(svm_model2, data = iris, 
     Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, 
                                            Sepal.Length = 4))
pred2 <- predict(svm_model2, iris)
# creating a table using the predicted one and the actual iris dataset
table2 <- table(Predicted = pred2, Actual = iris$Species)
table2
#We can calculate the model2 accuracy
Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate
# We can calcuate the missclassification rate 
Model2_MissClassificationRate = 1 - Model2_accuracyRate
Model2_MissClassificationRate


svm_model3 <- svm(Species~., data = iris, kernel = "polynomial")
summary(svm_model3)
plot(svm_model3, data = iris, 
     Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, 
                                            Sepal.Length = 4))
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted = pred3, Actual = iris$Species)
table3

Model3_accuracyRate = sum(diag(table3))/sum(table3)
Model3_accuracyRate

Model3_MissClassificationRate = 1 - Model3_accuracyRate
Model3_MissClassificationRate
