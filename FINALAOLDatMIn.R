library(arules)

#Import the dataset
diabetes_data <- read.csv("diabetes_data.csv", stringsAsFactors = FALSE)

## PREPROCESSING
#Factor the columns
diabetes_data$Sex  <- factor(diabetes_data$Sex)
diabetes_data$HighChol  <- factor(diabetes_data$HighChol )
diabetes_data$CholCheck  <- factor(diabetes_data$CholCheck)
diabetes_data$Smoker  <- factor(diabetes_data$Smoker )
diabetes_data$HeartDiseaseorAttack  <- factor(diabetes_data$HeartDiseaseorAttack )
diabetes_data$PhysActivity  <- factor(diabetes_data$PhysActivity )
diabetes_data$Fruits  <- factor(diabetes_data$Fruits )
diabetes_data$Veggies <- factor(diabetes_data$Veggies)
diabetes_data$HvyAlcoholConsump  <- factor(diabetes_data$HvyAlcoholConsump )
diabetes_data$GenHlth  <- factor(diabetes_data$GenHlth  )
diabetes_data$DiffWalk <- factor(diabetes_data$DiffWalk )
diabetes_data$Stroke <- factor(diabetes_data$Stroke )
diabetes_data$HighBP <- factor(diabetes_data$HighBP )
diabetes_data$Diabetes<- factor(diabetes_data$Diabetes)

# Binning the dataset
diabetes_data$Age <- cut(diabetes_data$Age, breaks = c(0, 1, 3, 5, 14), 
                         labels = c("Child", "Young Adult", "Adult", "Senior"))
diabetes_data$Age <- factor(diabetes_data$Age)

diabetes_data$BMI <- cut(diabetes_data$BMI, breaks = c(0, 18.5, 24.9, 29.9, 200), 
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))
diabetes_data$BMI <- factor(diabetes_data$BMI)

diabetes_data$PhysHlth <- cut(diabetes_data$PhysHlth, breaks = c(-1, 10, 20, 31), 
                              labels = c("Low", "Medium", "High"))
diabetes_data$PhysHlth <- factor(diabetes_data$PhysHlth)

diabetes_data$MentHlth <- cut(diabetes_data$MentHlth, breaks = c(-1, 10, 20, 31), 
                              labels = c("Low", "Medium", "High"))
diabetes_data$MentHlth <- factor(diabetes_data$MentHlth)

#Set the dataset as transactions
transactions <- as(diabetes_data, "transactions")

## APRIORI
# Generate rules
rules <- apriori(transactions, 
                 parameter = list(supp = 0.1, conf = 0.7, target="Rules"),
                 appearance = list(rhs = c("Diabetes=1"),
                                   default = "lhs"))

# Subset rules where the RHS includes 'Diabetes=1' or is positive for diabetes
rules_apriori <- subset(rules, rhs %pin% "Diabetes=1") 

# Sort rules by support in descending order
rules_apriori <- sort(rules_apriori, by = "confidence", decreasing = TRUE)

# Inspect the rules
inspect(head(rules_apriori, 10))


## ECLAT
#Generate Frequent Itemset
frequentItems <- eclat(transactions, parameter = list(supp = 0.1))

# Get itemsets where only ones with Diabetes=1 appears
frequentItems <- subset(frequentItems, items %pin% "Diabetes=1") 

#Inspect the frequent itemset
frequentItems <- sort(frequentItems, by = "support", decreasing = TRUE)
inspect(head(frequentItems, 21))


##FP GROWTH
#Generate rules
#use the frequent itemsets from ECLAT
rules_fpgrowth <- ruleInduction(frequentItems, transactions, supp=0.1, conf = 0.7)

# Subset rules where the RHS includes 'Diabetes=1' or is positive for diabetes
rules_fpgrowth <- subset(rules_fpgrowth, rhs %pin% "Diabetes=1") 

# Sort rules by support in descending order
rules_fpgrowth <- sort(rules_fpgrowth, by = "confidence", decreasing = TRUE)

# Inspect rules
inspect(head(rules_fpgrowth, 10))

