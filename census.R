fLoc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"

# read data, set stringsAsFactor FALSE
train <- read.table(fLoc, h = FALSE, sep = ',', stringsAsFactor = FALSE)
names(train) <- c("age", "workclass", "fnlwgt", "edu", "edu_num",
                  "marital", "occup", "relation", "race", "sex",
                  "capital_gain", "capital_loss", "hours_per_week",
                  "native", "higher50k")
summary(train)
str(train)
head(train)
dim(train)


library(ggplot2)
library(gridExtra)
# histogram:
# bin width = 2 * IQR / (N of var) ^ (1 / 3)
# in geom, y = ..density.., ..count..

# age
h0 <- ggplot(train, aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", fill = "gray",
                 binwidth = 2 * IQR(train$age) /
                 (dim(train)[1]) ^ (1 / 3)) +
  geom_density() +
  labs(x = "Age", y = "Probability") +
  theme_bw()

h1 <- ggplot(train, aes(x = age)) +
  geom_histogram(aes(y = ..count..),
                 color = "white", fill = "gray",
                 binwidth = 2 * IQR(train$age) /
                 (dim(train)[1]) ^ (1 / 3)) +
  labs(x = "Age", y = "Count") +
  theme_bw()

# fnlwgt
h2 <- ggplot(train, aes(x = fnlwgt)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", fill = "gray",
                 binwidth = 2 * IQR(train$fnlwgt) /
                 (dim(train)[1]) ^ (1 / 3)) +
  geom_density() +
  labs(x = "Sampling weight", y = "Probability") +
  theme_bw()

h3 <- ggplot(train, aes(x = fnlwgt)) +
  geom_histogram(aes(y = ..count..),
                 color = "white", fill = "gray",
                 binwidth = 2 * IQR(train$fnlwgt) /
                 (dim(train)[1]) ^ (1 / 3)) +
  labs(x = "Sampling weight", y = "Count") +
  theme_bw()

#edu_num
h4 <- ggplot(train, aes(x = edu_num)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", fill = "gray",
                 binwidth = 2 * IQR(train$edu_num) /
                 (dim(train)[1]) ^ (1 / 3)) +
  geom_density() +
  labs(x = "Education number", y = "Probability") +
  theme_bw()

h5 <- ggplot(train, aes(x = edu_num)) +
  geom_histogram(aes(y = ..count..),
                 color = "white", fill = "gray") +
               #  binwidth = 2 * IQR(train$edu_num) /
                # (dim(train)[1]) ^ (1 / 3)) +
  labs(x = "Education number", y = "Count") +
  theme_bw()


# capital_gain
h6 <- ggplot(train, aes(x = capital_gain)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", fill = "gray") +
#                 binwidth = 2 * IQR(train$capital_gain) /
#                 (dim(train)[1]) ^ (1 / 3)) +
  geom_density() +
  labs(x = "Capital gain", y = "Probability") +
  theme_bw()

h7<- ggplot(train, aes(x = capital_gain)) +
  geom_histogram(aes(y = ..count..),
                 color = "white", fill = "gray") +
  labs(x = "Capital gain", y = "Count") +
  theme_bw()

# capital_loss
h8 <- ggplot(train, aes(x = capital_loss)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", fill = "gray") +
  geom_density() +
  labs(x = "Capital loss", y = "Probability") +
  theme_bw()

h9 <- ggplot(train, aes(x = capital_loss)) +
  geom_histogram(aes(y = ..count..),
                 color = "white", fill = "gray") +
  labs(x = "Capital loss", y = "Count") +
  theme_bw()

# hours_per_week
h10 <- ggplot(train, aes(x = hours_per_week)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", fill = "gray",
                 binwidth = 2 * IQR(train$hours_per_week) /
                 (dim(train)[1]) ^ (1 / 3)) +
  geom_density() +
  labs(x = "Hours per week", y = "Probability") +
  theme_bw()

h11 <- ggplot(train, aes(x = hours_per_week)) +
  geom_histogram(aes(y = ..count..),
                 color = "white", fill = "gray") +
  labs(x = "Hours per week", y = "Count") +
  theme_bw()


# categorial variables
# workclass
b0 <- ggplot(train, aes(x = factor(workclass,
       levels = names(sort(table(train$workclass)))))) +
  geom_bar() +
#  facet_grid(higher50k ~ sex) +
  labs(x = "Workclass", y = "Counts") +
  coord_flip()

# edu
b1 <- ggplot(train, aes(x = factor(edu,
       levels = names(sort(table(train$edu)))))) +
  geom_bar() +
  labs(x = "Education", y = "Counts") +
  coord_flip()

# marital
b2 <- ggplot(train, aes(x = factor(marital,
       levels = names(sort(table(train$marital)))))) +
  geom_bar() +
  labs(x = "Marital", y = "Counts") +
  coord_flip()

# occupation
b3 <- ggplot(train, aes(x = factor(occup,
       levels = names(sort(table(train$occup)))))) +
  geom_bar() +
  labs(x = "Occupation", y = "Counts") +
  coord_flip()

# relation
b4 <- ggplot(train, aes(x = factor(relation,
       levels = names(sort(table(train$relation)))))) +
  geom_bar() +
  labs(x = "Relation", y = "Counts") +
  coord_flip()

# race
b5 <- ggplot(train, aes(x = factor(race,
       levels = names(sort(table(train$race)))))) +
  geom_bar() +
  labs(x = "Race", y = "Counts") +
  coord_flip()

# sex
b6 <- ggplot(train, aes(x = factor(sex,
       levels = names(sort(table(train$sex)))))) +
  geom_bar() +
  labs(x = "Sex", y = "Counts") +
  coord_flip()

# native
b7 <- ggplot(train, aes(x = factor(native,
       levels = names(sort(table(train$native)))))) +
  geom_bar() +
  labs(x = "Native", y = "Counts") +
  coord_flip()

# >50k
b8 <- ggplot(train, aes(x = factor(higher50k,
       levels = names(sort(table(train$higher50k)))))) +
  geom_bar() +
  labs(x = "> 50000 $", y = "Counts") +
  coord_flip()





t0 <- ggplot(train, aes(x = age)) +
  geom_bar()
t1 <- ggplot(train, aes(x = fnlwgt)) +
  geom_bar()
t2 <- ggplot(train, aes(x = edu_num)) +
  geom_bar()
t3 <- ggplot(train, aes(x = capital_gain)) +
  geom_bar()
t4 <- ggplot(train, aes(x = capital_loss)) +
  geom_bar()
t5 <- ggplot(train, aes(x = hours_per_week)) +
  geom_bar()


# grid.arrange
grid.arrange(b0, b1, b2, b3, b4, b5, b6, b7, b8, ncol = 3)
grid.arrange(h0, h2, h4, h6, h8, h10, ncol = 3)
grid.arrange(h1, h3, h5, h7, h9, h11, ncol = 3)
grid.arrange(t0, t1, t2, t3, t4, t5, ncol = 3)

# which

# native-country, occupation, and workclass contain missings
library(mice)
md.pattern(train)


