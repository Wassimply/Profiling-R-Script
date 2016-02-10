
####################_CONSTANTS_####################
postsPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
profPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
modelsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/"
bagOfWordsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/bagOfWords.txt"
trainingLength <- 7000 #75% of users

####################_FUNCTIONS_####################

#reads users' post and changes the class of ID and Gender to character and factor respectively
readUserProfiles <- function() {
  
  col.class <- c("integer","character", "numeric", "factor", rep("numeric",5))
  profile <- read.csv(profPath, header = TRUE, colClasses = col.class)
  
  return(profile)
}


#read post of a user, filters the post and converts it to lowercase
readUserPost <- function(userId) {
  file.path = paste(postsPath, userId, ".txt", sep="")
  #print(paste(file.path,i))
  file.text <- scan(file.path, what = "character", sep = "\n")
  file.text <- gsub("[[:punct:]]", " ", file.text)
  file.text <- tolower(file.text)
  return(file.text)
}

#couts the frequency of words in a post using bag-of-words
countWordsInVector <- function(bagOfWords, post) {
  counts <- c();
  
  post.tokens <- strsplit(post, " ")[[1]]
  
  #changes the token vector of post to frequency table of the post
  tokens.table <- table(post.tokens)
  
  #count the occurance of the bagOfWord
  tokens <- names(tokens.table)
  counts <- match(bagOfWords, tokens, nomatch = 0)
  
  return (counts)
}

#classifies a user given file path of user's post and a predictive model
classifyPost <- function(filePath, model) {
  post <- scan(filePath, what = "character", sep = "\n")
  post <- gsub("[[:punct:]]", " ", post)
  post <- tolower(post)
  
  counts <- countWordsInVector(wordsBag, post)
  countLst <- as.list(counts)
  names(countLst) <- wordsBag
  
  return (predict(model, countLst))
}

####################_MAIN PART OF SCRIPT_####################

library(e1071)

#readModels from file
ageModel <- readRDS("/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/ageBayesModel.rds")
genderModel <- readRDS("/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/genderBayesModel.rds")
wordsBag <- scan(bagOfWordsPath, what = "character", sep="\n")
test.users <- readUserProfiles()[7001:9500, ]

predictedAge <- c()
predictedGender <- c()

for(i in 1:20) {
  sample <- test.users[i,]
  
  #use the path where users' posts are located
  path <- paste(postsPath, sample$userid, ".txt", sep = "")
  
  #actualAge <- sample$age
  ageResult <- classifyPost(path, ageModel)
  print(paste("age ---> ", ageResult, "  ", sample$age))
  
  
  #actualGender <- sample$gender
  gendResult <- classifyPost(path, genderModel)
  print(paste("gender ---> ", gendResult, "  ", sample$gender))
}
