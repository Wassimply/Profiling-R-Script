
####################_CONSTANTS_####################
postsPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
profPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
bagOfWordsPath <- "/Users/Mehdi/Desktop/ML/Project/bagOfWords.txt"
modelsPath <- "/Users/Mehdi/Desktop/ML/Project/"
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
countWordsInVector_old <- function(bagOfWords, post) {
  counts <- c();
  
  post.tokens <- strsplit(post, " ")[[1]]
  
  #changes the token vector of post to frequency table of the post
  tokens.table <- table(post.tokens)
  
  #count the occurance of the bagOfWord
  tokens <- names(tokens.table)
  for(i in 1 : length(bagOfWords)) {
    if( bagOfWords[i] %in% tokens)
      counts[i] <- tokens.table[ names(tokens.table)==bagOfWords[i] ]
    else
      counts[i] <- 0
  }
  return(counts)
}

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

#generates the frequency table of all posts and saves a copy in model's folder
createFrequencyTable <- function (profiles) {

  wordsBag <- scan(bagOfWordsPath, what = "character", sep="\n")
  
  featuresCount <- c()
  for(i in 1 : nrow(profiles)) {
    post <- readUserPost(profiles[i,2])
    counts <- countWordsInVector(wordsBag, post)
    featuresCount <- rbind(featuresCount, counts)
  }
  
  colnames(featuresCount) <- wordsBag
  #rownames(featuresCount) <- profiles[,2]
  
  write.csv(featuresCount, paste(modelsPath, "frequencyTable.csv" ,sep = ""))
  return (featuresCount)
}

#generates the naive bayes model to predict gender
createGenderModel <- function(frequencyTbl) { 
  
  gender <- user.profiles$gender
  
  gender.training.data <- cbind.data.frame(gender, frequencyTbl)
  
  #gender.training.data$gender <- as.factor(gender.training.data$gender)
  model <- naiveBayes(gender.training.data[,-1], gender.training.data$gender)
  
  saveRDS(model,paste(modelsPath,"genderBayesModel.rds", sep=""))
  
  return (model)
}

#generates and saves the naive bayes model to predict age
createAgeModel <- function(frequencyTbl) { 
  
  age <- cut(user.profiles$age, 
             breaks = c(-Inf, 24, 34, 49, Inf), 
             labels = c("xx-24", "25-34", "35-49", "50-xx"), 
             right = T)
  tdata.age <- cbind.data.frame(age, frequencyTbl)
  model <- naiveBayes(tdata.age[,-1], tdata.age$age)
  
  saveRDS(model,paste(modelsPath,"ageBayesModel.rds", sep = ""))
  
  return (model)
}

#classifies a user given file path of user's post and a predictive model
classifyPost <- function(filePath, model) {
  post <- scan(filePath, what = "character", sep = "\n")
  post <- gsub("[[:punct:]]", " ", post)
  post <- tolower(post)
  
  wordsBag <- scan(bagOfWordsPath, what = "character", sep="\n")
  
  counts <- countWordsInVector(wordsBag, post)
  countLst <- as.list(counts)
  names(countLst) <- wordsBag
  
  return (predict(model, countLst))
}

####################_MAIN PART OF SCRIPT_####################

library(e1071)
user.profiles <- readUserProfiles()[1:trainingLength, ]

frequencyTable <- createFrequencyTable(user.profiles)

genderModel <- createGenderModel(frequencyTable)
ageModel <- createAgeModel(frequencyTable)

####################_TEST AREA_####################
test.users <- readUserProfiles()[9000:9010, ]
for(i in 1:2) {
  test.user <- test.users[i, ]
  print(paste(test.user$age, ",", test.user$gender))
  path <- paste(postsPath, test.user$userid, ".txt", sep = "")
  print(classifyPost(path, genderModel))
  print(classifyPost(path, ageModel))
}


