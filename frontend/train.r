folder <- c("data","output")

for(f in folder){
  if(!dir.exists(f)){
    dir.create(f)
  }
  else{
    print("folder alread exist")
  }
}

data <- "data/students.csv"

df <- read.csv(data)

StringsAsFactors=FALSE
na.strings = c("","NA","Missing","missing","unknown")

colnames(df)


str(df)

summary(df)

head(df)

tail(df)

colSums(is.na(df))

df$Gender <- trimws(df$Gender)

df$Attendance... <- as.numeric(df$Attendance...)
df$Study.hours.week <- as.numeric(df$Study.hours.week)
df$Previous.score <- as.numeric(df$Previous.score)

str(df)

df$Programme <- trimws(df$Programme)
df$Final.result <- trimws(df$Final.result)

df

df$Gender[df$Gender == "M"] <- "Male"
df$Gender[df$Gender == "F"] <- "Female"

df$Gender

df$Attendance...[is.na(df$Attendance...)] <- median(df$Attendance..., na.rm = TRUE)

df$Study.hours.week[is.na(df$Study.hours.week)] <- median(df$Study.hours.week, na.rm = TRUE)

df


colSums(is.na(df))
