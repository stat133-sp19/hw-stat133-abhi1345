# R Data Generation Script
# This script takes in
#input(s): what are the inputs required by the script?
#and returns output(s): what are the outputs created when running the script?

andre_data <- read.csv(file = 'data/andre-iguodala.csv', stringsAsFactors = FALSE)
steph_data <- read.csv(file = 'data/stephen-curry.csv', stringsAsFactors = FALSE)
kevin_data <- read.csv(file = 'data/kevin-durant.csv', stringsAsFactors = FALSE)
klay_data <- read.csv(file = 'data/klay-thompson.csv', stringsAsFactors = FALSE)
dray_data <- read.csv(file = 'data/draymond-green.csv', stringsAsFactors = FALSE)
nba_data <- read.csv(file = "data/nba-players-2018.csv", stringsAsFactors = FALSE)

inputfiles <- c('data/andre-iguodala.csv', 'data/stephen-curry.csv', 'data/kevin-durant.csv', 'data/klay-thompson.csv', 'data/draymond-green.csv')

andre_data$name = "Andre Iguodala"
steph_data$name = "Stephen Curry"
kevin_data$name = "Kevin Durant"
klay_data$name = "Klay Thompson"
dray_data$name = "Draymond Green"

print(class(andre_data))

filenames = c("output/andre-iguodala-summary.txt", "output/stephen-curry-summary.txt", "output/kevin-durant-summary.txt", "output/klay-thompson-summary.txt","output/draymond-green-summary.txt")
outputfiles = list()

i = 0
for (table in list(andre_data, steph_data, kevin_data, klay_data, dray_data)) {
  i = i + 1
  t1 <- table$shot_made_flag == "y"
  t2 <- table$shot_made_flag == "n"
  table$shot_made_flag[t1] <- "shot_yes"
  table$shot_made_flag[t2] <- "shot_no"
  table$minute <- table$period*12 - table$minutes_remaining
  sink(filenames[i])
  print(summary(table))
  sink()
  outputfiles[[i]] = table
  write.csv(table, file = inputfiles[i])
}

print(outputfiles[1])
stacked <- rbind(outputfiles[[1]], outputfiles[[2]], outputfiles[[3]], outputfiles[[4]], outputfiles[[5]])
write.csv(stacked, file = "data/shots-data.csv")

sink("output/shots-data-summary.txt")
summary(stacked)
sink()
