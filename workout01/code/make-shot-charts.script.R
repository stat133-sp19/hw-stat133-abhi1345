#Shot Chart Generator Script

court <- "images/court.jpg"

library(ggplot2, jpeg, grid)

court_image <- rasterGrob(
  readJPEG(court),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

inputfiles <- c('data/andre-iguodala.csv', 'data/stephen-curry.csv', 'data/kevin-durant.csv', 'data/klay-thompson.csv', 'data/draymond-green.csv')
pdfnames <- c("images/andre-iguodala-shot-chart.pdf", "images/stephen-curry-shot-chart.pdf", "images/kevin-durant-shot-chart.pdf", "images/klay-thompson-shot-chart.pdf", "images/draymond-green-shot-chart.pdf")
names <- c("Andre Iguodala", "Stephen Curry", "Kevin Durant", "Klay Thompson", "Draymond Green")

i = 0
for (file in inputfiles) {
  i = i + 1
  data <- read.csv(file = file, stringsAsFactors = FALSE)
  title <- c('Shot Chart: ',  names[i], ' (2016 season)')
  plot <- ggplot(data = data) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
    ggtitle(paste(title, collapse = "")) + theme_minimal()
  ggsave(filename = pdfnames[i], plot = plot, width = 6.5, height = 5)
}

#Facet Graphs

andre_data <- read.csv(file = 'data/andre-iguodala.csv', stringsAsFactors = FALSE)
steph_data <- read.csv(file = 'data/stephen-curry.csv', stringsAsFactors = FALSE)
kevin_data <- read.csv(file = 'data/kevin-durant.csv', stringsAsFactors = FALSE)
klay_data <- read.csv(file = 'data/klay-thompson.csv', stringsAsFactors = FALSE)
dray_data <- read.csv(file = 'data/draymond-green.csv', stringsAsFactors = FALSE)
nba_data <- read.csv(file = "data/nba-players-2018.csv", stringsAsFactors = FALSE)

stacked <- rbind(andre_data, steph_data, kevin_data, klay_data, dray_data)

faceted <- ggplot(data = stacked) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("GSW Shot Charts 2016") + theme_minimal() + facet_wrap(vars(name))

ggsave(filename = "images/gsw-shot-charts.pdf", plot = faceted, width = 8, height = 7)
ggsave(filename = "images/gsw-shot-charts.png", plot = faceted, width = 8, height = 7)