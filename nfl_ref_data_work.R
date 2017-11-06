packages <- c("dplyr", "tidyr", "XML")
install.packages(packages)
packages_2 <- c("xml2", "rvest")
install.packages(packages_2)
library(dplyr)
library(tidyr)
library(XML)
library(rvest)

packrat::snapshot()
packrat::clean()
packrat::on()

getwd()

url="https://operations.nfl.com/the-officials/these-officials-are-really-good/2017-roster-of-nfl-officials"

tables <- getNodeSet(htmlParse(url), "//tbody") 
# Parses html and find all rows (nodes) in the table
# Make a base table from the frist page  
DBuildTest <<- readHTMLTable(tables[[1]], header = T, trim = TRUE
                             , stringsAsFactors = FALSE) # %>% mutate(TEAM = "PHI")

###

RR_URL <- "https://operations.nfl.com/the-officials/these-officials-are-really-good/2017-roster-of-nfl-officials"

RR_Webpage <- RR_URL %>%
  xml2::read_html()

RR_Sample <- RR_Webpage %>%
  rvest::html_nodes(xpath = '//tbody') %>%
  rvest::html_attr("src") %>%
  data.frame()

RR_Sample <- RR_Sample %>%
  rvest::html_nodes(xpath = '//*[(@id = "pageswitcher-content")]') %>%
  rvest::html_attr("src") %>%
  data.frame()

###

refs_web <- read_html("https://operations.nfl.com/the-officials/these-officials-are-really-good/2017-roster-of-nfl-officials")

refs_web %>%
  html_node("tbody") %>%
  html_text() %>%
  as.numeric()

refs_web %>%
  html_nodes("td") %>%
  html_text() -> ref_data 

install.packages("data.table") # didn't use this here
library(data.table)            # didn't use this here

ref_data <- ref_data %>% data.frame(stringsAsFactors = F)
ref_data <- as.character(ref_data)

feeder_table <- c(1:750) # This is just a feeder table

ref_data_table <- ref_data[1:6,] %>% data.frame(stringsAsFactors = F)
nums <- c(1:6)
ref_data_table <- cbind(ref_data_table, nums)
names(ref_data_table)[1] <- "column_1"
names(ref_data_table)[2] <- "column_2"
ref_data_table <- spread(data = ref_data_table, key = column_2, value = "column_1")
ref_data_table[1,1:6]
names(ref_data_table)[1:6] <- ref_data_table[1,1:6]
rm(x)

for(i in seq(from=1, to=750, by=6)){
  # i <- 7
  x <- ref_data[i:(i+5),] %>% data.frame(stringsAsFactors = F)
  x <- cbind(x, nums)
  names(x)[1] <- "column_1"
  names(x)[2] <- "column_2"
  x <- spread(data = x, key = column_2, value = "column_1")
  
names(x)[1:6] <- ref_data_table[1,1:6]
  
  ref_data_table <- bind_rows(ref_data_table,x)
}
# 1:6
# 7:12

###

years <- c(2010:2017) %>% data.frame(stringsAsFactors = F)
years[j,1]
i <- 1
j <- 1

url <- paste0("http://www.nflpenalties.com/team/", NFL_Teams[i,1], "?year=", years[j,1])
#url <- url %>% data.frame(stringsAsFactors = F)


 for(i in 1:32){
   for(j in 1:8){
  # i <- 1
  # j <- 1
   url <- paste0("http://www.nflpenalties.com/team/", NFL_Teams[i,1], "?year=", years[j,1])
  # print(i)
  # print(j)
   url <- url %>% data.frame(stringsAsFactors = F)
   url$Team <- NFL_Teams[i,2]
   url$Year <- years[j,1]
   url_table <- bind_rows(url_table, url) %>% distinct()
   
   # url_table <- url
   # url_table <- url_table %>% data.frame(stringsAsFactors = F)
}}
url_table[256,1]


penalty_data <- read_html(url[1,1])

penalty_data %>%          # this is just used to get some meta data about the website
  html_node("tbody") %>%
  html_text() %>%
  as.numeric()

penalty_data %>%
  html_nodes("td") %>%
  html_text() -> penalty_data 

penalty_data <- penalty_data %>% data.frame(stringsAsFactors = F)


penalty_data %>% html_nodes("tr") %>% # I used this to extract the column names
  html_text()

header <- c("Opponent", "Date", "Week", "Outcome", "Total Count"
            , "Total Yards", "Off Count", "Off Yards", "Def Count"
            , "Def Yards", "ST Count" , "ST Yards")

penalty_data_table <- penalty_data[1:12,] %>% data.frame(stringsAsFactors = F)
nums_2 <- c(1:12)
penalty_data_table <- cbind(penalty_data_table, nums_2)
names(penalty_data_table)[1] <- "column_1"
names(penalty_data_table)[2] <- "column_2"
penalty_data_table <- spread(data = penalty_data_table, key = column_2, value = "column_1")
penalty_data_table[1,1:12]
names(penalty_data_table)[1:12] <- header
penalty_data_table$Year <- url[1,3]
penalty_data_table$Team <- url[1,2]



for(j in 1:256){ # url_table
    # for(i in seq(1, )) # penalty_data
       #j <- 1
       #i <- 1
       penalty_data <- read_html(url_table[j,1])
       penalty_data %>%
         html_nodes("td") %>%
         html_text() -> penalty_data 
       
       penalty_data <- penalty_data %>% data.frame(stringsAsFactors = F)
       
       number <- nrow(penalty_data)
for(i in seq(1, number, 12)){
  #i <- 1
  y <- penalty_data[i:(i+11),] %>% data.frame(stringsAsFactors = F)
  # nums_2 <- c(1:12)
  y <- cbind(y, nums_2)
  names(y)[1] <- "column_1"
  names(y)[2] <- "column_2"
  y <- spread(data = y, key = column_2, value = "column_1")
  # penalty_data_table[1,1:12]
  names(y)[1:12] <- header
  y$Year <- url_table[j,3]
  y$Team <- url_table[j,2]
  penalty_data_table <- bind_rows(penalty_data_table,y)
  print(i)
}
       print(j)
}



###

url_2 <- "http://www.nfl.com/schedules/2010/REG/EAGLES"
webpage <- read_html(url_2)

data_html <- html_nodes(webpage,'.list-matchup-row-date')
game_date_data <- html_text(data_html)
game_date_data <- data.frame(game_date_data, stringsAsFactors = F)
game_date_data <- game_date_data %>% filter(game_date_data != "Bye Week")
game_date_data

data_html <- html_nodes(webpage,'.at , .win , .lost')
game_game_data <- html_text(data_html)
game_game_data <- data.frame(game_game_data, stringsAsFactors = F)
game_game_data

game_game_data$id <- 1:6
game_game_data$id <- as.character(game_game_data$id)
game_game_data$game_game_data <- as.character(game_game_data$game_game_data)
names(game_game_data)[1] <- "data"

i <- 1
x <- game_game_data %>% filter(id == i) %>% select(1)
new_data <- x

for (i in 2:6) {
  #i <- 1
  x <- game_game_data %>% filter(id == i) %>% select(1)
  #new_data <- x
  new_data <- bind_cols(new_data,x)
}

game_date_data <- game_date_data %>% filter(game_date_data != "Bye Week")
new_data <- bind_cols(new_data,game_date_data)
new_data <- new_data %>% select(-c(1,3,4))
# game_data <- spread(data = x, value = "data", key = "id")

new_data$data4 = substr(new_data$data4,1,nchar(new_data$data4)-3)
new_data$data5 = substr(new_data$data5,1,nchar(new_data$data5)-3)

new_data$game_date_data <- as.character(new_data$game_date_data)

new_data$game_month <- substr(new_data$game_date_data, 1
                             , nchar(new_data$game_date_data)-2)
new_data$game_day <- substr(new_data$game_date_data, 4
                              , nchar(new_data$game_date_data)+2)


new_data <- left_join(new_data, just_months, by = c("game_date" = "month_abrv"))
new_data$game_date <- paste(new_data$month_num, new_data$game_day, sep = "-")

#justs
{

just_months <- data.frame(month_abrv = c("Sep", "Oct", "Nov", "Dec", "Jan")
                          , month_num = c("09", "10", "11", "12", "01")
                          , stringsAsFactors = F)
  

  
just_teams <- data.frame(team_names = c("redskins", "giants", "cowboys", "patriots"
                , "eagles", "stealers", "chiefs", "raiders", "texans", "dolphins"
                , "ravens", "browns" , "buccaneers", "bengals", "49ers" , "seahawks"
                , "rams", "chargers", "lions", "bills", "jets", "panthers", "jaguars"
                , "vickings", "packers", "saints", "falcons", "cardinals", "bears"
                , "titans", "colts", "broncos")
                , team_locals = c("washington", "new-york", "dallas", "new-england"
                                  , "philadelphia", "pittsburgh", "kansas-city"
                                  , "oakland", "houston", "miami", "baltimore", "cleveland"
                                  ,  "tampa-bay", "cincinnati", "san-francisco", "seatle"
                                  , "los-angeles", "los-angeles", "detroit", "buffalo"
                                  , "new-york", "carolina", "jacksonville", "minnesota"
                                  , "green-bay", "new-orleans", "atlanta", "arizona"
                                  , "chicago", "tennessee", "indiana", "denver")
                , stringsAsFactors = F)


just_years <- data.frame(2010:2017, stringsAsFactors = F)
just_years <- data_frame(just_years)

for (i in 1:8) {
  for (j in 1:32) {
    
#    i <- 1
#    j <- 1
x <- paste0("http://www.nfl.com/schedules/",just_years[i,1],"/REG/",just_teams[j,1]) %>% 
  data.frame(stringsAsFactors = F) 
# urls <- x
    
urls <- bind_rows(urls,x)
  }
} # makes >> "urls"

}



# First, I need to get all the meta data ready to make my ultimate url with, which looks a
# little like this >> 
# "http://www.nflpenalties.com/game/new-york-giants-at-philadelphia-eagles-09-24-2017"
# to do this I need a for loop that pulls in the teams in the games of the dates they 
# occurred.  Getting this data requires -- first -- getting the game data from urls of
# this type >> "http://www.nfl.com/schedules/2010/REG/eagles"
# thus I need >> "http://www.nfl.com/schedules/"
  # >> "[year]/"
    # >> "REG/"
      # >> "[team]"










                                            