##################################
### Star Realms EDA / Analysis ###
##################################

# # # File Description
# # # Code to parse log and store in dataframe

### Loading Packages
library(tidyverse)

### Setting Options
options(dplyr.print_max = 1e9)

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Reading in Data
sr <- read.csv("star_realms.csv")
dim(sr)
sr %>% head
x <- sr$Log[1]

cat(sr$Log[1])
cat(y[1])

# scan(text=   # use scan to separate after insertion of commas
#        gsub("It is now ", "~It is now",   
#             x) ,  
#      what="", sep="~") 

### Dividing into turns
y <- unlist(strsplit(gsub("It is now", "~It is now",   
                          x),split = "~"))

### Extracting who's turn it is
names <- (str_extract_all(string = y, pattern = "(?<=It is now ).*?(?='s turn)"))
player1 <- names[[1]][1] <- names[[3]][1]
player2 <- names[[2]][1]

### Extracting turn number
turn <- 1:length(names)

### Extracting which cards were acquired 
acquired_pre <- (str_extract_all(string = y, pattern = "(?<=Acquired ).*?(?=\n)"))
# acquired <- 

# do.call(rbind.data.frame, acquired_pre)

acquired <- lapply(str_extract_all, X = acquired_pre, pattern = "(?<=\\>).*?(?=\\<)")
acquired <- lapply(acquired, function(x) unlist(x))

acquired_color <- lapply(str_extract_all, X = acquired_pre, pattern = "(?<=\\=).*?(?=\\>)")
conv_colors <- c("#FFFF00" = "Yellow", "#FF0000" = "Red", "#4CC417" = "Green", "#1589FF" = "Blue", "#800080" = "Gray - Unaligned")
acquired_color <- lapply(acquired_color, function(x) conv_colors[unlist(x)])

named_acquired <- lapply(turn, function(x) setNames({if(length(acquired[[x]])>0) acquired[[x]] else character(0)}, {if(length(acquired[[x]])>0) acquired_color[[x]] }))

### Extracting what cards were scrapped
scrapped <- lapply(str_extract_all, X = str_extract_all(string = y, pattern = "(?<=Scrapped ).*?(?=\n)"), pattern = "(?<=\\>).*?(?=\\<)")
scrapped <- lapply(scrapped, function(x) {if(length(unlist(x)>0)) unlist(x) else character(0)})
### Ending Turn Health
# If attacking, will say (New Authority:)
# opp_authority <- lapply(tail, X = str_extract_all(string = y, pattern = "(?<=New Authority:)[[:digit:]]*?(?=\\))"), n = 1)
# 
# # If healing, will just say Authority:
# own_authority <- lapply(tail, X = str_extract_all(string = y, pattern = "(?<=\\(Authority:)[[:digit:]]*(?=\\))"), n = 1)

# lines w authority
lines_w_authority <-  str_extract_all(string = y, pattern = "(?<=\n).*?Authority.*(?=\n)")
player1_authority <- lapply(str_extract, X = lapply(tail, X = lapply(grep, X = lines_w_authority, pattern = player1, value = TRUE), n = 1), pattern = "(?<=Authority:)[\\-[[:digit:]]]*?(?=\\))")

player1_authority <- sapply(player1_authority, function(x) ifelse(length(x) == 0, 999, as.numeric(x)))
player1_authority[1] <- min(player1_authority[1], 50)
while(sum(player1_authority == 999)>0) player1_authority[which(player1_authority == 999)] <- player1_authority[which(player1_authority == 999)-1]

player2_authority <- lapply(str_extract, X = lapply(tail, X = lapply(grep, X = lines_w_authority, pattern = player2, value = TRUE), n = 1), pattern = "(?<=Authority:)[\\-[[:digit:]]]*?(?=\\))")
player2_authority <- sapply(player2_authority, function(x) ifelse(length(x) == 0, 999, as.numeric(x)))
player2_authority[1] <- min(player2_authority[1], 50)
while(sum(player2_authority == 999)>0) player2_authority[which(player2_authority == 999)] <- player2_authority[which(player2_authority == 999)-1]

### Trade gained during turn +x Trade
trade_gained <- sapply((str_extract_all(string = y, pattern = "(?<=\\+).*?(?= Trade)")), \(x) sum(as.numeric(x)))

### Combat gained during turn +x Combat
combat_gained <- sapply((str_extract_all(string = y, pattern = "(?<=\\+).*?(?= Combat)")), \(x) sum(as.numeric(x)))

### Authority gained during turn +x Authority
authority_gained <- sapply((str_extract_all(string = y, pattern = "(?<=\\+).*?(?= Authority)")), \(x) sum(as.numeric(x)))

length(trade_gained)
length(combat_gained)
length(authority_gained)
### Cards in deck
# # # For now I am going to assume just the starting handsize of 10. Will want to address nuanced games in the future
# cumsum(Starting, )
starting <- deck_size <- numeric(length(turn))
starting[1:2] <- 10
acquired_num <- sapply(acquired, length)
scrapped_num <- sapply(scrapped, length)

turns1 <- seq(1, length(turn), by = 2)
deck_size[turns1] <- cumsum(starting[turns1] + acquired_num[turns1] - scrapped_num[turns1])

turns2 <- seq(2, length(turn), by = 2)
deck_size[turns2] <- cumsum(starting[turns2] + acquired_num[turns2] - scrapped_num[turns2])


# Data frame
# # # Adding Turn Number, Player, Authority
turn_df <- data.frame("Turn" = turn, "Whose" = paste0(rep("Player", length(turn)), 1:2), "Player1" = player1_authority, "Player2" = player2_authority)

# # # Adding Purchased Cards
turn_df$purchased <- named_acquired
turn_df$purchased_factions <- acquired_color
turn_df$purchased_num <- acquired_num
turn_df$purchased_num_c[turns1] <- cumsum(acquired_num[turns1])
turn_df$purchased_num_c[turns2] <- cumsum(acquired_num[turns2])

# # # Adding Scrapped Cards
turn_df$scrapped <- scrapped
turn_df$scrapped_num <- scrapped_num
turn_df$scrapped_num_c[turns1] <- cumsum(scrapped_num[turns1])
turn_df$scrapped_num_c[turns2] <- cumsum(scrapped_num[turns2])

# # # Adding Deck Size
turn_df$deck_size <- deck_size

# # # Adding gained trade, combat, authority
turn_df$trade_gained <- trade_gained
turn_df$combat_gained <- combat_gained
turn_df$authority_gained <- authority_gained


qdapTools::mtabulate(turn_df$purchased_factions)

# Plotting
turn_df_long <- turn_df %>% pivot_longer(`Player1`:`Player2`, values_to = "Authority")

rect <- data.frame(xstart = turn-0.5, xend = turn+0.5, col = turn_df$Whose)

ggplot() + 
  geom_line(turn_df_long, mapping = aes(x = Turn, y = Authority, group = name, color = name), size = 1) + 
  geom_rect(data = rect, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = .1) + 
  theme_bw() + 
  guides(fill = "none") + 
  labs(color = "Player")







### Impact of factions
### Impact of number of factions
### Impact of scrapping
### Impact of discarding
### Impact of authority
### Impact of trade
### Impact of combat
### Impact of level (if possible)
### Impact of leaving trade unused
### Size of deck
