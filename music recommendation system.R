# Load necessary libraries
library(tidyverse)
library(ggplot2)
# Read the dataset
spotify_data <- read.csv("Spotify-2000.csv", header = TRUE, stringsAsFactors = FALSE)


#------------>Objective 1: Recommend top music artists<-------------------------
# Aggregate the data by artists and calculate the total number of plays

artists_play_count <- spotify_data %>% 
  group_by(Artist) %>% 
  summarise(total_plays = sum(PlayCount)) %>%
  arrange(desc(total_plays)) %>% 
  top_n(10, total_plays)

# Print the recommended artists
cat("Recommended Artists:\n")
artists_play_count$Artist
# Bar chart for top recommended artists

chart<- ggplot(artists_play_count, aes(x = reorder(Artist, total_plays), y = total_plays, 
                                       text = paste("PlayCount: ", total_plays))) +
  geom_col(fill = "lightgreen", width = 0.5) +
  ggtitle("Top Recommended Artists") +
  xlab("Artist") +
  ylab("Total Plays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  coord_flip()
ggplotly(chart, tooltip = "text")

#----------------------------->Objective 2: <------------------------------------------
#Recommend top tracks based on user's favorite artists
# Get user input on their favorite artists
# Request user input for favorite artists
favorite_artists <- strsplit(readline(prompt = "Enter your favorite artist (ENTER ONE ARTIST ONLY): "), ",")[[1]]


# Filter data based on user's favorite artists
filtered_data <- spotify_data %>%
  filter(Artist %in% favorite_artists)

# Group data by track name and calculate total play count
grouped_data <- filtered_data %>%
  group_by(Track.Name) %>%
  summarise(total_plays = sum(PlayCount))

# Arrange data in descending order of total play count
arranged_data <- grouped_data %>%
  arrange(desc(total_plays))

# Recommend top tracks based on total play count
top_tracks <- arranged_data %>%
  top_n(10)

# Print recommended top tracks
cat("\nTop recommended tracks based on your favorite artists:\n")
print(top_tracks)

# Bar Chart for top recommended tracks based on user's favorite artists
ggplot(top_tracks, aes(x = Track.Name, y = total_plays)) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  labs(title = "Top Recommended Tracks Based on Favorite Artists",
       x = "Track Name", y = "Total Plays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------------->Objective 3: Recommend top genres<-----------------------
# Get the top genres based on the total number of plays for each genre
genre_play_count <- spotify_data %>% 
  group_by(Top_Genre) %>% 
  summarise(total_plays = sum(`PlayCount`)) %>% 
  arrange(desc(total_plays)) %>% 
  top_n(10, total_plays)

# Print the recommended genres
cat("\nRecommended Genres:\n")
genre_play_count$Top_Genre


#Bar chart
ggplot(data = genre_play_count, aes(x = reorder(Top_Genre, total_plays), y = total_plays)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Recommended Genres by Total Plays",
       x = "Genre",
       y = "Total Plays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
#----->Objective 4: Recommend most popular Songs of year by number of plays<----
# Input year from user
user_year <- as.integer(readline(prompt = "Enter a year: "))

# Filter data for the input year
filtered_data <- spotify_data %>% 
  filter(Year == user_year)

# Check if any data exists for the input year
if (nrow(filtered_data) == 0) {
  cat("No data available for the input year.")
} else {
  # Group by song and calculate total play count
  song_play_count <- filtered_data %>% 
    group_by(Track.Name) %>% 
    summarise(total_play = sum(PlayCount)) %>% 
    arrange(desc(total_play))
  
  # Print songs with highest play count
  cat("\nSongs with highest play count for year", user_year, ":\n")
  print(song_play_count)
}

# Bar chart for songs with highest play count for a specific year
ggplot(song_play_count, aes(x = reorder(Track.Name, total_play), y = total_play)) +
  geom_col(fill = "lightblue", width = 0.5) +
  ggtitle(paste("Songs with Highest Play Count for Year", user_year)) +
  xlab("Track Name") +
  ylab("Total Plays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  coord_flip()

#---------->Objective 5: Recommend top tracks by recommended artists<-----------
# Get the top tracks for the recommended artists
# Request user input for recommended artist
recommended_artist <- readline(prompt = "Enter a recommended artist: ")

# Filter data based on recommended artist
filtered_data <- spotify_data %>%
  filter(Artist == recommended_artist)

# Group data by track name and calculate total play count
grouped_data <- filtered_data %>%
  group_by(Track.Name) %>%
  summarise(total_plays = sum(PlayCount))

# Arrange data in descending order of total play count
arranged_data <- grouped_data %>%
  arrange(desc(total_plays))

# Recommend top tracks based on total play count
top_tracks <- arranged_data %>%
  head(10)

# Print recommended top tracks
cat("\nTop recommended tracks by", recommended_artist, ":\n")
print(top_tracks)

# Graph showing top tracks by artist
ggplot(top_tracks, aes(x = total_plays, y = reorder(Track.Name, total_plays), fill = Track.Name)) +
  geom_col() +
  ggtitle(paste("Top Recommended Tracks by", recommended_artist)) +
  xlab("Total Plays") +
  ylab("Track Name") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(name = "Track Name", labels = top_tracks$Track.Name)

