#Data cleaning assessment
#210928972

install.packages('tidyr')
install.packages('dpylr')

library(tidyr)
library(dplyr)

#COMPLETED CODE
spotify_data <- read.table(file = "Spotify_Messy_210928972.txt",
                           header = T,
                           dec = ".",
                           sep = "\t",
                           na.strings = NA) %>%
  rename('track_name' = 'ZZtrack_name89') %>%
  mutate(mode = as.numeric(gsub('[^0-9.-]','',mode))) %>%
  separate_wider_delim("danceability.energy",
                       names = c('danceability','energy'),
                       delim = '_') %>%
  mutate(energy = as.numeric(energy)) %>%
  mutate(danceability = as.numeric(danceability)) %>%
  pivot_longer(cols = c('rap','rock','latin','r.b'),
               names_to = 'playlist_genre', values_to = 'playlist_subgenre') %>%
  filter(!is.na(playlist_subgenre)) %>%
  relocate(any_of(c("playlist_genre","playlist_subgenre")),.after = 'playlist_id') %>% 
  mutate(playlist_genre = gsub('r.b','r&b',playlist_genre)) %>%
  mutate(track_album_release_date = gsub('0[.]5-','2020-',track_album_release_date)) %>%
  mutate(track_artist = gsub('The Fourty Owls','The Four Owls',track_artist)) %>%
  mutate(instrumentalness = ifelse(instrumentalness > 1, NA, instrumentalness)) %>%
  mutate(average_danceability_scores = mean(danceability, na.rm = TRUE))%>%
  mutate(playlist_name = as.factor(playlist_name))


#STEP-BY-STEP OF THE CODE USED
#[1] Read the file using read.table()
spotify_data <- read.table(file = "Spotify_Messy_210928972.txt",
                           header = T, #there are column headers present
                           dec = ".", #there are decimal points in the txt file
                           sep = '\t', #the file is tab-separated
                           na.strings = NA) #this ensures that the gaps in the dataframe are filled with NA to make it easier to reference later down the line 

#[2] Rename 'ZZtrack_name89' to 'track name' using rename() function
spotify_data <- spotify_data %>% 
  rename('track_name' = 'ZZtrack_name89') 

#[3] Change 'mode' into a numeric type and remove the additional characters
unique(spotify_data$mode)
#0 1 1Q 0Q - mode contains unwanted characters of 'Q'

spotify_data <- spotify_data %>% mutate(mode = as.numeric(gsub('[^0-9.-]','',mode)))
#this substitutes characters that are not numerical with a space ('')
#as.numeric() is used to convert mode from character string to a numerical string
#mutate() is used to modify the existing column of 'mode'

str(spotify_data$mode)
#the mode variable is now a numerical string

#[4]Separate danceability and energy using seperate_wider_delim
spotify_data <- spotify_data %>% separate_wider_delim("danceability.energy",
                                                      names = c('danceability','energy'),
                                                      delim = '_') 
#separate_wider_delim uses the current names in the column to produce two new columns
#delim is the character that separates the two words, in this case 'danceability' and 'energy' is separated by an underscore ('_')

#[5] Convert danceability and energy into numeric types using as.numeric() and mutate()
str(spotify_data$energy)
#energy is a character string
str(spotify_data$danceability)
#danceability is a character string

spotify_data <- spotify_data %>% 
  mutate(energy = as.numeric(energy)) %>% 
  mutate(danceability = as.numeric(danceability))

str(spotify_data$energy)
#numeric
str(spotify_data$danceability)
#numeric

#[6] Create a 'playlist_genre' and 'playlist_subgenre' variable using pivot_longer
spotify_data <- pivot_longer(spotify_data, cols = c('rap','rock','latin','r.b'),
                             names_to = 'playlist_genre', values_to = 'playlist_subgenre')
#I specified the columsn that I would like to move to the new column of 'playlist_genre', their corresponding values will be placed in a column called 'playlist_subgenre'

#[6.1] Removing excess rows that result in NA in 'playlist subgenre' using filter() and !
spotify_data <- spotify_data %>% filter(!is.na(playlist_subgenre))
#this filters out the values that are NA, i.e. '!' means to negate these values in the 'playlist_subgenre' variable

#[6.2] Changing the location of "playlist genre" and "playlist subgenre" to be next to the other character types
spotify_data <- spotify_data %>% relocate(any_of(c("playlist_genre","playlist_subgenre")),.after = 'playlist_id')
#Using relocate() I changed the location of the columns for 'playlist genre' and 'playlist subgenre'
#'any_of' tells R to choose these columns

#[6.3] Using mutate() replace r.b with r&b in the 'playlist_genre' column
spotify_data <- spotify_data %>% mutate(playlist_genre = gsub('r.b','r&b',playlist_genre))
#here i specified the old name as 'r.b' and the new name is 'r&b'
#this will occur in the 'playlist_genre' column

#[7] Using mutate() change the wrong values
spotify_data <- spotify_data %>% mutate(track_album_release_date = gsub('0[.]5-','2020-',track_album_release_date)) %>%
  #because it is classed as a character i used gsub to substitute the values of 0.5 to 2020
  #as there are dates which also have '0' and '5', i.e. 2005, i had to specify that there was a decimal point in between the 0 and 5 and that the numbers were followed by '-'
  mutate(track_artist = gsub('The Fourty Owls','The Four Owls',track_artist))

#[8] Change the values greater than 1 to become NA in instrumentalness using mutate()  
spotify_data <- spotify_data %>% mutate(instrumentalness = ifelse(instrumentalness > 1, NA, instrumentalness))
#i used the ifelse() argument which meant that if the value was true for the argument of '>1' than the value would be replaced with NA

#[9] Finding out the number of playlists and changing 'playlist_name' from character to 'factor'
number_of_playlists <- unique(spotify_data$playlist_name)
length(number_of_playlists)
#45

spotify_data <- spotify_data %>% 
  mutate('average_danceability_scores' = mean(danceability, na.rm = TRUE))%>%
  mutate(playlist_name = as.factor(playlist_name))
#na.rm = TRUE means that there are NA's present, to prevent any errors
#i created a new column using mutate() called 'average_danceability_scores'
#i made the 'playlist_name' column in a factor using as.factor

levels(spotify_data$playlist_name)

#[8] Write the clean data into a suitable format
write.table(spotify_data, 'Spotify_Clean_210928972.txt', 
            sep = '\t',
            col.names = T,
            row.names = F,
            quote = F)








