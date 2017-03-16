library(tidyverse)
library(httr)
library(stringr)

## Search for artists by name
get_artists <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = str_replace(res[[x]]$uri, 'spotify:artist:', ''), # remove meta info from the uri string
            artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA)
        )
    })
    
    return(artists)
}

# artist_info <- get_artists('radiohead')

# str(artist_info)

# Filter out other artist matches
# artist_info <- artist_info %>% 
#     filter(artist_name == 'Radiohead')

## Get artist albums
library(lubridate)

get_albums <- function(artist_uri, lim = 50, off = 0) {
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums?album_type=album&limit=',lim,'&offset=',off)) %>% content
    
    map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = str_replace(tmp$uri, 'spotify:album:', ''),
                       album_name = str_replace_all(tmp$name, '\'', ''),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', str_replace(tmp$uri, 'spotify:album:', ''))) %>% content %>% .$release_date, # you need a separate call to on "albums" to get release date.
                       album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                )
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_year)
}

# album_info <- get_albums(artist_info$artist_uri)

# str(album_info)

# Filter out remixes and EPs
# non_studio_albums <- c('TKOL RMX 1234567', 'In Rainbows Disk 2', 'Com Lag: 2+2=5', 'I Might Be Wrong')
# album_info <- filter(album_info, !album_name %in% non_studio_albums)

## Get tracks
get_tracks <- function(artist_info, album_info, id, secret) {
    
    client_id <- id
    client_secret <- secret
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
            content %>% 
            .$items 
        
        uris <- map(1:length(tracks), function(z) {
            gsub('spotify:track:', '', tracks[z][[1]]$uri)
        }) %>% unlist %>% paste0(collapse=',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        df <- unlist(res) %>% 
            matrix(nrow = length(res), byrow = T) %>% 
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df <- df %>% 
            mutate(album_uri = x,
                   track_number = row_number()) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$name) %>%
            ungroup %>% 
            left_join(album_info, by = 'album_uri') %>% 
            rename(track_uri = id) %>% 
            select(-c(type, track_href, analysis_url, uri))
        return(df)
    }) %>%
        mutate(artist_img = artist_info$artist_img) %>% 
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
    return(track_info)
}


## Get tracks
# get_playlist <- function(user_id, playlist_id, id, secret) { 
#   
#   client_id <- id
#   client_secret <- secret
#   access_token <- POST('https://accounts.spotify.com/api/token',
#                        accept_json(), authenticate(client_id, client_secret),
#                        body = list(grant_type='client_credentials'),
#                        encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
#   
#   tracks <- GET(paste0('https://api.spotify.com/v1/users/', user_id,'/playlists/', playlist_id, '/tracks?fields=tracks.items(track(name,href,album(name,href)))'),
#                 query = list(access_token = access_token)) %>% 
#       
#     
#     
# 
# 
#   return(df)
# })


  # spotify_df <- get_tracks(artist_info, album_info)

# str(spotify_df)

