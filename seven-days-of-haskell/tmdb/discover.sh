#!/bin/bash
set -aux
curl --request GET \
     --url 'https://api.themoviedb.org/3/discover/movie?include_adult=false&include_video=false&language=en-US&page=1&sort_by=popularity.desc' \
     --header "Authorization: Bearer $TMDB_API_READ_ACCESS_TOKEN" \
     --header 'accept: application/json'
