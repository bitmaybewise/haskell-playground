#!/bin/bash
set -aux
curl --request GET \
     --url "https://api.themoviedb.org/3/movie/980489?api_key=$TMDB_API_KEY"
