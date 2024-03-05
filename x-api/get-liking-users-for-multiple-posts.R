# https://developer.twitter.com/en/docs/twitter-api/tweets/likes/api-reference/get-tweets-id-liking_users
# 
# https://twitter.com/Tesla/status/1756741790890131930

source("00-source.R")

here("data", today(), "post") |> 
   read_rds() -> 
   post

source("clean-post.R")

post |> 
   arrange(created_at) |> 
   filter(post_type != "Repost") ->
   post

post_id <- pull(post, post_id)

# post_id <- "1756741790890131930"
url_handle <- glue("https://api.twitter.com/2/tweets/{post_id}/liking_users")
like <- NULL
next_token <- "start"
i <- 1
loop_vector <- seq_along(url_handle)

for(j in loop_vector) {
   
   while(!is.null(next_token)) {
      
      params <- list(
         user.fields =
            paste(
               "created_at",
               "username",
               "name",
               "description",
               "public_metrics",
               "profile_image_url",
               "verified",
               "verified_type",
               "id",
               sep = ","
            ),
         pagination_token = next_token,
         max_results = 100
      )
      
      if(i == 1) {
         
         while (TRUE) {
            tryCatch({
               response <- GET(
                  url = url_handle[j],
                  config = add_headers(headers),
                  query = params[names(params) %in% "pagination_token" == FALSE]
               )
               
               if (status_code(response) == 200) {
                  # Process the successful response
                  break
               } else {
                  message(
                     "Attempt failed with status:", 
                     status_code(response), 
                     ". Retrying in 60 seconds."
                  )
                  Sys.sleep(60)
               }
            }, error = function(e) {
               message("Error: ", e$message, ". Retrying in 60 seconds.")
               Sys.sleep(retry_delay)
            })
         }
         
      }
      
      if(i > 1) {
         
         while (TRUE) {
            tryCatch({
               response <- GET(
                  url    = url_handle[j],
                  config = add_headers(headers),
                  query  = params
               )
               
               if (status_code(response) == 200) {
                  # Process the successful response
                  break
               } else {
                  message(
                     "Attempt failed with status:", 
                     status_code(response), 
                     ". Retrying in 60 seconds."
                  )
                  Sys.sleep(60)
               }
            }, error = function(e) {
               message("Error: ", e$message, ". Retrying in 60 seconds.")
               Sys.sleep(retry_delay)
            })
         }
         
      }
      
      obj <- content(response, as = "text")
      
      next_token <- fromJSON(obj)$meta$next_token
      
      fromJSON(obj, flatten = TRUE)$data |>
         as_tibble() |>
         clean_names() ->
         like_page_i
      
      if(nrow(like_page_i) > 0) {
         
         # Convert the mention data into a user tibble
         like_page_i |>
            rename_with(~str_remove(., 'public_metrics\\_')) |>
            mutate(
               created_at = ymd_hms(created_at, quiet = TRUE) |>
                  with_tz("America/New_York"),
               post_id = post_id[j],
               pulled_at = now()
            ) |>
            select(
               post_id, 
               user_id = id,
               pulled_at,
               created_at,
               username,
               name,
               description,
               any_of("location"),
               verified,
               verified_type,
               followers_count,
               following_count,
               post_count = tweet_count,
               listed_count,
               profile_image_url
            ) ->
            like_page_i
         
         like_page_i |>
            bind_rows(like) |>
            distinct(post_id, user_id, pulled_at, .keep_all = TRUE) ->
            like
         
      }
      
      i <- i + 1
      
      message(glue("Finished getting likes on page {i - 1} for post {j}."))
      
      if(i %% 10 == 0 | j %% 10 == 0) {
         
         message(glue("nrow(like) = {nrow(like)}."))
         
      }
      
      Sys.sleep(30)
      
   }
   
   next_token <- "restart"
   i <- 1
   
}

save_object(like)
