here::here("x-api", "get-users.R") |> 
   source()

user_id <- pull(user, user_id)
name <- pull(user, name)

url_handle <- glue("https://api.twitter.com/2/users/{user_id}/tweets")

post <- NULL
post_poll_option <- NULL
post_media <- NULL
post_mention  <- NULL
# user <- NULL
next_token <- "start"
i <- 1
loop_vector <- seq_along(url_handle)

# Create the datetime object in "America/New_York" time zone
ymd_hms("2024-02-01 00:00:00", tz = "America/New_York") |> 
   with_tz(tz = "UTC") |> 
   format("%Y-%m-%dT%H:%M:%SZ") ->
   start_time

ymd_hms("2024-02-29 23:59:59", tz = "America/New_York") |> 
   with_tz(tz = "UTC") |> 
   format("%Y-%m-%dT%H:%M:%SZ") ->
   end_time

# Get last month's posts --------------------------------------------------
for(j in loop_vector) {
   
   while(!is.null(next_token)) {
      
      params <- list(
         tweet.fields = 
            paste(
               "created_at",
               "text",
               "public_metrics",
               "geo",
               "attachments",
               "context_annotations",
               "entities",
               "lang",
               "referenced_tweets",
               "reply_settings",
               "conversation_id",
               "in_reply_to_user_id",
               "author_id",
               "edit_history_tweet_ids",
               "id",
               sep = ","
            ),
         user.fields =
            paste(
               "created_at",
               "username",
               "name",
               "description",
               "public_metrics",
               "profile_image_url",
               "id",
               sep = ","
            ),
         media.fields =
            paste(
               "duration_ms",
               "height",
               "width",
               "preview_image_url",
               "type",
               "url",
               "public_metrics",
               "variants",
               "media_key",
               sep = ","
            ),
         poll.fields = "end_datetime,duration_minutes,options,voting_status,id",
         expansions =
            paste(
               # user.fields for the tweet author, users mentioned, and authors
               # of referenced tweets
               "author_id",
               "entities.mentions.username",
               "referenced_tweets.id.author_id",
               # tweet.fields for referenced tweets
               "referenced_tweets.id",
               "in_reply_to_user_id",
               # media and poll information
               "attachments.media_keys",
               "attachments.poll_ids",
               sep = ","
            ),
         start_time = start_time,
         end_time   = end_time,
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
      
      # Get the next token for pagination
      next_token <- fromJSON(obj)$meta$next_token
      
      # Convert the post data into a tibble ------------------------------------
      fromJSON(obj, flatten = TRUE)$data |> 
         as_tibble() |> 
         clean_names() ->
         post_page_i
      
      post_page_i |> 
         add_column(
            !!!post_cols[setdiff(names(post_cols), names(post_page_i))]
         ) ->
         post_page_i
      
      if(nrow(post_page_i) > 0) {
         
         post_page_i |> 
            rename_with(~str_remove(., 'public_metrics\\_')) |> 
            mutate(
               created_at = ymd_hms(created_at) |> 
                  with_tz("America/New_York")
            ) |> 
            select(
               created_at,
               text,
               impression_count,
               like_count,
               repost_count = retweet_count,
               quote_count,
               reply_count,
               bookmark_count,
               context_annotations,
               entities_annotations,
               entities_cashtags,
               entities_hashtags,
               entities_mentions,
               entities_urls,
               poll_id   = attachments_poll_ids,
               media_ids = attachments_media_keys,
               geo_place_id,
               reply_settings,
               lang,
               referenced_posts = referenced_tweets,
               conversation_id,
               in_reply_to_user_id,
               user_id = author_id,
               edit_history_post_ids = edit_history_tweet_ids,
               post_id = id
            ) ->
            post_page_i
         
         # if the posts on post_page_i reference any other posts, separate the
         # reference_posts variable into replied_to, reposted, and quoted
         if(post_page_i |> 
            filter(map_lgl(referenced_posts, ~!is.null(.x))) |> 
            nrow() > 0
         ) {
            
            post_page_i |> 
               select(post_id, referenced_posts) |> 
               unnest(referenced_posts) |> 
               pivot_wider(
                  id_cols     = post_id, 
                  names_from  = type, 
                  values_from = id
               ) |> 
               left_join(x = post_page_i, by = "post_id", multiple = "all") ->
               post_page_i
            
            if("retweeted" %in% names(post_page_i)) {
               post_page_i |>
                  rename(reposted = retweeted) ->
                  post_page_i
            }
            
            post_page_i |> 
               select(
                  created_at:conversation_id, 
                  any_of(c("replied_to", "reposted", "quoted")),
                  everything(),
                  -referenced_posts
               ) ->
               post_page_i
            
         } else post_page_i <- select(post_page_i, -referenced_posts)
         
         post_page_i |> 
            bind_rows(post) |> 
            distinct(post_id, .keep_all = TRUE) ->
            post
         
      }
      
      # Convert the poll data into a tibble -----------------------------------
      fromJSON(obj, flatten = TRUE)$includes$poll |>
         as_tibble() ->
         post_poll_page_i

      if(nrow(post_poll_page_i) > 0) {

         post_poll_page_i |>
            add_column(
               !!!poll_cols[setdiff(names(poll_cols), names(post_poll_page_i))]
            ) ->
            post_poll_page_i

         post_poll_page_i |>
            unnest(options) |>
            mutate(
               end_datetime = ymd_hms(end_datetime, quiet = TRUE) |> 
                  with_tz(tz = "America/New_York")
            ) |>
            select(
               poll_id = id,
               end_datetime,
               position,
               label,
               votes,
               voting_status
            ) ->
            post_poll_page_i

         post_page_i |>
            select(post_id, poll_id) |>
            unnest(poll_id) |>
            left_join(post_poll_page_i, by = "poll_id", multiple = "all") ->
            post_poll_page_i

         post_poll_page_i |>
            bind_rows(post_poll_option) |>
            distinct(post_id, poll_id, position, .keep_all = TRUE) ->
            post_poll_option

      }
      
      # Convert the media data into a tibble -----------------------------------
      fromJSON(obj, flatten = TRUE)$includes$media |>
         as_tibble() ->
         post_media_page_i

      if(nrow(post_media_page_i) > 0) {

         post_media_page_i |>
            add_column(
               !!!post_media_cols[setdiff(
                  names(post_media_cols),
                  names(post_media_page_i)
               )]
            ) ->
            post_media_page_i

         post_media_page_i |>
            mutate(
               bit_rate =
                  map_chr(
                     variants,
                     ~ ifelse(
                        is.null(.x),
                        NA_character_,
                        .x %>%
                           filter(bit_rate == max(bit_rate, na.rm = TRUE))|>
                           mutate(bit_rate = as.character(bit_rate)) |>
                           slice(1) %>%
                           pull(bit_rate)
                     )
                  ),
               url_1 =
                  map_chr(
                     variants,
                     ~ ifelse(
                        is.null(.x),
                        NA_character_,
                        .x %>%
                           filter(bit_rate == max(bit_rate, na.rm = TRUE)) |>
                           slice(1) %>%
                           pull(url)
                     )
                  ),
               url = ifelse(!is.na(url_1), url_1, url)
            ) |>
            select(
               media_id = media_key,
               height,
               width,
               type,
               duration_ms,
               bit_rate,
               url
            ) ->
            post_media_page_i

         post_page_i |>
            select(post_id, media_ids) |>
            unnest(media_ids) |>
            rename(media_id = media_ids) |>
            left_join(post_media_page_i, by = "media_id", multiple = "all") ->
            post_media_page_i

         post_media_page_i |>
            bind_rows(post_media) |>
            distinct(post_id, media_id, .keep_all = TRUE) ->
            post_media

      }
      
      # Convert the mention data into a user tibble and a mention tibble -------
      # fromJSON(obj, flatten = TRUE)$includes$user |>
      #    as_tibble() |>
      #    clean_names() ->
      #    post_mention_page_i
      # 
      # if(nrow(post_mention_page_i) > 0) {
      # 
      #    # Convert the mention data into a user tibble
      #    post_mention_page_i |>
      #       rename_with(~str_remove(., 'public_metrics\\_')) |>
      #       mutate(
      #          created_at = ymd_hms(created_at, quiet = TRUE) |> 
      #             with_tz("America/New_York")
      #       ) |>
      #       select(
      #          user_id = id,
      #          created_at,
      #          username,
      #          name,
      #          description,
      #          any_of("location"),
      #          followers_count,
      #          following_count,
      #          post_count = tweet_count,
      #          listed_count,
      #          profile_image_url
      #       ) ->
      #       user_page_i
      # 
      #    user_page_i |>
      #       bind_rows(user) |>
      #       distinct(user_id, .keep_all = TRUE) ->
      #       user
      # 
      # }
      # 
      # if(nrow(post_mention_page_i) > 1) {
      # 
      #    if(nrow(unnest(post_page_i, entities_mentions)) > 0) {
      # 
      #       post_page_i |>
      #          select(post_id, entities_mentions) |>
      #          unnest(entities_mentions) |>
      #          mutate(
      #             type = case_when(
      #                start == 3 ~ "Reposted",
      #                start == 0 ~ "Replied to",
      #                TRUE ~ NA_character_
      #             )
      #          ) |>
      #          # drop usernames not mentioned in the zeroth or third spot
      #          # because they are most often users with posts t-2 and prior in a
      #          # conversation. TODO: only drop usernames that (a) come after a
      #          # user mentioned in the zeroth spot and (b) come before text
      #          # excluding usernames.
      #          filter(!is.na(type)) |> 
      #          select(post_id, type, user_id = id) ->
      #          post_mention_repost_or_reply
      # 
      #    } else post_mention_repost_or_reply <- tibble()
      # 
      #    #### fix for quotes needed
      #    if(exists(where = post_page_i, x = "quoted")) {
      # 
      #       post_page_i |>
      #          filter(!is.na(quoted)) |>
      #          select(post_id, quoted) |>
      #          mutate(user_id = NA_character_) ->
      #          post_quote_post_author_crosswalk
      # 
      #    } else post_quote_post_author_crosswalk <- tibble()
      # 
      #    # get user_id values for quoted posts
      #    if(nrow(post_quote_post_author_crosswalk) > 0) {
      # 
      #       post_quote_post_author_crosswalk |>
      #          pull(quoted) ->
      #          quote_post_id
      # 
      #       q_handle <- glue("https://api.twitter.com/2/tweets/{quote_post_id}")
      # 
      #       q_params <- list(user.fields = "id", expansions = "author_id")
      # 
      #       for(k in seq_along(q_handle)) {
      # 
      #          # sleep for three seconds to stay within the rate limit (300
      #          # requests every 15 minutes)
      #          Sys.sleep(5)
      # 
      #          GET(
      #             url    = q_handle[k],
      #             config = add_headers(headers),
      #             query  = q_params
      #          ) %>%
      #             content(as = "text", encoding = "UTF-8") ->
      #             q_obj
      # 
      #          if(grepl("Error|Unavailable", q_obj)) {
      # 
      #             post_quote_post_author_crosswalk |>
      #                mutate(
      #                   user_id = ifelse(
      #                      quoted == quote_post_id[k],
      #                      NA_character_,
      #                      user_id[k]
      #                   )
      #                ) ->
      #                post_quote_post_author_crosswalk
      # 
      #          } else {
      # 
      #             post_quote_post_author_crosswalk |>
      #                mutate(
      #                   user_id = ifelse(
      #                      quoted == quote_post_id[k],
      #                      fromJSON(q_obj)$includes$user$id,
      #                      user_id
      #                   )
      #                ) ->
      #                post_quote_post_author_crosswalk
      # 
      #          }
      # 
      #       }
      # 
      #       post_quote_post_author_crosswalk |>
      #          select(-quoted) |>
      #          mutate(type = "Quoted") |>
      #          inner_join(
      #             x        = post_mention_page_i,
      #             by       = "user_id",
      #             multiple = "all"
      #          ) |>
      #          bind_rows(post_mention_repost_or_reply) |>
      #          select(post_id, type, user_id, everything()) ->
      #          post_mention_page_i
      # 
      #    } else{
      # 
      #       post_mention_repost_or_reply |>
      #          select(
      #             any_of(c("created_at", "post_id", "type", "user_id")),
      #             everything()
      #          ) ->
      #          post_mention_page_i
      # 
      #    }
      # 
      #    post_mention_page_i |>
      #       bind_rows(post_mention) |>
      #       distinct(post_id, type, user_id, .keep_all = TRUE) ->
      #       post_mention
      # 
      # }
      
      # Next page ---------------------------------------------------------
      
      i <- i + 1
      
      message(
         glue(
            "Finished getting posts on page {i - 1} for {name[j]} ", 
            "(user {j} of {length(url_handle)}). ",
            "nrow(post) = {nrow(post)}."
         )
      )
      
      Sys.sleep(61)
      
      
   }
   
   next_token <- "restart"
   i <- 1
   
}
   
# create context
post |> 
   select(post_id, context_annotations) |> 
   unnest(context_annotations) |> 
   clean_names() ->
   post_context

post |> 
   select(post_id, entities_urls) |> 
   unnest(entities_urls) |> 
   clean_names() ->
   post_url

post |> 
   select(post_id, entities_cashtags) |> 
   unnest(entities_cashtags) |> 
   clean_names() ->
   post_cashtag

post |> 
   select(post_id, entities_hashtags) |> 
   unnest(entities_hashtags) |> 
   clean_names() ->
   post_hashtag

post |> 
   select(post_id, entities_annotations) |> 
   unnest(entities_annotations) |> 
   clean_names() ->
   post_entity_annotation

post |> 
   select(post_id, edit_history_post_ids) |> 
   unnest(edit_history_post_ids) |> 
   clean_names() |> 
   rename(edited_post_id = edit_history_post_ids) |> 
   filter(post_id != edited_post_id) ->
   post_edited_post_id

post_raw <- post

post |>
   select(
      created_at,
      text,
      impression_count,
      like_count,
      repost_count,
      quote_count,
      reply_count,
      bookmark_count,
      reply_settings,
      reposted,
      quoted,
      replied_to,
      in_reply_to_user_id,
      user_id,
      conversation_id,
      post_id
   ) ->
   post

save_object(post_raw)
save_object(post)
save_object(post_media)
save_object(post_poll_option)
# save_object(post_mention)
save_object(post_context)
save_object(post_url)
save_object(post_cashtag)
save_object(post_hashtag)
save_object(post_entity_annotation)
save_object(post_edited_post_id)
