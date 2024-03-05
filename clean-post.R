library(tidyverse)

# Convert the engagement metrics to missing for reposts ------------------
post |> 
  mutate(
     across(
        .cols = ends_with("_count"), 
        .fns  = ~ ifelse(!is.na(reposted), NA_integer_, .x)
     )
     
  ) ->
  post

# is_thread ---------------------------------------------------------------
post |>
  # arrange the posts in chronological order
  arrange(post_id) |>
  # cluster the posts at the user-conversation level
  group_by(user_id, conversation_id) |>
  mutate(
    is_first_post = post_id == first(post_id),
    is_self_reply  = in_reply_to_user_id == user_id
  ) |>
  # Within each cluster, keep the first post at the top, elevate replies to
  # self, and sort replies to self / replies to others chronologically based on
  # the post_id values of the posts the user replied to.
  arrange(
    desc(is_first_post),
    desc(is_self_reply),
    replied_to,
    .by_group = TRUE
  ) |>
  mutate(
    is_thread = if_else(
      condition =
      # For user-conversation groups with more than one post...
        n() > 1 &
      # that were started by the user, set is_thread to TRUE if...
        conversation_id %in% post_id &
      # the user replied to their last post or the post is a self-reply
        (post_id == lead(replied_to) | replied_to == lag(post_id)),
      true      = TRUE,
      false     = FALSE,
      missing   = FALSE
    )
  ) |>
  ungroup() |>
  select(-is_first_post, -is_self_reply) ->
  post

# post_type --------------------------------------------------------------
post |>
  mutate(
    post_type = case_when(
      is_thread          ~ "Thread",
      !is.na(replied_to) ~ "Reply",
      !is.na(quoted)     ~ "Quote post",
      !is.na(reposted)   ~ "Repost",
      TRUE               ~ "Post"
    ) |>
      factor(levels = c("Thread", "Post", "Quote post", "Reply", "Repost"))
  ) |>
  relocate(post_type, .before = impression_count) |>
  select(-is_thread) ->
  post

# post_url ---------------------------------------------------
post |> 
  mutate(
    post_url = str_c("https://twitter.com/tesla/status/", post_id)
  ) |> 
  relocate(post_url, .before = post_id) ->
  post
