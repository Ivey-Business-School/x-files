# https://developer.twitter.com/en/docs/twitter-api/lists/list-members/api-reference/get-lists-id-members
source("00-source.R")

# Get the list_id for the Tesla Affiliates list
"https://api.twitter.com/2/users/922299968/owned_lists" |> 
  GET(config = add_headers(headers)) |> 
  content(as = "text") |> 
  fromJSON(flatten = TRUE) %>%
  .$data |> 
  filter(name == "Tesla Affiliates") |> 
  pull(id) ->
  list_id

glue("https://api.twitter.com/2/lists/{list_id}/members") |> 
  GET(
  config = add_headers(headers),
  query  = list(
    user.fields = paste(
      "created_at",
      "description",
      "protected",
      "entities",
      "location",
      "profile_image_url",
      "public_metrics",
      "url",
      "verified",
      "verified_type",
      sep = ","
    ),
    max_results = 100
  )
) |> 
  content(as = "text", encoding = "UTF-8") |> 
  fromJSON(flatten = TRUE)%>%
  .$data |> 
  as_tibble() |> 
  clean_names() |>   
  rename_with(~str_remove(., "public_metrics_")) |> 
  mutate(
    user_id = id,
    created_at = ymd_hms(created_at, quiet = TRUE) |> 
       with_tz(tz = "America/New_York")
  ) ->
  user

user |> 
  add_column(
    !!!user_cols[setdiff(names(user_cols), names(user))]
  ) |> 
  select(names(user_cols)) |> 
  arrange(desc(followers_count)) ->
  user

write_rds(user, here("data", today(), "user"))
