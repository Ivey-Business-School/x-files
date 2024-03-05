bearer_token <- Sys.getenv("X_BEARER_TOKEN")

headers <- c(Authorization = glue("Bearer {bearer_token}"))
  
twitter_gold <- "#DBAB01"
tesla_red <- "#C20000"
tesla_logo_red <- "#E31837"

entity_label_automotive <- c(
  "Automotive",
  "Automotive, Aircraft & Boat Business",
  "Automobile Brands",
  "Auto Manufacturer - Auto",
  "Luxury Cars",
  "Electric vehicles",
  "Hybrid and electric vehicles",
  "Transportation",
  "Travel & Transportation Business"
)

entity_label_tesla <- c(
  "Tesla Motors",
  "Tesla - Model 3",
  "$TSLA",
  "Tesla's Investors Day"
)

# model_colors <- c(
#    "Cybertruck" = "#818181",
#    "Model S"    = "#212121",
#    "Model 3"    = "",
#    "Model X"    = "",
#    "Model Y"    = "#E31937"
# )

sysfonts::font_add_google("Roboto","Roboto")
sysfonts::font_add("fab", "fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# define columns ----------------------------------------------------------

user_cols <- tibble(
  created_at        = NA_POSIXct_,
  username          = NA_character_,
  name              = NA_character_,
  description       = NA_character_,
  followers_count   = NA_integer_,
  following_count   = NA_integer_,
  listed_count      = NA_integer_,
  tweet_count       = NA_integer_,
  protected         = NA,
  verified          = NA,
  verified_type     = NA_character_,
  location          = NA_character_,
  profile_image_url = NA_character_,
  # list_id           = NA_character_,
  user_id           = NA_character_
)

post_cols <- tibble(
  id                     = NA_character_,
  in_reply_to_user_id    = NA_character_, 
  context_annotations    = list(NULL),
  entities_annotations   = list(NULL),
  entities_cashtags      = list(NULL),
  entities_hashtags      = list(NULL),
  entities_mentions      = list(NULL),
  entities_urls          = list(NULL),
  geo_place_id           = NA_character_,
  referenced_tweets       = list(NULL),
  attachments_media_keys = list(NULL),
  attachments_poll_ids   = list(NULL)
)

poll_cols <- tibble(
  id               = NA_character_,
  end_datetime     = NA_character_,
  duration_minutes = NA_character_,
  options          = list(NULL),
  voting_status    = NA_character_
)

post_media_cols <- tibble(
  media_key   = NA_character_,
  duration_ms = NA_integer_,
  url         = NA_character_, 
  bit_rate    = NA_character_,
  variants    = list(NULL)
)
