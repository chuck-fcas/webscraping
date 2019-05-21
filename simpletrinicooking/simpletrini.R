# Libraries -----
library(tidyverse)
library(xml2)
library(rvest)
  
simplytrini_url <- "https://www.simplytrinicooking.org/category/recipes/"

# Functions to shorten the code a little-----
node_attr <- function(x, node, attr){html_node(x, node) %>% html_attr(attr)} #Uses html_node not html_nodes
node_text <- function(x, node){html_nodes(x, node) %>% html_text()}
node_node <- function(x, node_parent, node_child){html_nodes(x, node_parent) %>% html_nodes(node_child)}

parse_ptdhms <- function(str){
  # Converts an html datetime format of "PTDHMS" to number of seconds.  
  # E.g. "PT15H10M" is 15 hours and 10 minutes.  "PT1D3H5M" is 1 day 3 hours and 5 minutes. 
  stringr::str_extract(str, "\\d+(?=D)") %>% {ifelse(is.na(.), 0, as.numeric(.) * 24 * 60 * 60)} +
  stringr::str_extract(str, "\\d+(?=H)") %>% {ifelse(is.na(.), 0, as.numeric(.) * 60 * 60)} +
  stringr::str_extract(str, "\\d+(?=M)") %>% {ifelse(is.na(.), 0, as.numeric(.) * 60)} +
  stringr::str_extract(str, "\\d+(?=S)") %>% {ifelse(is.na(.), 0, as.numeric(.))}
}

# Initialize tibble with page urls----
tbl_trini <- data.frame(
  page_url =
    simplytrini_url %>%
    read_html %>%
    html_nodes("[class='archive-pagination pagination'] li") %>%
    html_text %>%
    str_extract("\\d+") %>%
    max(na.rm = TRUE) %>%
    1:. %>%
    paste0(simplytrini_url, "page/", .),
  stringsAsFactors = FALSE
)

# Scrape each page for every recipe title and url, unnest to recipe level---
tbl_trini <-
  tbl_trini %>% 
  mutate(recipe_html  = page_url    %>% map(read_html),
         recipe_title = recipe_html %>% map(html_nodes, "[class='entry-title-link']") %>% map(html_text),
         recipe_url   = recipe_html %>% map(html_nodes, "[class='entry-title-link']") %>% map(html_attr, "href")
  ) %>% 
  select(-recipe_html) %>% 
  group_by(page_url) %>% 
  unnest

# Read the html for each recipe---
tbl_trini <-
  tbl_trini %>% 
  mutate(recipe_html = recipe_url %>% map(read_html))

# Annoying to lose the work so we save.
tbl_trini %>% save(file = "tbl_trini_recipehtml.Rda")
load("tbl_trini_recipehtml.Rda")

# Parse the html into smaller components, tidy tags and prepare items----
tbl_trini <-
  tbl_trini %>% 
  mutate(
    img_link        = recipe_html %>% map(node_attr,   "noscript img[itemprop='image']", "src"),
    tags            = recipe_html %>% map(node_attr,   "article",                        "class"),
    times           = recipe_html %>% map(node_node,   "div.ERSTimes",                   "time"),
    items           = recipe_html %>% map(node_node,   "div.divERSHeadItems",            "span"),
    ingredients     = recipe_html %>% map(node_node,   "div.ERSIngredients",             "li.ingredient"),
    instructions    = recipe_html %>% map(node_node,   "div.ERSInstructions",            "li.instruction"),
    notes           = recipe_html %>% map(node_text,   "div.ERSNotesDiv") %>% sapply(str_extract, "(?<=<div class=\"ERSNotes\">).+(?=</div>)"),
    tag_recipe_type = tags        %>% map(str_extract, "(?<=recipe_type-)[:graph:]*(?=[:blank:])"),
    tag_skill_level = tags        %>% map(str_extract, "(?<=skill_level-)[:graph:]*(?=[:blank:])"),
    tag_course      = tags        %>% map(str_extract, "(?<=course-)[:graph:]*(?=[:blank:])"),
    tag_cuisine     = tags        %>% map(str_extract, "(?<=cuisine-)[:graph:]*(?=[:blank:])"),
    notes_ind       = !(notes     %>% is.na),
    tag_tag = 
      tags %>% 
      map(str_extract, "tag.+") %>% 
      str_split("[ ]") %>% 
      map(function(x) x[str_detect(x, "tag-")]), #Filter to 'tag-' only
    tag_ingredient = 
      tags %>% 
      map(str_extract, "ingredient.+") %>% 
      str_split("[ ]") %>%  
      map(function(x) x[str_detect(x, "ingredient-")]), #Filter to 'ingredient-' only
    items_temp = 
      items %>% 
      map(function(x)
        data.frame(
          itemprop = x %>% sapply(html_attr, "itemprop"),
          value    = x %>% sapply(html_text), 
          stringsAsFactors = FALSE) %>% 
          as.tibble)
  ) %>% 
  unnest(
    tag_recipe_type,
    tag_skill_level,
    tag_course,
    tag_cuisine
  )

# Force the spread function to work on items.  Probably a better way but it does the trick----
tbl_trini <-
  tbl_trini %>%
  mutate(
    items_temp =
      items_temp %>% 
      seq_along %>% 
      lapply(function(i) tbl_trini$items_temp[[i]] %>% spread(itemprop, value))
  ) %>% 
  unnest(items_temp)

# Tidy the times----
tbl_trini <-
  tbl_trini %>% 
  left_join(
    tbl_trini %>% 
    mutate(
      times_new = 
        times %>% 
        map(function(x)
          data.frame(
            itemprop = x %>% sapply(html_attr, "itemprop") %>% ifelse(is.na(.), "totalTime", .),
            datetime = x %>% sapply(html_attr, "datetime") %>% parse_ptdhms,
            stringsAsFactors = FALSE
          ) %>% 
            as.tibble 
        ) %>% 
        map(function(x) x %>% spread(itemprop, datetime))
      ) %>% 
      unnest(times_new)
  )

cooking_units <- 
  c("tsps?",
    "tbsps?",
    " c |cups?",
    "ozs?|ounces?",
    "lbs?|pounds?",
    "(?<=\\d)g|(?<=\\d) g |(?<=\\d) g\\.",
    "dash",
    "to taste",
    "pinch|pinch of",
    "bundles?",
    "pints?|glass[es]?|liters?|litres?",
    "big|small|med(ium)?|large",
    "head|head of",
    "cloves?",
    "leaf|leaves",
    "tin|can") %>% 
  paste0(collapse = "|")

cooking_verbs <-
  "https://www.enchantedlearning.com/wordlist/cooking.shtml" %>% 
  read_html %>% 
  html_nodes("[class='wordlist-item']") %>% 
  html_text %>% 
  str_to_lower %>% 
  paste0("(?=[|d|ed]?)") %>% 
  paste0(collapse = "|")

cooking_amounts <- 
  c("00BC", "00BD", "00BE", 
    #Vulgar fractions
    paste0("215", 0:9),  
    paste0("215", LETTERS[1:6])) %>% # A to F
  paste0("\\u", ., collapse = "|") %>% # Add unicode prescipt (C, C++, and Java)
  paste0("|[:digit:]+") # Add integars

# Tidy the ingredients----
tbl_trini <-
  tbl_trini %>% 
  mutate(
    ingredients_unit_measure =
      ingredients %>% 
      map(str_extract, cooking_units)%>% 
      map(str_to_lower) %>% 
      map(str_trim),
    ingredients_temp = # Original preserved just in case
      ingredients %>% 
      map(str_remove, cooking_units),
    ingredients_verb =
      ingredients_temp %>% 
      map(str_extract, cooking_verbs)%>% 
      map(str_to_lower) %>% 
      map(str_trim),
    ingredients_note =
      ingredients_temp %>% 
      map(str_extract, "(?<=\\().+(?=\\))") %>% 
      map(str_to_lower) %>% 
      map(str_trim),
    ingredients_temp =
      ingredients_temp %>% 
      map(str_remove, "(?<=\\().+(?=\\))"),
    ingredients_amount =
      ingredients_temp %>% 
      map(str_extract, cooking_amounts) %>% 
      map(str_to_lower) %>% 
      map(str_trim),
    ingredients_leftover =
      ingredients_temp %>% 
      map(str_remove, cooking_amounts) %>% 
      map(str_remove_all, "[^[:alnum:][:space:]-]") %>%  
      map(str_to_lower) %>% 
      map(str_trim)
  )

# Convert CamelCase names into snake_case names
names(tbl_trini) <- 
  tbl_trini %>% 
  names %>% 
  gsub("([a-z])([A-Z])", "\\1_\\L\\2", ., perl = TRUE)

# Remove nodeset objects.
tbl_trini <-
  tbl_trini %>% 
  select(
    -recipe_html,
    -times,
    -items,
    -ingredients,
    -instructions
    )

tbl_trini %>% write_rds("tbl_trini.Rds")

## Garbage code. ----
## This was having problems but leaving here

# Function to parallize a function f(x) across CPUs
# foreach_core <- 
#   function(x, f, no_cores){  
#     require(foreach)
#     x_split <- 
#       split(x, 
#             rep(1:ceiling(nrow(x)/no_cores), 
#                 each = no_cores, 
#                 length.out = nrow(x)))
#     f_output <-
#       foreach(x = x_split) %dopar% {f(x)}
#     return(f_output)
#   }
# 
# cl <- parallel::makeCluster(parallel::detectCores()-1)
# doParallel::registerDoParallel(cl)
# 
# x <- Sys.time()
# tbl_trini <-
#   foreach_core(
#     tbl_trini, 
#     function(x) {
#       library(tidyverse)
#       library(rvest)
#       library(xml2)
#       mutate(x, 
#              recipe_html  = page_url    %>% map(read_html),
#              recipe_title = recipe_html %>% map(html_nodes, "[class='entry-title-link']") %>% map(html_text),
#              recipe_url   = recipe_html %>% map(html_nodes, "[class='entry-title-link']") %>% map(html_attr, "href")
#       ) %>% 
#         select(-recipe_html) %>% 
#         group_by(page_url) %>% 
#         unnest},
#     5) %>% 
#   bind_rows 
# 
# tbl_trini <-
#   tbl_trini %>% 
#   foreach_core(function(x) dplyr::mutate(x, recipe_html = purrr::map(recipe_url, xml2::read_html)), 5) %>%
#   bind_rows
# Sys.time() - x
# 
# parallel::stopCluster(cl)