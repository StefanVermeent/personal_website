library(scholar)
library(rorcid)
library(dplyr)
library(stringr)
library(htmltools)

# -----------------------------
# 1. BOLD MY NAME
# -----------------------------
bold_my_name <- function(authors) {
  str_replace_all(
    authors,
    regex("S Vermeent", ignore_case = TRUE),
    "**S Vermeent**"
  )
}



# -----------------------------
# 2. OPTIONAL: link lookup table
# -----------------------------
# Create a file manual_links.csv with columns:
# title,pdf,code,doi
#
# Example row:
# "Adversity is associated...", pubs/vermeent_2025_JEPGeneral.pdf, https://github..., 10.1037/xxxx
# -----------------------------
#manual_links <- read.csv("manual_links.csv", stringsAsFactors = FALSE)
#
#find_links <- function(title, type) {
#  row <- manual_links[which(manual_links$title == title), ]
#  if (nrow(row) == 0) return(NA)
#  row[[type]]
#}
#
#icon_html <- function(type, link) {
#  if (is.na(link) || link == "") return("")
#  
#  icon <- switch(type,
#                 pdf = '<i class="bi bi-file-earmark-pdf"></i>',
#                 code = '<i class="bi bi-github"></i>',
#                 doi = '<i class="bi bi-link-45deg"></i>'
#  )
#  
#  sprintf('<a href="%s" target="_blank">%s</a>', link, icon)
#}

# -----------------------------
# 3. Format one publication
# -----------------------------
format_pub <- function(pub) {
  
  title   <- pub$title
  authors <- bold_my_name(pub$author)
  year    <- pub$year
  venue   <- pub$venue
  
  pdf_link  <- find_links(title, "pdf")
  code_link <- find_links(title, "code")
  doi_link  <- find_links(title, "doi")
  
  icons <- paste(
    icon_html("pdf", pdf_link),
    icon_html("code", code_link),
    icon_html("doi", if (!is.na(doi_link)) paste0("https://doi.org/", doi_link) else NA)
  )
  
  HTML(sprintf(
    '%s (%s). %s. *%s*. %s<br><br>',
    authors, year, title, venue, icons
  ))
}

# -----------------------------
# 4. MAIN ENTRY POINT
# -----------------------------
fetch_publications <- function(id) {
  
  orcid <- rorcid::works("0000-0002-9595-5373") |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    select(type, title = title_title_value, url = url_value, journal = journal_title_value) |> 
    filter(type == "journal-article", str_detect(url, "https://doi.org"), !str_detect(title, "^Correction to")) |> 
    mutate(match_title = str_to_lower(title)) |> 
    select(match_title, url)
  
  scholar <- scholar::get_publications(id) |> 
    as_tibble() %>%
    rowwise() %>%
    mutate(
      match_title = str_to_lower(title)
   #   author = case_when(
   #     str_ends(author, "\\.\\.\\.") ~ scholar::get_complete_authors(id = id, pubid = pubid, delay= 3),
   #     TRUE ~ author)
      ) |> 
    ungroup() |> 
    mutate(year = ifelse(is.na(year), max(year, na.rm=T), year)) 
  
  orcid |> 
    left_join(scholar, by = "match_title") %>% 
    rowwise() |> 
    mutate(
      author = str_split(author, pattern = ", ", simplify = TRUE) |> str_replace("^([A-Z]+)\\s+(.+)$", "\\2, \\1") %>% str_c(., collapse = "., ")
    ) |> 
    glue::glue_data("{author} ({year}). {title} *{journal}* {number}. [{url}]({url})\n\n")
}


