# Scrape billboard charts

library(tidyverse)
library(rvest)
library(lubridate)
library(here)


charts_list <- c("billboard-global-200")


for (chart in charts_list) {
  
  # make the url
  scrape_url <- paste(
    "https://www.billboard.com/charts/",
    chart,
    "/",
    sep = ""
  )
  

  scrape_first <- read_html(scrape_url)
  
  chart_date <- scrape_first |> 
    html_element("div#chart-date-picker") |> 
    html_attr("data-date")
  
  chart_year <- year(chart_date)
  
  scrape_tibble <- scrape_first |> 
    html_elements("ul.o-chart-results-list-row") |> 
    html_text2() |> 
    as_tibble()
  
  scrape_clean <- scrape_tibble |> 
    mutate(
      data_cleaned = str_remove_all(value, " NEW\n|NEW"),
      data_cleaned = str_remove_all(data_cleaned, " RE- ENTRY\n|RE- ENTRY")
    ) |> 
    select(data_cleaned, value) |> 
    separate(
      col = data_cleaned,
      sep = "\n",
      into = c(
        "current_week",
        "title",
        "performer",
        "last_week",
        "peak_pos",
        "wks_on_chart"
      )
    ) |> 
    select(-value) |> 
    mutate(chart_week = chart_date) |> 
    select(chart_week, everything())
  
  folder_path <- paste("data-scraped/", chart, "/", chart_year, "/", sep = "")
  
  if (!dir.exists(here(folder_path))) {dir.create(here(folder_path), recursive = TRUE)}
  
  scrape_clean |> write_csv(paste(folder_path, chart_date, ".csv", sep = ""))
  
}
