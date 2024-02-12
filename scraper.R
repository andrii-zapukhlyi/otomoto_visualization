library(Rcrawler)
library(tidyverse)
library(rvest)
library(purrr)

url <- "https://www.otomoto.pl/osobowe/audi?page="

df <- data.frame()

for (link in paste0(url, 1:500)) {
  crawler <- Rcrawler(link, dataUrlfilter = "oferta", MaxDepth = 1)
  web_crawling_specific_urls <- INDEX$Url
  for(specific_website in web_crawling_specific_urls){
    page <- read_html(specific_website)
    model_b <- FALSE
    year_b <- FALSE
    transmission_b <- FALSE
    mileage_b <- FALSE
    engineSize_b <- FALSE
    fuelType_b <- FALSE
    hp_b <- FALSE
    lKM_b <- FALSE
    doorCount_b <- FALSE
    for (el in page %>% html_elements(xpath = '//*[@id="__next"]/div/div/div/main/div/section[2]/div[2]/div[2]') %>% html_children() %>% html_children() %>% html_text()) {
      if (is.null(el)) {
        return(NA_character_)
      }
      if (model_b) {
        model <- el
        model_b <- FALSE
      }
      if (el == "Model pojazdu") {
        model_b <- TRUE
      }
      if (year_b) {
        year <- parse_number(el)
        year_b <- FALSE
      }
      if (el == "Rok produkcji") {
        year_b <- TRUE
      }
      if (transmission_b) {
        transmission <- el
        transmission_b <- FALSE
      }
      if (el == "Skrzynia biegów") {
        transmission_b <- TRUE
      }
      if (mileage_b) {
        mileage <- parse_number(gsub(" ", "", el))
        mileage_b <- FALSE
      }
      if (el == "Przebieg") {
        mileage_b <- TRUE
      }
      if (engineSize_b) {
        engineSize <- parse_number(gsub(" ", "", el))
        engineSize_b <- FALSE
      }
      if (el == "Pojemność skokowa") {
        engineSize_b <- TRUE
      }
      if (fuelType_b) {
        fuelType <- el
        fuelType_b <- FALSE
      }
      if (el == "Rodzaj paliwa") {
        fuelType_b <- TRUE
      }
      if (hp_b) {
        hp <- parse_number(gsub(" ", "", el))
        hp_b <- FALSE
      }
      if (el == "Moc") {
        hp_b <- TRUE
      }
      if (lKM_b) {
        lKM <- parse_number(gsub(",", ".", el))
        lKM_b <- FALSE
      }
      if (el == "Spalanie W Mieście") {
        lKM_b <- TRUE
      }
    }
    price <- page %>% html_node(xpath='//*[@id="__next"]/div/div/div/main/div/aside/div[1]/div[2]/div/div/h3') %>% html_text()
    price <- parse_number(gsub(" ", "", price))
    DataFrame <- data.frame(model, year, transmission, mileage, engineSize, fuelType, hp, lKM, price)
    df <- bind_rows(df, DataFrame)
  }
}
names(df) <- c("model", "year", "transmission", "mileage", "engineSize", "fuelType", "hp", "lkm", "price")
df <- df[!duplicated(df),]
df <- df[!is.na(df$price),]
write_csv(df, "data.csv")
