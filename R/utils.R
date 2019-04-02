pdf_downloader <- function(day, month, year) {
  `
  %>%` <- dplyr::`%>%`

  url <- 'https://www.camara.leg.br/internet/plenario/notas/notas.asp'
  query <- list('dia'= day,
                'mes'= month,
                'ano'= year)


  response <- httr::GET(url = url, query = query)
  response_html <- httr::content(response)

  links <- response_html %>%
    rvest::html_node('ul') %>%
    rvest::html_children() %>%
    rvest::html_children() %>%
    rvest::html_children() %>%
    rvest::html_attr('href')

  text <- response_html %>%
    rvest::html_node('ul') %>%
    rvest::html_children() %>%
    rvest::html_children() %>%
    rvest::html_text()

    if (length(links) > 0 ) {
      solene <- which(grepl('Solene|Congresso', text))
    
#      links <- 
#        ifelse(
#          grepl('leg\\.br', links), 
#          links,  
#          paste0('https://www.camara.leg.br', links))
      
      if (length(solene) > 0) {
        links_plenario <- links[-solene]
      } else {
        links_plenario <- links
      }
    
    
      files <- c()
      if(length(links_plenario) > 0) {
        for (i in seq_along(links_plenario)) {
          file_name <- tempfile()
          download.file(
            links_plenario[i],
            file_name,
            mode = 'wb')
          files <- append(files, file_name)
        }}
    
      return(files)
  }
}


pdf_parser <- function(file) {
  
  `%>%` <- dplyr::`%>%`
  
  if (file.size(file) > 0) {
    
    pdf <- pdftools::pdf_text(file)
  
    date <-
      stringr::str_extract(pdf[1], '[\\d]{2}\\/[\\d]{2}\\/[\\d]{4}')
  
  
    clean <- stringr::str_replace(pdf, '.*\\\r', '')
    clean <- paste(clean, collapse = ' ')
    clean <- gsub('\\n|\\r', ' ', clean)
  
    citations <-
      unlist(strsplit(clean, '(?<=.)(?= (O SR\\.|A SRA\\.) .*\\(.*\\) - )', perl = T))
    citations <- dplyr::tibble(citations)
    citations$identifier <-
      stringr::str_extract(citations$citations, ' (O SR\\.|A SRA\\.) .*\\(.*\\) - ')
    citations$gender <-
      ifelse(grepl(
        citations$identifier,
        pattern = 'SR\\.',
        ignore.case = F
      ),
      'M',
      'F')
  
    citations$name <-
      gsub('( O SR\\. | A SRA\\. )(.*)( \\(.*)', '\\2', citations$identifier)
  
  
    citations$president <- ifelse(citations$name == 'PRESIDENTE', 1, 0)
    citations$name <- ifelse(citations$name == 'PRESIDENTE',
                             sub('(.*\\()(.*)(\\..*)', '\\2', x = citations$identifier) %>% toupper(),
                             citations$name)
  
    session_type <-
      stringr::str_extract(citations$citations[1], '(?<=\\()SESSÃO.*?(?=\\))')
    session_n <-
      unlist(stringr::str_extract_all(citations$citations[1], '[\\d]+ª SESSÃO'))[2] %>% gsub(pattern = 'ª SESSÃO', replacement =  '')
    session_legislature <-
      stringr::str_extract(citations$citations[1] , '[\\d]+(?=ª LEGISLATURA)')
    citations$citations <-
      gsub('( (O SR\\.|A SRA\\.) .*\\(.*\\) - )(.*)',
           '\\3',
           citations$citations)
  
    session_start_time <-
      stringr::str_extract_all(pattern = '(?<=Às )[\\d]+|[\\d]+(?= minutos)', string =  citations$citations[1]) %>% unlist() %>% paste(collapse = ':')
  
    session_end_time <-
      stringr::str_extract_all(pattern = '(?<=Encerra-se a sessão às )[\\d]+|[\\d]+(?= minutos)', string =  citations$citations[nrow(citations)]) %>% unlist() %>% paste(collapse = ':')
  
    citations <-
      citations %>% dplyr::select(name, president, gender, quote = citations) %>% dplyr::filter(!is.na(name))
    citations$date <- date
    citations$session_type <- session_type
    citations$session_n <- session_n
    citations$session_start_time <- session_start_time
    citations$session_end_time <- session_end_time
    citations$legislature <- session_legislature
  
    citations$word_count <- stringr::str_count(citations$quote, '\\S+')
    citations$sequence <- c(1:nrow(citations))
    print(paste(date, '- File Parsed'))
    return(citations)
  }

}

#' Get Plenary Data
#'
#' Crawls trough plenary session transcripts between defined dates, downloads PDF files and parse data
#'
#' @param initial.date (\code{character}) defining first date to include in the search
#' @param final.date (\code{character}) defining last date to include in the search
#' @param format specify which date format is being used as input
#'
#' @return a tibble with one row for each speech
#'
#' @examples plenary_data <- get_data('20/03/2019', '27/03/2019', format = '%d/%m/%Y')
#'
#' @export

get_data <- function(initial.date, final.date, format = '%d/%m/%Y') {

  `%>%` <- dplyr::`%>%`

  initial.date <- as.Date(initial.date, format)
  final.date <- as.Date(final.date, format)
  

  search_grid <-
    seq(initial.date,final.date, by = 'days') %>%
    dplyr::tibble() %>%
    dplyr::mutate(
      day = lubridate::day(.),
      month = lubridate::month(.),
      year = lubridate::year(.)
    ) %>%
    dplyr::select(-.) %>% 
    dplyr::filter(
      dplyr::case_when(
              month == 1 ~ F,
              month == 2 & day == 1 ~ F,
              month == 7 & day > 17 ~ F,
              month == 12 & day > 22 ~ F,
              TRUE ~ TRUE
    ))

  files <- purrr::pmap(search_grid, pdf_downloader) %>% unlist()

  data <- purrr::map_df(files, pdf_parser)
  data <- dplyr::left_join(data,
                           nlpcam::dep_data %>%
                             dplyr::select(name = nome,
                                    party = siglaPartido,
                                    state = siglaUf,
                                    legislature = idLegislatura),
                           by = c("name", "legislature"))

  return(data)

  }




