library(dplyr)
library(purrr)
library(stringr)
output <- function(articles){
  endcit<- "</cite></p>"
  begcit<- "<p><cite>"
  for(i in 1:length(articles)){
    out <-  
      capture.output(print(articles[i], 
                           .opts = list(style = "html",
                                        bib.style = "authoryear",
                                        max.names =10,dashed=FALSE) )) %>%
      paste(collapse = "") %>%
      {substr(., nchar(begcit)+1,nchar(.))} %>% # remore the <cite>
      paste("<p>",.) %>%
      {substr(., 1,nchar(.)-nchar(endcit))}
    if(!is.null(articles[i]$customb)){
      out <- paste(out, "<a href=",articles[i]$customb,">[data/code]</a>")
    }
    cat(out)
  }
}

as_nice_tbl <- function(bib) { 
  # just last names:
  author <- map_chr(bib, ~ paste0(unlist(.x$author$family), collapse= ", "))
  bibdf <- as.data.frame(bib) %>%
    as_tibble()
  
  bibdf <- bibdf %>% select(any_of(c("year", "title", "journal", "keywords", "customb", "eprint", "url")))
  colnames(bibdf)[colnames(bibdf) %in% "customb"] <- "data/code"
    
  bibdf <- bibdf %>% #select(year, title, journal, keywords, `data/code` = customb, eprint, doi) %>%
    mutate(author = author) %>%
    select(year, author, everything()) %>%
    arrange(desc(year), author) %>%
    #removes the NAs that are problematic for DT
    mutate(across(where(is.character), function(x) ifelse(is.na(x),"",x) )) %>%
    mutate( title = map_chr(title, function(x){
      #in brackets with special capitalization    
      inbrackets  <- str_extract_all(x, "\\{.*?\\}")[[1]] 
      # changes capitalization
      x <- str_to_sentence(x)
      if(length(inbrackets)>0){
        # puts back the stuff with special capitalization    
        inbrackets_new  <- str_extract_all(x, "\\{.*?\\}")[[1]]
        names(inbrackets) <- str_replace(inbrackets_new,"\\{","\\\\{") %>%
          str_replace("\\}","\\\\}") 
        x <- stringr::str_replace_all(x,  inbrackets)
      }
      x %>% 
        str_remove_all("\\{") %>%
        str_remove_all("\\}")
    })) %>%
    # add the doi http so that it works as a link
  #   mutate(doi = ifelse(length(doi)>1 && !str_detect(doi, "https://doi.org/"),
  #                       paste0("https://doi.org/",doi),
  #                       doi
  #   ))
  # %>%
    mutate(across(where(is.character), 
                  function(x) str_replace_all(x,"\\{\\\\&\\}","&")  ))
  bibdf
}  

datatable_papers <- function(bibdf){
  text <- which(colnames(bibdf) %in% c("author", "keywords")) - 1
  title <- which(colnames(bibdf) %in% c("title"  )) - 1
  links <- which(colnames(bibdf) %in% c("data/code", "eprint", "doi", "url")) - 1
  
datatable(bibdf, rownames = FALSE, options = list(pageLength = 5, 
                                                        autoWidth = TRUE,
                                                  columns = list(list(width = "10px"),
                                                                 list(width = "10px"),
                                                                 list(width = "200px")),
                                                        columnDefs = list(
                                                          list(
                                                            targets = title,
                                                            render = JS(
                                                              "function(data, type, row, meta) {",
                                                              "return type === 'display' && data.length > 200 ?",
                                                              "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                                                              "}")
                                                          ),
                                                          list(
                                                            targets = text,
                                                            render = JS(
                                                              "function(data, type, row, meta) {",
                                                              "return type === 'display' && data.length > 30 ?",
                                                              "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                              "}")
                                                          ),
                                                          list(
                                                            targets =   links,
                                                            render = JS(
                                                              "function(data, type, row, meta) {",
                                                              "return type === 'display' && data.length > 1  ?",
                                                              "'<a href=\"' + data + '\">' + '[link]' + '</a>' : data;",
                                                              "}"),
                                                            width = "5px"
                                                          )
                                                        )
                                                        
))
}

