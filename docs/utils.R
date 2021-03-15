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
