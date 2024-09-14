search_ameco <- function(expresion) {
    data <- ameco::ameco_series_df %>%
        filter(str_detect(str_to_lower(title), str_to_lower(expresion)) | str_detect(str_to_lower(series), str_to_lower(expresion)))
    return(data)
}
