get_ameco <- function(variables, countries = NULL, years = NULL) {

    if (is.null(countries)) {
        countries <- c("EU27", "EU15", "EA19", "EA20", "EA12", "DU15", "DA12", "BEL",
                       "BGR", "CZE", "DNK", "DEU", "D_W", "EST", "IRL", "GRC", "ESP",
                       "FRA", "HRV", "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT",
                       "NLD", "AUT", "POL", "PRT", "ROM", "SVN", "SVK", "FIN", "SWE",
                       "GBR", "ALB", "MNE", "MKD", "SRB", "TUR", "ISL", "NOR", "CHE",
                       "AUS", "CAN", "JPN", "KOR", "MEX", "NZL", "USA", "CU15", "CA12",
                       "FA19", "FA20", "FU27")

    }

    get_data_by_country <- function(country_code) {
        if (is.null(years)) {
            url <- paste0("https://ec.europa.eu/economy_finance/ameco/wq/series?fullVariable=",
                          paste(variables, collapse = ","),
                          "&Countries=", country_code,
                          "&years=1960&Lastyear=1&Yearorder=ASC")
        }else{
            url <- paste0("https://ec.europa.eu/economy_finance/ameco/wq/series?fullVariable=",
                          paste(variables, collapse = ","),
                          "&Countries=", country_code,
                          "&years=", paste(years, collapse = ","),
                          "&Lastyear=0&Yearorder=ASC")
        }

        data_c <- rvest::read_html(url) %>%
            rvest::html_table() %>%
            first()

        if (nrow(data_c) == 0) {
            return(NULL)
        }else{
            return(data_c)
        }


    }

    data <- map_df(
        countries,
        get_data_by_country
    ) %>%
        pivot_longer(-(1:3), names_to = "year") %>%
        mutate_at("year", as.numeric) %>%
        drop_na()

    return(data)

}
