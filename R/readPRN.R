

#' read metadata from prn balance output of delwaq model
#' @param filename the balance file to be read (*.prn)
#' @return A dataframe with metadata
read_balance_prn_metadata <- function(filename) {
  if(!grepl(".prn", filename)) {stop; print("file is not a *.prn file")}
  require(tidyverse)
  all_data <- readr::read_lines(filename)
  rep_type_entries = grep("^MASS BALANCE PER ", all_data)
  rep_location_entries = grep("^Mass balances for ", all_data)
  rep_substance_entries = grep("^Substance ", all_data)
  rep_SUM_OF_ALL_TERMS = grep("^SUM OF ALL TERMS", all_data)
  types <- stringr::str_remove(all_data[rep_type_entries], "MASS BALANCE PER ")
  locations <- stringr::str_remove(all_data[rep_location_entries], "Mass balances for ")
  substances <- str_split(all_data[rep_substance_entries], " +", simplify = T)[,2]
  # substancelines <- grep

  ntypes <- length(types)
  nlocs <- length(locations)
  nsubs <- length(substances)

  metadf <- data.frame(
    massBalanceType = rep(types, each = nsubs/ntypes),
    location = rep(locations, each = nsubs/nlocs),
    substance = substances,
    skipLines =  rep_substance_entries + 1,
    NoOfLines = rep_SUM_OF_ALL_TERMS - rep_substance_entries -1,
    stringsAsFactors = FALSE
    #   n = integer())
  )
  return(metadf)
}



#' read complete prn balance file into vector
#' @param filename the balance file to be read (*.prn)
#' @return A vector containing *.prn data lines
read_prn_all <- function(filename) {
  require(tidyverse)
  if(!grepl(".prn", filename)) {stop; print("file is not a *.prn file")}
  all_data <- readr::read_lines(filename)
  return(all_data)
}

#' read selected data into dataframe from prn balance file
#' @param metadata metadata of the balance file as obtained with read_prn_metadata
#' @param all_data vectorized balance file as read with read_prn_all()
#' @param myType type of balance ("DUMPAREA", "SURFACE", "VOLUME")
#' @param myLocation vector of monitoring points or areas from metadata
#' @param mySubstance vector of substances from metadata
#' @return A dataframe containg all balances
read_prn_data <- function(metadf, all_data, myType, myLocation, mySubstance, sum = FALSE) {
  require(tidyverse)
  if(!grepl(".prn", filename)) {stop; print("file is not a *.prn file")}
  selection <- metadf %>%
    dplyr::filter(massBalanceType %in% myType, location %in% myLocation, substance %in% mySubstance)

  sel <- split(selection, seq(nrow(selection)))

  if(sum){
    res <- lapply(sel,
                  function(x)
                    read_fwf(all_data, trim_ws = T,
                             col_positions = fwf_widths(c(30,15,15),
                                                        col_names = c("process", "Source", "Sink")#,
                                                        # col_types = cols(process = col_character(),
                                                        #                  process = col_double(),
                                                        #                  process = col_double()
                                                        #                  )
                                                        ),
                             skip =  x[,"skipLines"],      #as.integer(x$skipLines),
                             n_max =  x[,"NoOfLines"]         #as.integer(x$NoOfLines)
                    ) %>%
                    mutate(process = sub("SUM OF ALL TERMS", paste(mySubstance, "SUM OF ALL TERMS", sep = "_"), process)) %>%
                    merge(x)
    )
  } else {
    res <- lapply(sel,
                  function(x)
                    read_fwf(all_data, trim_ws = T,
                             col_positions = fwf_widths(c(30,15,15), c("process", "Source", "Sink")),
                             skip =  x[,"skipLines"],      #as.integer(x$skipLines),
                             n_max =  x[,"NoOfLines"] - 1     #as.integer(x$NoOfLines)
                    ) %>%
                    merge(x)
    )
  }
  res <-  res %>%
    bind_rows() %>%
  mutate(process = str_split(string = process, pattern = "_", simplify = T)[,2])

  return(res)
}



plotFluxes <- function(prn_data){
  require(ggplot2)
  prn_data %>%
    ungroup() %>%
    dplyr::select(-skipLines, -NoOfLines) %>%
    mutate(process = as.factor(process)) %>%
    gather(key = "balance", value = "value", -massBalanceType, -location, - substance, -process) %>%
    group_by(location) %>%
    ggplot(aes(process, value)) +
    geom_col(aes(fill = substance), position = "dodge") +
    coord_flip() +
    facet_grid(location ~ .) +
    ggplot2::labs(subtitle=unique(prn_data$massBalanceType))

}

