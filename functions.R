################################################################################
# GOAL                                                                         #
################################################################################

# The goal of this script is to indicate all functions used for the analysis.

## get_county
get_county <- function(df,
                       County = "New Castle",
                       State = "Delaware") {
  
  if (County == "All counties") {
    
    df %>% 
      filter(Province_State == State)
    
  } else {
    
    df %>% 
      filter(Admin2 == County,
             Province_State == State)
    
  }
  
}

## tidy_covid
tidy_covid <- function(df) {
  
  df %>% 
    pivot_longer(cols = matches("^[0-9]+"),
                 names_to = "Date",
                 values_to = "Count")  %>% 
    pivot_wider(names_from = Type,
                values_from = Count) %>% 
    mutate(Date = lubridate::mdy(Date))  %>% 
    select(-Population, everything(), Population) %>% 
    mutate(across(c(Confirmed, Deaths), ~ . / Population, .names = "{.col}_norm")) %>% 
    group_by(Admin2, Province_State) %>% 
    mutate(across(c(Confirmed, Deaths), ~ lag_diff(., default = 0), .names = "{.col}_DoD")) %>% 
    ungroup()
  
}

## lag_diff
lag_diff <- function(column,
                     ...) {
  
  column - lag(column, ...)
  
}


## graph_covid_over_time
graph_covid_over_time <- function(df,
                                  field = Confirmed){
  
  df %>% 
    ggplot(aes(x = Date,
               y = {{ field }},
               color = Admin2,
               group = Admin2)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "top",
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "italic")) +
    labs(color = "")
  
}