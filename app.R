library(tidyverse)
library(rvest)
library(janitor)

ui <- fluidPage(
  textInput("caption", "paste fbref link here:", "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo"),
  verbatimTextOutput("value")
)

server <- function(input, output) {
  
  get_page <- function(link){
    
    page <- read_html(link)
    
  }
  
  get_name <- function(page){
    name <- page %>% html_element("h1") %>% html_text()
    
  }
  
  get_player_stats <- function(page){
    
    tables <- page %>% html_table()
    
    if(length(tables) == 11){
      
      
      standard <- tables[[3]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      shooting <- tables[[4]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      passing <- tables[[5]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      pass_types <- tables[[6]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      defense <- tables[[8]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      possession <- tables[[9]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      misc <- tables[[11]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      
      
    }
    if(length(tables) == 13){
      
      
      standard <- tables[[5]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      shooting <- tables[[6]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      passing <- tables[[7]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      pass_types <- tables[[8]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      defense <- tables[[10]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      possession <- tables[[11]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      
      misc <- tables[[13]] %>% janitor::row_to_names(1) %>% clean_names() %>% mutate(age = as.numeric(age)) %>% drop_na(age)
      

      
    }
    standard %>% 
      left_join(shooting, by = c("season", "age", "squad", "comp", "lg_rank", "x90s")) %>%
      left_join(passing, by = c("season", "age", "squad", "comp", "lg_rank", "x90s")) %>%
      left_join(pass_types, by = c("season", "age", "squad", "comp", "lg_rank", "x90s")) %>%
      left_join(misc, by = c("season", "age", "squad", "comp", "lg_rank", "x90s")) %>% 
      left_join(defense, by = c("season", "age", "squad", "comp", "lg_rank", "x90s")) %>% 
      left_join(possession, by = c("season", "age", "squad", "comp", "lg_rank", "x90s")) %>%
      mutate(fpts = as.numeric(kp) + 0.1 * as.numeric(won) + as.numeric(sh.y) + 0.2 * as.numeric(recov) + 0.25 * as.numeric(clr) +
               0.5 * as.numeric(succ.y) + 0.5 * as.numeric(fld) + 0.03 * as.numeric(crs.y) - 0.25 * as.numeric(int.x) +
               10 * as.numeric(gls.x) + 0.03 * as.numeric(cmp.x) - 6 * as.numeric(pk.x) - 3 * as.numeric(crd_r.x) +
               as.numeric(sh.x) + as.numeric(so_t) + 3 * 0.33 * as.numeric(mp) + as.numeric(tkl_w.y) + as.numeric(int) +
               0.02 * as.numeric(x1_3.y) + 6 * as.numeric(ast.x),
             fppg = fpts / as.numeric(mp),
             fpts90 = 90 * fpts / as.numeric(min %>% str_remove_all(",")),
             mpg = as.numeric(min %>% str_remove_all(","))/as.numeric(mp)) %>% 
      select(1, 2, 3, mp, mpg, fpts, fpts90, fppg) %>%
      mutate(across(mpg:fpts, round),
             across(fpts90:fppg, ~round(., 1))) %>% 
      rename(gp = mp) %>% 
      drop_na() %>% 
      # mutate(squad = "hidden") %>% 
      knitr::kable(format = "markdown", caption = get_name(page)) %>%
      {.}
    
  }
  
  tableInput <- reactive({
    
    page <- get_page(input$caption)
    # name <- get_name(page)
    table <- get_player_stats(page)
    
    # txt <- paste0(name, table)
    
  })

  output$value <- renderPrint({ print(tableInput()) })
}
shinyApp(ui, server)
