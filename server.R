library(ggridges)
library(nflfastR)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(reactable)
library(reactablefmtr)
library(tidyverse)
library(dplyr)
library(gtExtras)
library(rsconnect)
library(gtable)
library(gt)
library(magick)
library(ggplot2)
library(ggimage)
library(shiny)
nine <- read_csv("war20.csv", col_names = T)
names(nine)[names(nine) == "...3"] <- " "


# Define server logic required to draw a histogram
function(input, output, session) {

  observeEvent(input$update, {
    
    y1 = paste("20", input$years[1], sep = "")
    y2 = paste("20", input$years[2], sep = "")
    year = paste(y1, y2, sep = ":")
    rbs <- nflfastR::load_pbp(as.integer(y1):as.integer(y2)) %>%
      dplyr::filter(week <= 18) %>%
      nflfastR::calculate_player_stats() %>%
      dplyr::mutate(
        ppt = (fantasy_points_ppr - (as.integer(input$ppr)*receptions)) / games,
        touches = carries+receptions
      ) %>%
      filter(games > as.integer(input$min)) %>%
      filter(touches > 0) %>% 
      # only keep the rbs
      inner_join(
        nflfastR::fast_scraper_roster(as.integer(y2)) %>% filter(position == "RB") %>% select(gsis_id, headshot_url),
        by = c("player_id" = "gsis_id")
      ) %>%
      dplyr::arrange(-ppt) %>% 
      left_join(nflfastR::teams_colors_logos, by = c('recent_team' = 'team_abbr'))
    
    rb_mean = mean(rbs$ppt[1:30])
    rb_sd = sd(rbs$ppt[1:30])
    
    wrs <- nflfastR::load_pbp(as.integer(y1):as.integer(y2)) %>%
      dplyr::filter(week <= 18) %>%
      nflfastR::calculate_player_stats() %>%
      dplyr::mutate(
        ppt = (fantasy_points_ppr - (as.integer(input$ppr)*receptions)) / games,
        touches = carries+receptions
      ) %>%
      filter(games > as.integer(input$min)) %>%
      filter(receptions > 0) %>% 
      # only keep the WRs
      inner_join(
        nflfastR::fast_scraper_roster(as.integer(y2)) %>% filter(position == "WR" | position == "WR") %>% select(gsis_id, headshot_url),
        by = c("player_id" = "gsis_id")
      ) %>%
      dplyr::arrange(-ppt) %>% 
      left_join(teams_colors_logos, by = c('recent_team' = 'team_abbr'))
    
    
    
    wr_mean = mean(wrs$ppt[1:30])
    wr_sd = sd(wrs$ppt[1:30])
    
    qbsw <- nflfastR::load_pbp(as.integer(y1):as.integer(y2)) %>%
      dplyr::filter(week <= 18) %>%
      nflfastR::calculate_player_stats() %>%
      dplyr::mutate(
        ppt = fantasy_points_ppr / games,
        touches = carries+receptions+attempts
      ) %>%
      filter(games > as.integer(input$min)) %>%
      # only keep the WRs
      inner_join(
        nflfastR::fast_scraper_roster(as.integer(y2)) %>% filter(position == "QB") %>% select(gsis_id, headshot_url),
        by = c("player_id" = "gsis_id")
      ) %>%
      dplyr::arrange(-ppt) %>% 
      left_join(teams_colors_logos, by = c('recent_team' = 'team_abbr'))
    
    qb_mean = mean(qbsw$ppt[1:30])
    qb_sd = sd(qbsw$ppt[1:30])
    
    
    tes <- nflfastR::load_pbp(as.integer(y1):as.integer(y2)) %>%
      dplyr::filter(week <= 18) %>%
      nflfastR::calculate_player_stats() %>%
      dplyr::mutate(
        ppt = (fantasy_points_ppr - (as.integer(input$ppr)*receptions)) / games,
        touches = carries+receptions
      ) %>%
      filter(games > as.integer(input$min)) %>% 
      filter(touches > 0) %>%
      # only keep the tes
      inner_join(
        nflfastR::fast_scraper_roster(as.integer(y2)) %>% filter(position == "TE") %>% select(gsis_id, headshot_url),
        by = c("player_id" = "gsis_id")
      ) %>%
      dplyr::arrange(-ppt) %>% 
      left_join(teams_colors_logos, by = c('recent_team' = 'team_abbr'))
    
    
    te_mean = mean(tes$ppt[1:30])
    te_sd = sd(tes$ppt[1:30])
    pbp <- nflfastR::load_pbp(as.integer(y1):as.integer(y2))
    ret <- pbp %>% 
      filter(week <= 18) %>% 
      group_by(posteam) %>% 
      mutate(tdpunt = ifelse(touchdown & (punt_attempt | kickoff_attempt), 1, 0)) %>% 
      summarise(
        return_yards = sum(return_yards, na.rm =T),
        tdp = sum(tdpunt, na.rm=T)
      ) 
      
   
    def <- pbp %>% 
      filter(week <= 18) %>% 
      group_by(defteam) %>% 
      mutate(def_td = ifelse(touchdown & defteam_score_post > defteam_score, 1, 0)) %>% 
      summarise(
        games = last(week)*(as.integer(y2)-as.integer(y1)+1),
        sacks = sum(sack, na.rm =T),
        ints = sum(interception, na.rm=T),
        fumbles_forced = sum(fumble_forced, na.rm=T),
        fumbles = sum(fumble_lost, na.rm=T),
        tds = sum(def_td, na.rm=T),
        pts_all = last(posteam_score_post)
      ) 
    
    def$ptallpt <- ifelse(def$pts_all == 0, 5, ifelse(def$pts_all >= 1 & def$pts_all <= 6, 4, ifelse(def$pts_all >= 7 & def$pts_all <= 13, 3, ifelse(def$pts_all >= 14 & def$pts_all <= 17, 1,ifelse(def$pts_all >= 18 & def$pts_all <= 27, 0,ifelse(def$pts_all >= 28 & def$pts_all <= 34, -1,ifelse(def$pts_all >= 35 & def$pts_all <= 45, -3,-5)))))))
    
    def <- def %>% 
      left_join(ret, by = c("defteam" = "posteam")) %>% 
      left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>% 
      drop_na(defteam)
    
    def$pts <- (def$sacks + 2*def$ints + def$fumbles_forced + 2*def$fumbles + 6*def$tds + def$ptallpt +(def$return_yards/25) + 6*def$tdp) / def$games
    
    def_mean <- mean(def$pts)
    def_sd <- sd(def$pts)
    
    mean_team <- as.integer(input$rbs)*rb_mean + as.integer(input$qbs)*qb_mean + as.integer(input$wrs)*wr_mean + te_mean + def_mean + as.integer(input$flex)*((rb_mean+wr_mean)/2)
    team_sd = sqrt(as.integer(input$rbs)*rb_sd**2 + as.integer(input$wrs)*wr_sd**2 + as.integer(input$qbs)*qb_sd**2 + te_sd**2 + def_sd**2 + as.integer(input$flex)*((rb_sd + wr_sd)/2)**2)
    rbs$win_percent <- 100*round((pnorm((rbs$ppt + (mean_team - rb_mean)), mean_team, team_sd)), 50)
    qbsw$win_percent <- 100*round((pnorm((qbsw$ppt + (mean_team - qb_mean)), mean_team, team_sd)),50)
    wrs$win_percent <- 100*round((pnorm((wrs$ppt + (mean_team - wr_mean)), mean_team, team_sd)),50)
    tes$win_percent <- 100*round((pnorm((tes$ppt + (mean_team - te_mean)), mean_team, team_sd)),50)
    def$win_percent <- 100*round((pnorm((def$pts + (mean_team - def_mean)), mean_team, team_sd)),50)
    
    rbs$WAR <- (rbs$win_percent  - rbs$win_percent[30])/100*13
    qbsw$WAR <- (qbsw$win_percent  - qbsw$win_percent[20])/100*13
    wrs$WAR <- (wrs$win_percent  - wrs$win_percent[30])/100*13
    tes$WAR <- (tes$win_percent  - tes$win_percent[20])/100*13
    def$WAR <- (def$win_percent  - def$win_percent[16])/100*13
    
    
    qbsw$rank <- NA
    order.scores<-order(-qbsw$WAR,qbsw$player_name)
    qbsw$rank[order.scores] <- 1:nrow(qbsw)
    
    wrs$rank <- NA
    order.scores<-order(-wrs$WAR,wrs$player_name)
    wrs$rank[order.scores] <- 1:nrow(wrs)
    
    rbs$rank <- NA
    order.scores<-order(-rbs$WAR,rbs$player_name)
    rbs$rank[order.scores] <- 1:nrow(rbs)
    
    tes$rank <- NA
    order.scores<-order(-tes$WAR,tes$player_name)
    tes$rank[order.scores] <- 1:nrow(tes)
    
    def$rank <- NA
    order.scores<-order(-def$WAR,def$defteam)
    def$rank[order.scores] <- 1:nrow(def)
    
    
    qbsw$Pos <- "QB"
    tes$Pos <- "TE"
    rbs$Pos <- "RB"
    wrs$Pos <- "WR"
    def$Pos <- "D/ST"
    
    
   
    
    
    players <- rbind(qbsw, wrs, rbs, tes)
    rm(rbs)
    rm(wrs)
    rm(tes)
    rm(qbsw)
    players <- players %>% select(recent_team, player_name, WAR, win_percent, rank, headshot_url.x, Pos, games)
    def <- def %>% select(team_name, defteam, Pos, WAR, win_percent, rank, team_logo_espn, games)
    names(def)[names(def) == "team_name"] <- "player_name"
    names(def)[names(def) == "team_logo_espn"] <- "headshot_url.x"
    names(def)[names(def) == "defteam"] <- "recent_team"
    players <- rbind(players, def)
    players$rank1 <- NA
    order.scores<-order(-players$WAR,players$player_name)
    players$rank1[order.scores] <- 1:nrow(players)
    
    
    
    eight <- players %>% 
      select(rank1, rank, headshot_url.x, Pos, player_name, recent_team, WAR, win_percent, games)
    
    eight$` ` <- eight$headshot_url.x
    names(eight)[names(eight) == "rank"] <- "Pos. Rank"
    names(eight)[names(eight) == "rank1"] <- "Ovr. Rank"
    names(eight)[names(eight) == "win_percent"] <- "Avg. Win %"
    names(eight)[names(eight) == "player_name"] <- "Player"
    names(eight)[names(eight) == "recent_team"] <- "Team"
    names(eight)[names(eight) == "games"] <- "Games"
    
    
    
    eight <- eight %>% 
      select(-headshot_url.x) %>% 
      arrange(`Ovr. Rank`)
    eight <- eight[c(1,2,9,4,3,5,8,6,7)]
    eight$`Avg. Win %` <- round((eight$`Avg. Win %`), 1)
    eight$`WAR` <- round((eight$`WAR`), 1)
    eight$`$ Value` <- round((eight$WAR * 20.17336),2)
    #print("four")
    nine<-eight
    
    output$wartab <- renderReactable({
      tab <- reactable(nine,
                       showSortIcon = FALSE,
                       theme = cyborg(),
                       searchable = FALSE,
                       filterable = TRUE,
                       language = reactableLang(
                         searchPlaceholder = "SEARCH FOR PLAYER...", filterPlaceholder = "..."),
                       columns = list(
                         ` ` = colDef(
                           name = " ",
                           align = "center",
                           maxWidth = 50,
                           cell = embed_img(height = "30", width = "40")
                         ),
                         `Ovr. Rank` = colDef(show = T, maxWidth = 50,),
                         `Pos. Rank` = colDef(show = T, maxWidth = 50,),
                         `Player` = colDef(show = T, maxWidth = 170,),
                         `Team` = colDef(show = T, maxWidth = 50,),
                         `Pos` = colDef(show = T, maxWidth = 50, filterable = T),
                         `Avg. Win %` = colDef(
                           show = T,
                           maxWidth = 75,
                           style = color_scales(nine, colors = c("#fd84a9", "white", "#42c2ca"))
                         ),
                         `$ Value` = colDef(
                           show = T,
                           maxWidth = 75,
                           style = color_scales(nine, colors = c("brown1", "white", "chartreuse4"))
                         ),
                         `WAR` = colDef(
                           show = T,
                           maxWidth = 75,
                           style = color_scales(nine, colors = c("#fd84a9", "white", "#42c2ca")))
                       ),
                       fullWidth = F
      ) %>% add_source("By: Samuel DiSorbo @analytacist with {reactablefmtr} •  Data: nflfastR", font_size = 12)
    })#wartab
    
    
  })#end
  
  


}
