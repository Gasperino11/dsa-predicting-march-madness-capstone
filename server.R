library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(grid)
library(gridExtra)

#Import data
tourneyCompact <- read.csv("data/MNCAATourneyCompactResults.csv")
tourneyDetailed <- read.csv("data/MNCAATourneyDetailedResults.csv")
tourneySeeds <- read.csv("data/MNCAATourneySeeds.csv")
tegularSeasonCompact <- read.csv("data/MRegularSeasonCompactResults.csv")
tegularSeasonDetailed <- read.csv("data/MRegularSeasonDetailedResults.csv")
teams <- read.csv("data/MTeams.csv")
ncaa_tourney <- read.csv("data/ncaa_tourney.csv", stringsAsFactors = FALSE)
regular_season <- read.csv("data/regular_season.csv", stringsAsFactors = FALSE)
regular_season_full <- read.csv("data/seasonResultsFull.csv")
advanced_stats <- read.csv("data/seasonResultsSummarized.csv")
PFB <- read.csv("data/vegas.csv",as.is=TRUE, header=TRUE)
data_dictionary <- read.csv("data/datadictionary.csv",header=TRUE,encoding="UTF-8")

#data manipulation for vegas data
names(PFB) <- c("line","games","fave_win","fave_win_per")
PFB_list <- c("All", PFB$line)
FWP_list <- c(PFB$fave_win_per)
PFB_newdata <- subset(PFB, select = c("line", "fave_win_per"))

#Remap tourney Seed data to be more descriptive
tourneySeeds$Seed_num <- as.numeric(gsub("([^0-9]+)","",tourneySeeds$Seed))

#setting some color values
loss_made = "#F8766D"
win_made = "#619CFF"
loss_att = "#00BA38"
win_att = "#C77CFF"

#color assigner function based on correlation
color_corr_check <- function(var1,var2){
  if(cor(var1,var2) > 0){
    paste("#619CFF")
  } else if(cor(var1,var2) < 0){
    paste("#F8766D")
  } else if(cor(var1,var2) == 0){
    paste("#C77CFF")
  }
}

# Define server logic 
shinyServer(
  function(input, output, session) {
    
    #Conference Selector for historical regular season data
    output$conf_select <- renderUI({
      selectizeInput("conf_selector",
                     "Conferences Shown:",
                     multiple=TRUE,
                     choices = sort(unique(union(unique(ncaa_tourney$WConf),unique(ncaa_tourney$LConf)))),
                     selected= c("Southeastern Conference","Big 12 Conference")
                     )
    })
    
    #Seed selector Filter for Seed Win Ratio Plot
    output$seed_select <- renderUI({
      checkboxGroupInput("seed_selector",
                    "Select Seeds to Compare:",
                    choices=unique(tourneySeeds$Seed_num),
                    selected=c(1))
    })
    
    #Logic to update the seed selector if the Select All/Deselect All box is checked
    observe({
      updateCheckboxGroupInput(session,
                               "seed_selector",
                               choices = unique(tourneySeeds$Seed_num),
                               selected = if(input$seed_select_all) unique(tourneySeeds$Seed_num))
    })

    #Tourney wins visualization by team
    output$tourney_wins <- renderPlot({
      ncaa_tourney %>%
        filter(WConf %in% input$conf_selector) %>%
        group_by(WTeamName) %>%
        summarise(cnt = n()) %>%
        filter(cnt >= 1) %>%
        ggplot(aes(x = reorder(WTeamName,cnt), y = cnt)) +
        geom_bar(stat = "identity",fill="#619CFF") +
        geom_text(aes(label=cnt),hjust=1.25,color="white") +
        coord_flip() +
        ylab("Wins") + 
        xlab("School") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_text(size=14,face="bold")
        )
    })
    
    #top 25 teams by wins
    output$tourney_25_wins <- renderPlot({
      ncaa_tourney %>%
        group_by(WTeamName) %>%
        summarize(cnt = n ()) %>%
        top_n(25) %>%
        ggplot(aes(x = reorder(WTeamName,cnt), y = cnt)) +
        geom_bar(stat = "identity",fill="#619CFF") +
        geom_text(aes(label=cnt),hjust=1.25,color="white") +
        coord_flip() +
        ylab("Wins") + 
        xlab("School") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_text(size=14,face="bold")
        )
    })
    
    #Tourney losses by team
    output$tourney_losses <- renderPlot({
      ncaa_tourney %>%
        filter(LConf %in% input$conf_selector) %>%
        group_by(LTeamName) %>%
        summarise(cnt = n()) %>%
        filter(cnt >= 1) %>% 
        ggplot(aes(x = reorder(LTeamName, cnt), y = cnt)) + 
        geom_bar(stat = "identity",fill="#F8766D") +
        geom_text(aes(label=cnt),hjust=1.25,color="white") +
        coord_flip() + 
        ylab("Losses") + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
    })
    
    #top 25 team by losses
    output$tourney_25_losses <- renderPlot({
      ncaa_tourney %>%
        filter(LConf %in% input$conf_selector) %>%
        group_by(LTeamName) %>%
        summarise(cnt = n()) %>%
        top_n(25) %>% 
        ggplot(aes(x = reorder(LTeamName, cnt), y = cnt)) + 
        geom_bar(stat = "identity",fill="#F8766D") +
        geom_text(aes(label=cnt),hjust=1.25,color="white") +
        coord_flip() + 
        ylab("Losses") + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
    })
    
    
    #Tourney win/loss ratio
    output$tourney_ratio <- renderPlot({
      winners <- ncaa_tourney %>%
        filter(WConf %in% input$conf_selector) %>%
        group_by(WTeamName) %>%
        summarise(wins = n())
      
      losers <- ncaa_tourney %>%
        filter(LConf %in% input$conf_selector) %>%
        group_by(LTeamName) %>%
        summarise(losses = n())
      
      plot_df <- left_join(winners,losers,by=c("WTeamName"="LTeamName")) %>%
        rename(TeamName = WTeamName) %>%
        mutate(ratio = wins/losses)
      
      plot_df %>%
        filter(ratio > 0) %>% 
        ggplot(aes(x = reorder(TeamName, ratio), y = ratio)) + 
        geom_bar(stat = "identity",fill="#00BA38") + 
        geom_text(aes(label=round(ratio,2)),hjust=1.25,color="white") +
        coord_flip() + 
        ylab("Win/Loss Ratio") + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
    })
    
    #Tourney win/loss ratio top 25 teams
    output$tourney_25_ratio <- renderPlot({
      winners <- ncaa_tourney %>%
        group_by(WTeamName) %>%
        summarise(wins = n())
      
      losers <- ncaa_tourney %>%
        group_by(LTeamName) %>%
        summarise(losses = n())
      
      plot_df <- left_join(winners,losers,by=c("WTeamName"="LTeamName")) %>%
        rename(TeamName = WTeamName) %>%
        mutate(ratio = wins/losses) %>%
        select(c(TeamName,ratio))
      
      plot_df %>%
        top_n(25) %>% 
        ggplot(aes(x = reorder(TeamName, ratio), y = ratio)) + 
        geom_bar(stat = "identity",fill="#00BA38") + 
        geom_text(aes(label=round(ratio,2)),hjust=1.25,color="white") +
        coord_flip() + 
        ylab("Win/Loss Ratio") + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
    })
    
    #Tourney wins by seed
    output$tourney_seed_wins <- renderPlot({
      ncaa_tourney %>%
        group_by(WSeed_num) %>%
        filter(WSeed_num %in% input$seed_selector) %>%
        summarise(cnt = n()) %>%
        ggplot(aes(x = reorder(WSeed_num, cnt), y = cnt)) +
        geom_bar(stat = "identity",fill="#619CFF") +
        geom_text(aes(label=cnt),hjust=1.25,color="white") +
        coord_flip() +
        ylab("Wins") + 
        xlab("Seed") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_text(size=14,face="bold")
        )
    })
    
    #Tourney losses by seed
    output$tourney_seed_losses <- renderPlot({
      ncaa_tourney %>%
        group_by(LSeed_num) %>%
        filter(LSeed_num %in% input$seed_selector) %>%
        summarise(cnt = n()) %>%
        filter(cnt >= 10) %>%
        ggplot(aes(x = reorder(LSeed_num, cnt), y = cnt)) +
        geom_bar(stat = "identity",fill="#F8766D") +
        geom_text(aes(label=cnt),hjust=1.25,color="white") +
        coord_flip() +
        ylab("Losses") + 
        xlab("Seed") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_text(size=14,face="bold")
        )
    })
    
    #win ratio for each seed at each round of the tournament
    output$seed_plot <- renderPlot({
      #remap tourney round to descriptive labels
      tourneyCompactResults_desc <- tourneyCompact %>%
        mutate(TourneyRound_desc = ifelse(DayNum %in% 134:135, "Play-In",
                                          ifelse(DayNum %in% 136:137, "Round of 64",
                                                 ifelse(DayNum %in% 138:139, "Round of 32",
                                                        ifelse(DayNum %in% 143:144, "Sweet Sixteen",
                                                               ifelse(DayNum %in% 145:146, "Elite Eight",
                                                                      ifelse(DayNum == 152, "Final Four",
                                                                             ifelse(DayNum == 154, "National Championship","Unknown")))))))
        ) %>%
        mutate(TourneyRound_num = ifelse(DayNum %in% 134:135, 0,
                                         ifelse(DayNum %in% 136:137, 1,
                                                ifelse(DayNum %in% 138:139, 2,
                                                       ifelse(DayNum %in% 143:144, 3,
                                                              ifelse(DayNum %in% 145:146, 4,
                                                                     ifelse(DayNum == 152, 5,
                                                                            ifelse(DayNum == 154, 6,-1)))))))
        )
      
      
      #map wins to single dataframe
      plot_data_wins <- tourneyCompactResults_desc %>%
        select(c(Season,TourneyRound_desc,TourneyRound_num,WTeamID)) %>%
        rename(TeamID = WTeamID) %>%
        left_join(tourneySeeds,by=c("Season"="Season","TeamID"="TeamID")) %>%
        select(-c(Seed))
      
      #map losses to single dataframe
      plot_data_losses <- tourneyCompactResults_desc %>%
        select(c(Season,TourneyRound_desc,TourneyRound_num,LTeamID)) %>%
        rename(TeamID = LTeamID) %>%
        left_join(tourneySeeds,by=c("Season"="Season","TeamID"="TeamID")) %>%
        select(-c(Seed))
      
      #map win loss column
      plot_data_wins$win_loss <- 1
      plot_data_losses$win_loss <- 0
      
      #union win_loss dataframes
      plot_data <- union(plot_data_wins,plot_data_losses)
      
      #create final dataframe for plot
      plot_data_grouped <- plot_data %>%
        group_by(TourneyRound_desc,TourneyRound_num,Seed_num) %>%
        summarize(wins = sum(win_loss),appearances = n()) %>%
        mutate(win_ratio = wins/appearances) %>%
        mutate(losses = appearances - wins) %>%
        mutate(loss_ratio = losses/appearances) %>%
        mutate(win_loss_ratio = wins/losses) %>%
        select(-c(appearances))
      
      plot_df <- plot_data_grouped %>% filter(Seed_num %in% input$seed_selector)
        
      #set plot and plot attributes
      seed_plot <- ggplot(plot_df,aes(x=TourneyRound_num,y=win_ratio,color=as.factor(Seed_num)))
      seed_plot <- seed_plot + geom_line(size=1.5)
      seed_plot <- seed_plot + scale_x_continuous(
        breaks = plot_df$TourneyRound_num,
        labels = plot_df$TourneyRound_desc
      )
      seed_plot <- seed_plot + theme_minimal()
      seed_plot <- seed_plot + labs(y="Win Ratio (in Round)",color="Tourney Seed")
      seed_plot <- seed_plot + theme(
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14,face="bold")
      )
      
      seed_plot
    })#end of seet polot
    
    #begin of vegas plot
    output$vegas_plot <- renderPlot({ 
      ggplot(data=PFB, aes(x=line, y=fave_win_per)) +
        geom_point() + 
        scale_y_continuous(breaks = c(50,60,70,80,90,100), limits = c(50, 100)) +
        scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), limits = c(0, 50)) +
        labs(title = "NCAA Men's Basketball Favorites' Winning Percentage: 2003-2018") +
        xlab("Points Favored By (Betting Line)") +
        ylab("Winning Percentage of Favorites") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_text(size=14,face="bold"),
          plot.title = element_text(size=14, face="bold")
          
        ) 
    }) #end of vegas_plot
  
    
    #render data dynamically based on of there is a point that has been clicked
    output$vegas_info <- renderText({
      #get nearest points
      points <- nearPoints(PFB_newdata, input$plot_click, "line", "fave_win_per", threshold = 100, maxpoints = 1)
      if(length(points$line)==0){
        HTML("<span style=color:#F8766D;>No points selected - please click on the plot above.</span>")
      } else {
        HTML("Teams favored by <code>", points$line, "points</code> win <code>", points$fave_win_per,"%</code> of the time.")
      }
    }) #end of vegas_info
    
    #regular season output
    output$regular_season <- renderPlot({
      
      # Points Line Plot
      points <- regular_season %>%
        group_by(Season) %>%
        summarise(Wmean = mean(WScore), Lmean = mean(LScore)) %>%
        gather(avg, value, Wmean:Lmean) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_point() +
        geom_line(size = 1) +
        scale_color_manual(name=NULL,
                           values = c(loss_made, win_made), 
                           labels=c("Loser Average Points","Winner Average Points")) +
        ggtitle("Average Points per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # Field Goals Line Plot
      field_goals <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WFGM = mean(WFGM), Avg_WFGA = mean(WFGA), Avg_LFGM = mean(LFGM), Avg_LFGA = mean(LFGA)) %>%
        gather(avg, value, Avg_WFGM:Avg_LFGA) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_point () + 
        geom_line(size = 1) +
        scale_color_manual(name=NULL,
                           values = c(loss_att, loss_made, win_att, win_made),
                           labels = c("Loser FG Attempted","Loser FG Made","Winner FG Attempted","Winner FG Made")
        ) +
        ggtitle("Average Field Goals Made & Attempted per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      # Three Pointers Line Plot
      three_point <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WFGM3 = mean(WFGM3), Avg_WFGA3 = mean(WFGA3), Avg_LFGM3 = mean(LFGM3), Avg_LFGA3 = mean(LFGA3)) %>%
        gather(avg, value, Avg_WFGM3:Avg_LFGA3) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() +
        scale_color_manual(name=NULL,
                           values = c(loss_att, loss_made, win_att, win_made),
                           labels = c("Loser 3-pointers Attempted","Loser 3-pointers Made",
                                      "Winner 3-pointers Attempted","Winner 3-pointers Made")
        ) +
        ggtitle("Average 3-point Field Goals Made & Attempted per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # Free Throws Line Plot
      free_throw <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WFTM = mean(WFTM), Avg_WFTA = mean(WFTA), Avg_LFTM = mean(LFTM), Avg_LFTA = mean(LFTA)) %>%
        gather(avg, value, Avg_WFTM:Avg_LFTA) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() +
        scale_color_manual(name=NULL,
                           values = c(loss_att, loss_made, win_att, win_made),
                           labels = c("Loser Free Throws Attempted","Loser Free Throws Made",
                                      "Winner Free Throws Attempted","Winner Free Throws Made")
        ) +
        ggtitle("Average Free Throws Made & Attempted per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # Rebounds Line Plot
      rebounds <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WOR = mean(WOR), Avg_WDR = mean(WDR), Avg_LOR = mean(LOR), Avg_LDR = mean(LDR)) %>%
        gather(avg, value, Avg_WOR:Avg_LDR) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() +
        scale_color_manual(name=NULL,
                           values = c(loss_made, loss_att, win_made, win_att),
                           labels = c("Loser Offensive Rebounds","Loser Defensive Rebounds",
                                      "Winner Offensive Rebounds","Winner Defensive Rebounds")) +
        ggtitle("Average Offensive & Defensive Rebounds per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # Steals Line Plot
      steals <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WStl = mean(WStl), Avg_LStl = mean(LStl)) %>%
        gather(avg, value, Avg_WStl:Avg_LStl) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() +
        scale_color_manual(name=NULL,
                           values = c(loss_made, win_made),
                           labels = c("Loser Steals","Winner Steals")
        ) +
        ggtitle("Average Steals per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # Turnovers Line Plot
      turnovers <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WTO = mean(WTO), Avg_LTO = mean(LTO)) %>%
        gather(avg, value, Avg_WTO:Avg_LTO) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() + 
        scale_color_manual(name=NULL,
                           values = c(loss_made, win_made),
                           labels = c("Loser Turnovers","Winner Turnovers")
        ) +
        ggtitle("Average Turnovers per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      #Blocks
      blocks <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WBlk = mean(WBlk), Avg_LBlk = mean(LBlk)) %>%
        gather(avg, value, Avg_WBlk:Avg_LBlk) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() + 
        scale_color_manual(name=NULL,
                           values = c(loss_made, win_made),
                           labels = c("Loser Blocks","Winner Blocks")
        ) +
        ggtitle("Average Blocks per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      #Persnal fouls
      personal_fouls <- regular_season %>%
        group_by(Season) %>%
        summarise(Avg_WPF = mean(WPF), Avg_LPF = mean(LPF)) %>%
        gather(avg, value, Avg_WPF:Avg_LPF) %>%
        ggplot(aes(x= Season, y= value, group = avg, color = avg)) +
        geom_line(size = 1) +
        geom_point() + 
        scale_color_manual(name=NULL,
                           values = c(loss_made, win_made),
                           labels = c("Loser Personal Fouls","Winner Personal Fouls")
        ) +
        ggtitle("Average Personal Fouls per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # chart selector if statement
      if(input$reg_season_chart_select == "Points"){points}
      else if(input$reg_season_chart_select == "Field Goals"){field_goals}
      else if(input$reg_season_chart_select == "3-point Field Goals"){three_point}
      else if(input$reg_season_chart_select == "Free Throws"){free_throw}
      else if(input$reg_season_chart_select == "Rebounds"){rebounds}
      else if(input$reg_season_chart_select == "Steals"){steals}
      else if(input$reg_season_chart_select == "Turnovers"){turnovers}
      else if(input$reg_season_chart_select == "Blocks"){blocks}
      else if(input$reg_season_chart_select == "Personal Fouls"){personal_fouls}
    })
    
    #regular season output
    output$regular_season_corr <- renderPlot({
      
      #points correlation
      points <- ggplot(advanced_stats,aes(x=wins,y=avg_score))
      points <- points + geom_point()
      points <- points + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                    advanced_stats$avg_score))
      points <- points + xlab("Wins")
      points <- points + ylab("Averge Points Scored")
      points <- points + ggtitle("Average Points Scored per Season as Related to Wins that Season")
      points <- points + theme_minimal()
      points <- points +  theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11,face="bold"),
          axis.title.y = element_blank()
      )
      
      #field goals made correlation
      plot_fgm <- ggplot(advanced_stats,aes(x=wins,y=avg_fgm))
      plot_fgm <- plot_fgm + geom_point()
      plot_fgm <- plot_fgm + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_fgm))
      plot_fgm <- plot_fgm + xlab("Wins")
      plot_fgm <- plot_fgm + ylab("Average Field Goals Made")
      plot_fgm <- plot_fgm + ggtitle("")
      plot_fgm <- plot_fgm + theme_minimal()
      plot_fgm <- plot_fgm + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #field goals attempted correlation
      plot_fga <- ggplot(advanced_stats,aes(x=wins,y=avg_fga))
      plot_fga <- plot_fga + geom_point()
      plot_fga <- plot_fga + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_fga))
      plot_fga <- plot_fga + xlab("Wins")
      plot_fga <- plot_fga + ylab("Average Field Goals Attempted")
      plot_fga <- plot_fga + ggtitle("Average Field Goals Attempted per Season as Related to Wins that Season")
      plot_fga <- plot_fga + theme_minimal()
      plot_fga <- plot_fga + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #3-point field goals made correlation
      plot_fgm3 <- ggplot(advanced_stats,aes(x=wins,y=avg_fgm3))
      plot_fgm3 <- plot_fgm3 + geom_point()
      plot_fgm3 <- plot_fgm3 + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                          advanced_stats$avg_fgm3))
      plot_fgm3 <- plot_fgm3 + xlab("Wins")
      plot_fgm3 <- plot_fgm3 + ylab("Average 3-point Field Goals Made")
      plot_fgm3 <- plot_fgm3 + ggtitle("Average 3-point Field Goals Made per Season as Related to Wins that Season")
      plot_fgm3 <- plot_fgm3 + theme_minimal()
      plot_fgm3 <- plot_fgm3 + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #3-point field goals attempted correlation
      plot_fga3 <- ggplot(advanced_stats,aes(x=wins,y=avg_fga3))
      plot_fga3 <- plot_fga3 + geom_point()
      plot_fga3 <- plot_fga3 + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_fga3))
      plot_fga3 <- plot_fga3 + xlab("Wins")
      plot_fga3 <- plot_fga3 + ylab("Average 3-point Field Goals Attempted")
      plot_fga3 <- plot_fga3 + ggtitle("Average 3-point Field Goals Attempted per Season as Related to Wins that Season")
      plot_fga3 <- plot_fga3 + theme_minimal()
      plot_fga3 <- plot_fga3 + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #free throws made correlation
      plot_ftm <- ggplot(advanced_stats,aes(x=wins,y=avg_ftm))
      plot_ftm <- plot_ftm + geom_point()
      plot_ftm <- plot_ftm + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_ftm))
      plot_ftm <- plot_ftm + xlab("Wins")
      plot_ftm <- plot_ftm + ylab("Average Free Throws Made")
      plot_ftm <- plot_ftm + ggtitle("Average Free Throws Made per Season as Related to Wins that Season")
      plot_ftm <- plot_ftm + theme_minimal()
      plot_ftm <- plot_ftm + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #free throws attempted correlation
      plot_fta <- ggplot(advanced_stats,aes(x=wins,y=avg_fta))
      plot_fta <- plot_fta + geom_point()
      plot_fta <- plot_fta + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_fta))
      plot_fta <- plot_fta + xlab("Wins")
      plot_fta <- plot_fta + ylab("Average Free Throws Attempted")
      plot_fta <- plot_fta + ggtitle("Average Free Throws Attempted per Season as Related to Wins that Season")
      plot_fta <- plot_fta + theme_minimal()
      plot_fta <- plot_fta + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #off rebounds correlation
      plot_or <- ggplot(advanced_stats,aes(x=wins,y=avg_or))
      plot_or <- plot_or + geom_point()
      plot_or <- plot_or + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_or))
      plot_or <- plot_or + xlab("Wins")
      plot_or <- plot_or + ylab("Average Offensive Rebounds")
      plot_or <- plot_or + ggtitle("Average Offensive Rebounds per Season as Related to Wins that Season")
      plot_or <- plot_or + theme_minimal()
      plot_or <- plot_or + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #defensive rebounds correlation
      plot_dr <- ggplot(advanced_stats,aes(x=wins,y=avg_dr))
      plot_dr <- plot_dr + geom_point()
      plot_dr <- plot_dr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_dr))
      plot_dr <- plot_dr + xlab("Wins")
      plot_dr <- plot_dr + ylab("Average Defensive Rebounds")
      plot_dr <- plot_dr + ggtitle("Average Defensive Rebounds per Season as Related to Wins that Season")
      plot_dr <- plot_dr + theme_minimal()
      plot_dr <- plot_dr + theme(
        plot.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #steals correlation
      steals <- ggplot(advanced_stats,aes(x=wins,y=avg_stl))
      steals <- steals + geom_point()
      steals <- steals + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                    advanced_stats$avg_stl))
      steals <- steals + xlab("Wins")
      steals <- steals + ylab("Average Steals")
      steals <- steals + ggtitle("Average Steals per Season as Related to Wins that Season")
      steals <- steals + theme_minimal()
      steals <- steals +  theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #turnovers correlation
      turnovers <- ggplot(advanced_stats,aes(x=wins,y=avg_to))
      turnovers <- turnovers + geom_point()
      turnovers <- turnovers + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                          advanced_stats$avg_to))
      turnovers <- turnovers + xlab("Wins")
      turnovers <- turnovers + ylab("Average Turnovers")
      turnovers <- turnovers + ggtitle("Average Turnovers per Season as Related to Wins that Season")
      turnovers <- turnovers + theme_minimal()
      turnovers <- turnovers +  theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #blocks
      blocks <- ggplot(advanced_stats,aes(x=wins,y=avg_blk))
      blocks <- blocks + geom_point()
      blocks <- blocks + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                          advanced_stats$avg_blk))
      blocks <- blocks + xlab("Wins")
      blocks <- blocks + ylab("Average Blocks")
      blocks <- blocks + ggtitle("Average Blocks per Season as Related to Wins that Season")
      blocks <- blocks + theme_minimal()
      blocks <- blocks +  theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )
      
      #personal fouls
      personal_fouls <- ggplot(advanced_stats,aes(x=wins,y=avg_pf))
      personal_fouls <- personal_fouls + geom_point()
      personal_fouls <- personal_fouls + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                    advanced_stats$avg_pf))
      personal_fouls <- personal_fouls + xlab("Wins")
      personal_fouls <- personal_fouls + ylab("Average Personal Fouls")
      personal_fouls <- personal_fouls + ggtitle("Average Personal Fouls per Season as Related to Wins that Season")
      personal_fouls <- personal_fouls + theme_minimal()
      personal_fouls <- personal_fouls +  theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11,face="bold"),
        axis.title.y = element_blank()
      )

      
      # chart selector if statement
      if(input$reg_season_chart_select == "Points"){points}
      else if(input$reg_season_chart_select == "Field Goals"){
        grid.arrange(plot_fgm,
                     plot_fga,
                     ncol=1,
                     top=textGrob("Average Field Goals Made (Top) and Attempted (Bottom) per Season as Related to Wins that Season",
                                  gp=gpar(fontface=2,fontsize=14))
                    )
      }
      else if(input$reg_season_chart_select == "3-point Field Goals"){
        grid.arrange(plot_fgm3,
                     plot_fga3,
                     ncol=1,
                     top=textGrob("Average 3-Point Field Goals Made (Top) and Attempted (Bottom) per Season as Related to Wins that Season",
                                  gp=gpar(fontface=2,fontsize=14))
        )
      }
      else if(input$reg_season_chart_select == "Free Throws"){
        grid.arrange(plot_ftm,
                     plot_fta,
                     ncol=1,
                     top=textGrob("Average Free Throws Made (Top) and Attempted (Bottom) per Season as Related to Wins that Season",
                                  gp=gpar(fontface=2,fontsize=14))
        )
      }
      else if(input$reg_season_chart_select == "Rebounds"){
        grid.arrange(plot_or,
                     plot_dr,
                     ncol=1,
                     top=textGrob("Average Offensive Rebounds (Top) and Defensive Rebounds (Bottom) per Season as Related to Wins that Season",
                                  gp=gpar(fontface=2,fontsize=14))
        )
      }
      else if(input$reg_season_chart_select == "Steals"){steals}
      else if(input$reg_season_chart_select == "Turnovers"){turnovers}
      else if(input$reg_season_chart_select == "Blocks"){blocks}
      else if(input$reg_season_chart_select == "Personal Fouls"){personal_fouls}
    })
    
    output$reg_season_corr_info <- renderText({
      if(input$reg_season_chart_select == "Points"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_score)
        HTML("Average Points per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_chart_select == "Field Goals"){
        corr1 <- cor(advanced_stats$wins,advanced_stats$avg_fgm)
        corr2 <- cor(advanced_stats$wins,advanced_stats$avg_fga)
        HTML("Average Field Goals Made per Season and Wins per Season have a correlation of <code>",paste(round(corr1,5)),"</code>
             <br>Average Field Goals Attempted per Season and Wins per Season have a correlation of <code>",paste(round(corr2,5)))
      }
      else if(input$reg_season_chart_select == "3-point Field Goals"){
        corr1 <- cor(advanced_stats$wins,advanced_stats$avg_fgm3)
        corr2 <- cor(advanced_stats$wins,advanced_stats$avg_fga3)
        HTML("Average 3-point Field Goals Made per Season and Wins per Season have a correlation of <code>",paste(round(corr1,5)),"</code>
             <br>Average 3-point Field Goals Attempted per Season and Wins per Season have a correlation of <code>",paste(round(corr2,5)))        
      }
      else if(input$reg_season_chart_select == "Free Throws"){
        corr1 <- cor(advanced_stats$wins,advanced_stats$avg_ftm)
        corr2 <- cor(advanced_stats$wins,advanced_stats$avg_fta)
        HTML("Average Free Throws Made per Season and Wins per Season have a correlation of <code>",paste(round(corr1,5)),"</code>
             <br>Average Free Throws Attempted per Season and Wins per Season have a correlation of <code>",paste(round(corr2,5)))        
      }
      else if(input$reg_season_chart_select == "Rebounds"){
        corr1 <- cor(advanced_stats$wins,advanced_stats$avg_or)
        corr2 <- cor(advanced_stats$wins,advanced_stats$avg_dr)
        HTML("Average Offensive Rebounds per Season and Wins per Season have a correlation of <code>",paste(round(corr1,5)),"</code>
             <br>Average Defensive Rebounds Attempted per Season and Wins per Season have a correlation of <code>",paste(round(corr2,5)))        
      }
      else if(input$reg_season_chart_select == "Steals"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_stl)
        HTML("Average Steals per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))        
      }
      else if(input$reg_season_chart_select == "Turnovers"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_to)
        HTML("Average Turnovers per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_chart_select == "Blocks"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_blk)
        HTML("Average Blocks per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_chart_select == "Personal Fouls"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_pf)
        HTML("Average Personal Fouls per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
    }) #end of vegas_info

    #regular season output
    output$regular_season_adv_plots <- renderPlot({
      
      #avg true shooting percentage over time
      ts_over_time <- regular_season_full %>%
        group_by(Season) %>%
        summarize(WTS_mean=mean(WTS_perc),LTS_mean=mean(LTS_perc)) %>%
        gather(avg,value,WTS_mean:LTS_mean) %>%
        ggplot(aes(x=Season,y=value,group=avg,color=avg)) +
        geom_point() +
        geom_line(size=1) +
        scale_color_manual(name=NULL,
                           values=c(loss_made,win_made),
                           labels=c("Loser True Shooting %","Winner True Shooting %")) +
        ggtitle("Average True Shooting % per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      
      #avg effective field goal percent over time
      efg_over_time <- regular_season_full %>%
        group_by(Season) %>%
        summarize(W_mean=mean(WEffFG_perc),L_mean=mean(LEffFG_perc)) %>%
        gather(avg,value,W_mean:L_mean) %>%
        ggplot(aes(x=Season,y=value,group=avg,color=avg)) +
        geom_point() +
        geom_line(size=1) +
        scale_color_manual(name=NULL,
                           values=c(loss_made,win_made),
                           labels=c("Loser Effective Field Goal %","Winner Effective Field Goal %")) +
        ggtitle("Average Effective Field Goal % per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      #average field goal rate over time
      ftr_over_time <- regular_season_full %>%
        group_by(Season) %>%
        summarize(W_mean=mean(WFTR),L_mean=mean(LFTR)) %>%
        gather(avg,value,W_mean:L_mean) %>%
        ggplot(aes(x=Season,y=value,group=avg,color=avg)) +
        geom_point() +
        geom_line(size=1) +
        scale_color_manual(name=NULL,
                           values=c(loss_made,win_made),
                           labels=c("Loser Free Throw Rate","Winner Free Throw Rate")) +
        ggtitle("Average Free Throw Rate per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      #average offensive rebound percentage over time
      orp_over_time <- regular_season_full %>%
        group_by(Season) %>%
        summarize(W_mean=mean(WOR_perc),L_mean=mean(LOR_perc)) %>%
        gather(avg,value,W_mean:L_mean) %>%
        ggplot(aes(x=Season,y=value,group=avg,color=avg)) +
        geom_point() +
        geom_line(size=1) +
        scale_color_manual(name=NULL,
                           values=c(loss_made,win_made),
                           labels=c("Loser Offensive Rebound %","Winner Offensive Rebound %")) +
        ggtitle("Average Offensive Rebound % per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      #WTR_perc	LTR_perc
      #average offensive rebound percentage over time
      drp_over_time <- regular_season_full %>%
        group_by(Season) %>%
        summarize(W_mean=mean(WDR_perc),L_mean=mean(LDR_perc)) %>%
        gather(avg,value,W_mean:L_mean) %>%
        ggplot(aes(x=Season,y=value,group=avg,color=avg)) +
        geom_point() +
        geom_line(size=1) +
        scale_color_manual(name=NULL,
                           values=c(loss_made,win_made),
                           labels=c("Loser Defensive Rebound %","Winner Defensive Rebound %")) +
        ggtitle("Average Defensive Rebound % per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      #average offensive rebound percentage over time
      trp_over_time <- regular_season_full %>%
        group_by(Season) %>%
        summarize(W_mean=mean(WTR_perc),L_mean=mean(LTR_perc)) %>%
        gather(avg,value,W_mean:L_mean) %>%
        ggplot(aes(x=Season,y=value,group=avg,color=avg)) +
        geom_point() +
        geom_line(size=1) +
        scale_color_manual(name=NULL,
                           values=c(loss_made,win_made),
                           labels=c("Loser Total Rebound %","Winner Total Rebound %")) +
        ggtitle("Average Total Rebound % per Season") +
        theme_minimal() +
        theme(
          plot.title = element_text(size=16,face="bold"),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14,face="bold"),
          axis.title.y = element_blank()
        )
      
      # chart selector if statement
      if(input$reg_season_adv_select == "True Shooting %"){ts_over_time}
      else if(input$reg_season_adv_select == "Effective Field Goal %"){efg_over_time}
      else if(input$reg_season_adv_select == "Free Throw Rate"){ftr_over_time}
      else if(input$reg_season_adv_select == "Offensive Rebound %"){orp_over_time}
      else if(input$reg_season_adv_select == "Defensive Rebuond %"){drp_over_time}
      else if(input$reg_season_adv_select == "Total Rebound %"){trp_over_time}
    })
    
    #regular season output
    output$regular_season_adv_corr <- renderPlot({
      
      #true shooting % correlation
      ts_corr <- ggplot(advanced_stats,aes(x=wins,y=avg_ts_perc))
      ts_corr <- ts_corr + geom_point()
      ts_corr <- ts_corr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                      advanced_stats$avg_ts_perc))
      ts_corr <- ts_corr + xlab("Wins")
      ts_corr <- ts_corr + ggtitle("Average True Shooting % per Season as Related to Wins that Season")
      ts_corr <- ts_corr + theme_minimal()
      ts_corr <- ts_corr + theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_blank()
      )
      
      #effective field goal % correlation
      efg_corr <- ggplot(advanced_stats,aes(x=wins,y=avg_eff_fg_perc))
      efg_corr <- efg_corr + geom_point()
      efg_corr <- efg_corr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                      advanced_stats$avg_eff_fg_perc))
      efg_corr <- efg_corr + xlab("Wins")
      efg_corr <- efg_corr + ggtitle("Average Effective Field Goal % per Season as Related to Wins that Season")
      efg_corr <- efg_corr + theme_minimal()
      efg_corr <- efg_corr + theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_blank()
      )
      
      #free throw rate correlation
      ftr_corr <- ggplot(advanced_stats,aes(x=wins,y=avg_ftr))
      ftr_corr <- ftr_corr + geom_point()
      ftr_corr <- ftr_corr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_ftr))
      ftr_corr <- ftr_corr + xlab("Wins")
      ftr_corr <- ftr_corr + ggtitle("Average Free Throw Rate per Season as Related to Wins that Season")
      ftr_corr <- ftr_corr + theme_minimal()
      ftr_corr <- ftr_corr + theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_blank()
      )
      
      
      #offensive rebound % corr
      orp_corr <- ggplot(advanced_stats,aes(x=wins,y=avg_or_perc))
      orp_corr <- orp_corr + geom_point()
      orp_corr <- orp_corr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_or_perc))
      orp_corr <- orp_corr + xlab("Wins")
      orp_corr <- orp_corr + ggtitle("Average Offensive Rebound % per Season as Related to Wins that Season")
      orp_corr <- orp_corr + theme_minimal()
      orp_corr <- orp_corr + theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_blank()
      )
      
      #defensive rebound % corr
      drp_corr <- ggplot(advanced_stats,aes(x=wins,y=avg_dr_perc))
      drp_corr <- drp_corr + geom_point()
      drp_corr <- drp_corr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_dr_perc))
      drp_corr <- drp_corr + xlab("Wins")
      drp_corr <- drp_corr + ggtitle("Average Defensive Rebound % per Season as Related to Wins that Season")
      drp_corr <- drp_corr + theme_minimal()
      drp_corr <- drp_corr + theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_blank()
      )
      
      #defensive rebound % corr
      trp_corr <- ggplot(advanced_stats,aes(x=wins,y=avg_dr_perc))
      trp_corr <- trp_corr + geom_point()
      trp_corr <- trp_corr + geom_smooth(method="loess",se=FALSE,color=color_corr_check(advanced_stats$wins,
                                                                                        advanced_stats$avg_tr_perc))
      trp_corr <- trp_corr + xlab("Wins")
      trp_corr <- trp_corr + ggtitle("Average Total Rebound % per Season as Related to Wins that Season")
      trp_corr <- trp_corr + theme_minimal()
      trp_corr <- trp_corr + theme(
        plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_blank()
      )
      
      
      # chart selector if statement
      if(input$reg_season_adv_select == "True Shooting %"){ts_corr}
      else if(input$reg_season_adv_select == "Effective Field Goal %"){efg_corr}
      else if(input$reg_season_adv_select == "Free Throw Rate"){ftr_corr}
      else if(input$reg_season_adv_select == "Offensive Rebound %"){orp_corr}
      else if(input$reg_season_adv_select == "Defensive Rebuond %"){drp_corr}
      else if(input$reg_season_adv_select == "Total Rebound %"){trp_corr}
    })
    
    output$reg_season_adv_corr_info <- renderText({
      if(input$reg_season_adv_select == "True Shooting %"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_ts_perc)
        HTML("Average True Shooting % per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_adv_select == "Effective Field Goal %"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_eff_fg_perc)
        HTML("Average Effective Field Goal % per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))        
      }
      else if(input$reg_season_adv_select == "Free Throw Rate"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_ftr)
        HTML("Average Free Throw Rate per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_adv_select == "Offensive Rebound %"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_or_perc)
        HTML("Average Offensive Rebound % per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_adv_select == "Defensive Rebuond %"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_dr_perc)
        HTML("Average Defensive Rebound % per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
      else if(input$reg_season_adv_select == "Total Rebound %"){
        corr <- cor(advanced_stats$wins,advanced_stats$avg_tr_perc)
        HTML("Average Total Rebound % per Season and Wins per Season have a correlation of <code>",paste(round(corr,5)))
      }
    })#end of vegas_info
    
    output$dict_descript <- renderUI({
      if(input$dict_file_select == "Teams.csv"){
        helpText("This file identifies the different college teams present in the dataset. Each school is uniquely identified by a 4 digit id number. You will not see games present for all teams in all seasons, because the games listing is only for matchups where both teams are Division-I teams. There are 353 teams currently in Division-I, and an overall total of 366 teams in our team listing (each year, some teams might start being Division-I programs, and others might stop being Division-I programs). This year there are two teams that are new to Division I: Cal Baptist (TeamID=1465) and North Alabama (TeamID=1466), and so you will not see any historical data for these teams prior to the current season.")
      } 
      else if(input$dict_file_select == "NCAATourneySeeds.csv"){
        helpText("This file identifies the seeds for all teams in each NCAA tournament, for all seasons of historical data. Thus, there are between 64-68 rows for each year, depending on whether there were any play-in games and how many there were. In recent years the structure has settled at 68 total teams, with four 'play-in' games leading to the final field of 64 teams entering Round 1 on Thursday of the first week. We will not know the seeds of the respective tournament teams, or even exactly which 68 teams it will be, until Selection Sunday on March 17, 2019.")
      }
      else if (input$dict_file_select == "RegularSeasonCompactResults.csv"){
        helpText("This file identifies the game-by-game results for many seasons of historical data, starting with the 1985 season (the first year the NCAA had a 64-team tournament). For each season, the file includes all games played from daynum 0 through 132. It is important to realize that the 'Regular Season' games are simply defined to be all games played on DayNum=132 or earlier (DayNum=132 is Selection Sunday, and there are always a few conference tournament finals actually played early in the day on Selection Sunday itself). Thus a game played on or before Selection Sunday will show up here whether it was a pre-season tournament, a non-conference game, a regular conference game, a conference tournament game, or whatever.")
      }
      else if (input$dict_file_select == "NCAATourneyCompactResults.csv"){
        helpText("This file identifies the game-by-game NCAA tournament results for all seasons of historical data. The data is formatted exactly like the RegularSeasonCompactResults data. Note that these games also include the play-in games (which always occurred on day 134/135) for those years that had play-in games. Thus each season you will see between 63 and 67 games listed, depending on how many play-in games there were. Because of the consistent structure of the NCAA tournament schedule, you can actually tell what round a game was, depending on the exact DayNum. Thus:",
          tags$ul(
            tags$li("DayNum=134 or 135 (Tue/Wed) - play-in games to get the tournament field down to the final 64 teams"),
            tags$li("DayNum=136 or 137 (Thu/Fri) - Round 1, to bring the tournament field from 64 teams to 32 teams"),
            tags$li("DayNum=138 or 139 (Sat/Sun) - Round 2, to bring the tournament field from 32 teams to 16 teams "),
            tags$li("DayNum=143 or 144 (Thu/Fri) - Round 3, otherwise known as 'Sweet Sixteen', to bring the tournament field from 16 teams to 8 teams "),
            tags$li("DayNum=145 or 146 (Sat/Sun) - Round 4, otherwise known as 'Elite Eight' or 'regional finals', to bring the tournament field from 8 teams to 4 teams "),
            tags$li("DayNum=152 (Sat) - Round 5, otherwise known as 'Final Four' or 'national semifinals', to bring the tournament field from 4 teams to 2 teams "),
            tags$li("DayNum=154 (Mon) - Round 6, otherwise known as 'national final' or 'national championship', to bring the tournament field from 2 teams to 1 champion team ")
          )
        )
      }
      else if (input$dict_file_select == "RegularSeasonDetailedResults.csv"){
        helpText("This file provides game-by-game stats at a team level (free throws attempted, defensive rebounds, turnovers, etc.) for all regular season games since the 2002-03 season. Team Box Scores are provided in 'Detailed Results' files rather than 'Compact Results' files. However, the two files are strongly related. In a Detailed Results file, the first eight columns (Season, DayNum, WTeamID, WScore, LTeamID, LScore, WLoc, and NumOT) are exactly the same as a Compact Results file. However, in a Detailed Results file, there are many additional columns. The column names should be self-explanatory to basketball fans (as above, 'W' or 'L' refers to the winning or losing team):")
      }
      else if (input$dict_file_select == "NCAATourneyDetailedResults.csv"){
        helpText("This file provides game-by-game stats at a team level (free throws attempted, defensive rebounds, turnovers, etc.) for all NCAA tournament games since the 2002-03 season. Team Box Scores are provided in 'Detailed Results' files rather than 'Compact Results' files. However, the two files are strongly related. In a Detailed Results file, the first eight columns (Season, DayNum, WTeamID, WScore, LTeamID, LScore, WLoc, and NumOT) are exactly the same as a Compact Results file. However, in a Detailed Results file, there are many additional columns. The column names should be self-explanatory to basketball fans (as above, 'W' or 'L' refers to the winning or losing team):")
      }
    })#end of data dictionary description text
    
    output$dict_table  <- renderTable({
      if(input$dict_file_select == "Teams.csv"){
        #teams
        table_data <- data_dictionary %>% filter(file == tolower(input$dict_file_select)) %>% select(-c(file))
        table_data
      } 
      else if(input$dict_file_select == "NCAATourneySeeds.csv"){
        table_data <-data_dictionary %>% filter(file == tolower(input$dict_file_select)) %>% select(-c(file))
        table_data
      }
      else if(input$dict_file_select == "RegularSeasonCompactResults.csv"){
        table_data <-data_dictionary %>% filter(file == tolower(input$dict_file_select)) %>% select(-c(file))
        table_data
      }
      else if(input$dict_file_select == "NCAATourneyCompactResults.csv"){
        table_data <-data_dictionary %>% filter(file == tolower(input$dict_file_select)) %>% select(-c(file))
        table_data
      }
      else if(input$dict_file_select == "RegularSeasonDetailedResults.csv"){
        table_data <-data_dictionary %>% filter(file == tolower(input$dict_file_select)) %>% select(-c(file))
        table_data
      }
      else if(input$dict_file_select == "NCAATourneyDetailedResults.csv"){
        table_data <-data_dictionary %>% filter(file == tolower(input$dict_file_select)) %>% select(-c(file))
        table_data
      }
    }, striped=TRUE, bordered=TRUE, hover=TRUE) #end of dict_table reactive data

    # image2 sends pre-rendered images
    output$image2 <- renderImage({
      
      width  <- session$clientData$output_image2_width
      height <- session$clientData$output_image2_height
      
      pixelratio <- session$clientData$pixelratio
      
      if (is.null(input$picture))
        return(NULL)
      
      if (input$picture == "Group 12 Predicted") {
        return(list(
          src = "images/Group 12 Bracket Final.png",
          contentType = "image/png",
          res = 72 * pixelratio,
          width = 1200 * pixelratio,
          height = 570 * pixelratio,
          alt = "Group 12"
        ))
      } else if (input$picture == "Actual") {
        return(list(
          src = "images/Actual Bracket Final.png",
          filetype = "image/jpeg",
          res = 72 * pixelratio,
          width = 1200 * pixelratio,
          height = 570 * pixelratio,
          alt = "Actual Results"
        ))
      }
      
    }, deleteFile = FALSE)
    #End of function(input, output)
  }
)