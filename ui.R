library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(grid)
library(gridExtra)


ui <- fluidPage(theme=shinytheme('flatly'),
  titlePanel("Predicting March Madness"),
  h4("Can the NCAA Men's Basketball Tournament be predicted using Machine Learning?"),
  helpText("Authors: Nathan Duvenick (nadbn4@mail.missouri.edu), Gary Gasperino (gdgp63@mail.missouri.edu), Jason Hensley (jh5xf@mail.missouri.edu), Jared Wetzel (jdwkr3@mail.missouri.edu)"),
  helpText("The dashboard provides an overview of the data exploration and analysis done by the authors in order to try
           and predict the outcomes of games in the 2019 NCAA Men's Basketball March Madness Tournament. With the 2020 tournament
           being cancelled because of COVID-19, the dashboard serves as an outline for exploring, visualizing, and analyzing
           NCAA men's basketball data. All data was provided by the NCAA as a part of a ", a("Kaggle competition",href="https://www.kaggle.com/c/mens-machine-learning-competition-2019"),
           "they host annually as a partnership with Google Cloud. Data is available on the Kaggle website."),
  
  #beginning of NavBar and TabPanels
  navbarPage(title=icon('basketball-ball'),
    
    #Predicting the tournament tabPanel
    tabPanel("Predicting the Tournament",fluid=TRUE,icon=icon("code"),
      titlePanel("Predicting March Madness"),
      helpText(a("Bracketology:",href="https://en.wikipedia.org/wiki/Bracketology"), 
               "The process of predicting the field of college basketball participants in the NCAA March Madness Tournament. This process has historically been done by experts trying to show off their knowledge of college basketball or by groups of friends competing to see who can predict the most correct match ups in any given tournament. Recently, with the growing popularity of machine learning and data science, many people have taken to predicting the tournament using computational methods. This is our attempt at building a machine learning model to predict the outcome of games in the 2019 NCAA Men's Basketball Tournament. In the below brackets if a team name is in ", 
               tags$span(style="color:#00BA38;","green"), 
               "that means it was a correct prediction where as if a team name is in ",
               tags$span(style="color:#F8766D;","red"),
               " that means it was an incorrect prediction. Our model and the below predictions were made using just aggregated regular season game statistics and advanced computed statistics. Various other data shown in this dashbaord such as the win ratio for each seed in each given tournament round and Vegas spread data was not used in this iteration of the model although we believe they could be used in creating a better, more predictive model overall."),
      helpText("Picking a perfect bracket is incredibly hard (thus why many betting sites are willing to have competitions to payout for a perfect bracket). Depending on the source the odds of a perfect bracket are somewhere between: ", 
               a("one in 128 billion and a staggering 1 in 9,223,372,036,854,775,808 (that is 9.2 quintillion)!", href= "https://math.duke.edu/news/duke-math-professor-says-odds-perfect-bracket-are-one-24-trillion")),
      fluidRow(column(width = 2, selectInput("picture", "Bracket:",
                                             choices = c("Group 12 Predicted", "Actual"),
                                             selected = "Group 12 Predicted")),
               column(width = 6,
                      imageOutput("image2"))
      )
    ),#End of prediction and modeling tab panel
    
    #Start of Historical Data TabPanel
    tabPanel("Historical Tourney Data",fluid=TRUE,icon=icon("chart-bar"),
      titlePanel("The History of March Madness"),
      helpText("College basketball's March Madness tournament first started in 1939. That tournament, however, looked nothing like today's.
                The first tournament's champion, Oregon, beat their opponent, Ohio State, by a score of 46-33. The tournament also
                included only eight teams. It wasn't until 1985 and 46 years after the first tournament that it expanded to 64 teams.
                For more historical trivia about March Madness, the NCAA offers this ",a("ultimate guide.",href="https://www.ncaa.com/news/basketball-men/article/2019-04-30/march-madness-history-ultimate-guide")),
      helpText("When filling out a March Madness bracket, people will often go with the teams they know over the teams they don't. Several teams
                stand out as consistent performers over the years and are nearly sure bets whenever filling out a March Madness bracket. They may vary year to year
                in how dominant they are, but you can almost always rely on teams like North Carolina, Duke, and Kansas to do well. When looking to predict
                the outcomes of games though, it's not simply enough to assume teams who have done well in the past will do well again. We'll need to build a much
                more robust model than that, taking into account as many factors as possible to identify why teams like North Carolina, Kansas, and Duke do well
                and also why they sometimes don't."),
      helpText("Below are the first of many visualizations used for data exploration. The below visualizations summarize at a high level how teams have performed
               in the tournament in recent years by looking at their total number of wins, total number of losses, and win/loss ratio. A team with a below
               one ratio has lost more games in the tournament than they have won, for example. Additionally, a set of static charts which show the top 25 teams
               in each category is shown for context."),
      h4("Data was limited to the 2003 to 2019 NCAA Men's Baskteball Tournaments so charts represent wins, losses, and win/loss ratio in tournaments during that time frame."),
      fluidRow(uiOutput("conf_select"),width=12),
      fluidRow(column(plotOutput("tourney_wins"),width=4),
               column(plotOutput("tourney_losses"),width=4),
               column(plotOutput("tourney_ratio"),width=4)
               ),
      h4("The below three charts show the top 25 teams for wins, losses, and win/loss ratio regardless of conference. They are unaffected by the filter."),
      fluidRow(column(plotOutput("tourney_25_wins"),width=4),
               column(plotOutput("tourney_25_losses"),width=4),
               column(plotOutput("tourney_25_ratio"),width=4)
               )
    ),#End of Tourney Data Tabpanel
    
    tabPanel("Regular Season Statistics",fluid=TRUE,icon=icon("chart-line"),
      titlePanel("Regular Season Statistics Over Time and Their Correlation with Winning"),
      helpText("The game we know as basketball today plays very differently than it did a few decades ago. For example, ",
                a("the three-point shot wasn't even adopted by the NBA and NCAA until 1977 and 1980, respectively. ",href="https://hooptactics.net/premium/basketballbasics/bb8rulesevolution.php"),
               "Things like the shot clock and the three point arc ",
               a("have even been changed in the last few years. ",href="http://www.orangehoops.org/NCAA/NCAA%20Rule%20Changes.htm"),
               "Along with rule changes, new strategies are developed and teams have to adapt.
               The most successful and consistent teams adapt quickly while others are left behind. The best way to encapsulate this is through the various
               statistics captured during each game, like points scored, field goals made & attempted, and three pointers to name a few. The charts below look at both
               how those statistics have changed over the years by looking at the seasonal average for all winning and losing teams as well as how these
               statistics correlate with the number of seasonal wins. Looking at the statistics over time tells us how much the game has changed and which, if any,
               statistics are irrelevant to the modern game. The correlation will tell us what type of relationship that statistic may have with winning. A ",tags$span(style="color:#619CFF;","blue"),
               " line means there is a positive correlation while a ", tags$span(style="color:#F8766D;","red")," line means there is a negative one."),
      fluidRow(
        column(width=6,
               selectInput("reg_season_chart_select",
                           "Select a Statistic:",
                           choices = c("Points","Field Goals","3-point Field Goals",
                                              "Free Throws","Rebounds","Steals","Turnovers",
                                              "Blocks","Personal Fouls"),
                           selected = "Points")),
        column(width=6,
               htmlOutput("reg_season_corr_info"))
      ),
      fluidRow(
        column(width=6,plotOutput("regular_season")),
        column(width=6,plotOutput("regular_season_corr"))
      )
    ),#End of Regular Season Data Tabpanel
    
    tabPanel("Advanced Statistics",fluid=TRUE,icon=icon("chart-line"),
      titlePanel("Looking at Advanced Statistics"),
      helpText("For the purposes of this dashboard and our analysis, we have defined advanced statistics as any of those stats
               which need to be computed from regularly captured NCAA game data (things like points, field goals, 3-pointers etc). 
               This tab and the visualizations below look at the relationship between those calculated statistics and wins as well as 
               how those statistics have changed over time (in a very similar way as we did with the typical stats captured for a basketball game).
               Below are the advanced stats we computed and how we computed them per ",a("basketball reference.",href="https://www.basketball-reference.com/about/glossary.html")),
      helpText(
        tags$ul(
          tags$li("True Shooting % = 100 * Team Points / (2 * (Field Goals Attempted + (0.475 * Free Throws Attempted)))"),
          tags$li("Effective FG % = (Field Goals Made + 0.5 * Threes Made) / Field Goals Attempted"),
          tags$li("Free Throw Rate = Free Throws Made / Field Goals Attempted"),
          tags$li("Offensive Rebound % = Offensive Rebounds / (Offensive Rebounds + Opponent's Defensive Rebounds)"),
          tags$li("Defensive Rebound % = Defensive Rebounds / (Defensive Rebounds + Opponent's Offensive Rebounds)"),
          tags$li("Total Rebound % = (Defensive Rebounds + Offensive Rebounds) / (Defensive Rebounds + Offensive Rebounds + Opponent's Defensive Rebounds + Opponent's Offensive Rebounds)")
        )
      ),
      helpText("Among these advanced stats that we computed are what are commonly referred to as the ", a("Four Factors.",href="https://www.basketball-reference.com/about/factors.html"),
               " Identified by Dean Oliver, these four factors are the majority of the reason that teams win games. Those four factors being shooting,
               turnovers, rebounding, and free throws. Of these four factors, true shooting percentage and effective field goal percentage are associated with
               shooting, offensive and defensive rebound percentage are associated with rebounding, and free throw rate is associated with free throws.
               Looking at the correlation for these stats, they are all fairly strongly positively correlated with winning, which is why they're so important
               to include in analysis. For further reading on Four Factors analysis, check out this ",a("link.",href="https://squared2020.com/2017/09/05/introduction-to-olivers-four-factors/")),
      fluidRow(column(width=6,
                      selectInput("reg_season_adv_select",
                                  "Select an Advanced Statistics:",
                                  choices = c("True Shooting %","Effective Field Goal %","Free Throw Rate",
                                              "Offensive Rebound %","Defensive Rebuond %","Total Rebound %"),
                                  selected = "True Shooting %"
                                  )),
               column(width=6,
                      htmlOutput("reg_season_adv_corr_info"))
      ),
      fluidRow(
        column(width=6,plotOutput("regular_season_adv_plots")),
        column(width=6,plotOutput("regular_season_adv_corr"))
      )
    ),#End of Advanced Statistics Tabpanel
    
    #Start of Seed Data TabPanel
    tabPanel("Looking at Tournament Seeds",fluid=TRUE,icon=icon("trophy"),
      titlePanel("How does tournament seed affect a team's chances of winning in each round?"),
      helpText("This was one of the initial questions we had whenever we set out to explore our data. The seed a team receives
               isn't based strictly off their win-loss ratio from the regular season because there is a selection process for the tournament. The NCAA notes on their", 
               a("'selections 101' page",href="http://www.ncaa.org/about/resources/media-center/mens-basketball-selections-101-bracket"),
               " that ",em("'This is accomplished by adding up the true seed number for the 16 teams that comprise the top four teams in each of the four regions.
               The committee will then go through the seed list, placing all teams by seed starting with the four No. 1 seeds, through the No. 4 seeds,
               and place those geographically by region while using the",
               a("principles and procedures",href="https://www.ncaa.com/news/basketball-men/article/2018-10-19/how-field-68-teams-picked-march-madness"),
               "for bracketing.'"), " With this human input, we thought there might be some relationship between a team's
               tournament seed and their overall performance."
               ),
      helpText("Additionally, since the teams are split up into four different regions for tournament play, that means that there are four number one seeds, four
               number two seeds, etc. We wanted to look and see how each seed performed throughout the tournament and see if there was any sort of
               bias being introduced. Could it be that the number one seeds don't do nearly as well as the number two seeds? What about if the number 16 seeds perform better on average
               than the number 15 seeds? The charts below are exploration done to look at those questions."),
     fluidRow(
       column(width=2,wellPanel(uiOutput("seed_select"),checkboxInput('seed_select_all', 'Select All / Deselect All'))),
       column(width=10,fluidRow(plotOutput("seed_plot",height=550)))
     ),
     fluidRow(column(plotOutput("tourney_seed_wins"),width=6),
              column(plotOutput("tourney_seed_losses"),width=6)
              )
    ),#End of Seed Data Tabpanel
    
    tabPanel("Looking at Vegas Betting Lines",fluid=TRUE,icon=icon("usd"),
      h1("Winning Percentage Based on Betting Line"),
      helpText("One of the most useful sources of information on sports and how a general population expects any game to go is Vegas betting data.
                Most sports have what is called a point spread", a("which can be used to determine who is the favorite in the match.",href="https://www.thespread.com/betting-point-spreads"),
               "This point spread is equal to the expected point differential between the two teams. For instance, if a team has a -7 points spread, that means
               they are the favorite by 7 points (i.e. the opposing team is expected to be 7 points behind them). Point spreads are important and useful to us
               as they get updated frequently (and in some cases right up until the game starts depending on the betting rules) which means they take into account
               things like injuries or sudden changes in weather. These are things that are difficult to account for with typically available sports data. The below chart looks at historical Vegas spreads and calculates a probability of how often a team that was a favorite by that many points won.
               By clicking on the chart, you can get information are particular point spreads and their probability of winning in the past. For example, clicking on the 15-point favorite
               you'll see that historically 15-point favorites have won 96% of the time."),
      fluidRow(
        plotOutput("vegas_plot", 
          click = "plot_click"),
          htmlOutput("vegas_info")
        )
    ),#End of Vegas Betting Tabpanel
    navbarMenu("More",
        tabPanel("Data Dictionary",
                 fluidRow(
                   column(width=3,
                     selectInput("dict_file_select",
                                 "Select a File to View Columns From:",
                                 choices = c("Teams.csv","NCAATourneySeeds.csv",
                                             "RegularSeasonCompactResults.csv","NCAATourneyCompactResults.csv",
                                             "RegularSeasonDetailedResults.csv","NCAATourneyDetailedResults.csv"),
                                 selected = "Teams.csv"
                     ),
                     htmlOutput("dict_descript")
                   ),
                   column(width=9,
                     tableOutput("dict_table")
                   )
                 )
          ),
        tabPanel("References",
                 tags$ul(
                   tags$li("Data Source:",
                           tags$ul(
                             tags$li(a("https://www.kaggle.com/c/mens-machine-learning-competition-2019",href="https://www.kaggle.com/c/mens-machine-learning-competition-2019"))
                           )),
                   tags$li("Bracketology and Predicting a Perfect Bracket",
                           tags$ul(
                             tags$li(a("https://en.wikipedia.org/wiki/Bracketology",href="https://en.wikipedia.org/wiki/Bracketology")),
                             tags$li(a("https://math.duke.edu/news/duke-math-professor-says-odds-perfect-bracket-are-one-24-trillion",href="https://math.duke.edu/news/duke-math-professor-says-odds-perfect-bracket-are-one-24-trillion"))
                           )
                   ),
                   tags$li("NCAA & March Madness History",
                           tags$ul(
                             tags$li(a("https://www.ncaa.com/news/basketball-men/article/2019-04-30/march-madness-history-ultimate-guide",href="https://www.ncaa.com/news/basketball-men/article/2019-04-30/march-madness-history-ultimate-guide")),
                             tags$li(a("https://hooptactics.net/premium/basketballbasics/bb8rulesevolution.php",href="https://hooptactics.net/premium/basketballbasics/bb8rulesevolution.php")),
                             tags$li(a("http://www.orangehoops.org/NCAA/NCAA%20Rule%20Changes.htm",href="http://www.orangehoops.org/NCAA/NCAA%20Rule%20Changes.htm")),
                             tags$li(a("http://www.ncaa.org/about/resources/media-center/mens-basketball-selections-101-bracket",href="http://www.ncaa.org/about/resources/media-center/mens-basketball-selections-101-bracket")),
                             tags$li(a("https://www.ncaa.com/news/basketball-men/article/2018-10-19/how-field-68-teams-picked-march-madness",href="https://www.ncaa.com/news/basketball-men/article/2018-10-19/how-field-68-teams-picked-march-madness"))
                           )),
                   tags$li("Four Factors & Advanced Statistics",
                           tags$ul(
                             tags$li(a("https://www.basketball-reference.com/about/glossary.html",href="https://www.basketball-reference.com/about/glossary.html")),
                             tags$li(a("https://www.basketball-reference.com/about/factors.html",href="https://www.basketball-reference.com/about/factors.html")),
                             tags$li(a("https://squared2020.com/2017/09/05/introduction-to-olivers-four-factors/",href="https://squared2020.com/2017/09/05/introduction-to-olivers-four-factors/"))
                           )),
                   tags$li("Vegas Betting",
                           tags$ul(
                             tags$li(a("https://www.thespread.com/betting-point-spreads",href="https://www.thespread.com/betting-point-spreads"))
                           ))
                 )
        )
    )#End of NavBar Menu
  )#End of Navbar
)#End of UI