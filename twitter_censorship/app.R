
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(infer)
library(broom)
library(readr)
library(ggthemes)
library(viridis)
library(gt)
library(plotly)
library(ggplot2)


# Loading in the data

twitter_gdp <- read_csv("twitter_gdp.csv")
mydata1 <- read_csv("mydata1.csv") %>% 
  
# Defining the levels
  
  mutate(year = factor(year, levels =
                                c("January - June 2012",
                                  "July - December 2012",
                                  "January - June 2013",
                                  "July - December 2013",
                                  "January - June 2014",
                                  "July - December 2014",
                                  "January - June 2015",
                                  "July - December 2015",
                                  "January - June 2016",
                                  "July - December 2016",
                                  "January - June 2017",
                                  "July - December 2017",
                                  "January - June 2018",
                                  "July - December 2018",
                                  "January - June 2019")))

# Creating the year values list

tg_year <- tibble(
    "2012" = "2012",
    "2013" = "2013", 
    "2014" = "2014", 
    "2015" = "2015", 
    "2016" = "2016", 
    "2017" = "2017", 
    "2018" = "2018")

# Creating the countries value list 

tg_countries <- tibble(
    'Afghanistan' = 'Afghanistan' ,
    'Argentina' = 'Argentina' ,
    'Australia' = 'Australia' ,
    'Austria' = 'Austria' ,
    'Azerbaijan' = 'Azerbaijan' ,
    'Bahrain' = 'Bahrain' ,
    'Belarus' = 'Belarus' ,
    'Belgium' = 'Belgium' ,
    'Bosnia and Herzegovina' = 'Bosnia and Herzegovina' ,
    'Brazil' = 'Brazil' ,
    'Bulgaria' = 'Bulgaria' ,
    'Canada' = 'Canada' ,
    'Chile' = 'Chile' ,
    'Colombia' = 'Colombia' ,
    'Croatia' = 'Croatia' ,
    'Cyprus' = 'Cyprus' ,
    'Czech Republic' = 'Czech Republic' ,
    'Denmark' = 'Denmark' ,
    'Dominican Republic' = 'Dominican Republic' ,
    'Ecuador' = 'Ecuador' ,
    'Egypt' = 'Egypt' ,
    'Estonia' = 'Estonia' ,
    'Finland' = 'Finland' ,
    'France' = 'France' ,
    'Georgia' = 'Georgia' ,
    'Germany' = 'Germany' ,
    'Greece' = 'Greece' ,
    'Haiti' = 'Haiti' ,
    'Hong Kong' = 'Hong Kong' ,
    'India' = 'India' ,
    'Indonesia' = 'Indonesia' ,
    'Iraq' = 'Iraq' ,
    'Ireland' = 'Ireland' ,
    'Israel' = 'Israel' ,
    'Italy' = 'Italy' ,
    'Japan' = 'Japan' ,
    'Kazakhstan' = 'Kazakhstan' ,
    'Kenya' = 'Kenya' ,
    'Kuwait' = 'Kuwait' ,
    'Kyrgyzstan' = 'Kyrgyzstan' ,
    'Lebanon' = 'Lebanon' ,
    'Luxembourg' = 'Luxembourg' ,
    'Macedonia' = 'Macedonia' ,
    'Malaysia' = 'Malaysia' ,
    'Maldives' = 'Maldives' ,
    'Mexico' = 'Mexico' ,
    'Mongolia' = 'Mongolia' ,
    'Nepal' = 'Nepal' ,
    'Netherlands' = 'Netherlands' ,
    'New Zealand' = 'New Zealand' ,
    'Norway' = 'Norway' ,
    'Oman' = 'Oman' ,
    'Pakistan' = 'Pakistan' ,
    'Panama' = 'Panama' ,
    'Paraguay' = 'Paraguay' ,
    'Philippines' = 'Philippines' ,
    'Poland' = 'Poland' ,
    'Portugal' = 'Portugal' ,
    'Qatar' = 'Qatar' ,
    'Russian Federation' = 'Russian Federation' ,
    'Saudi Arabia' = 'Saudi Arabia' ,
    'Serbia' = 'Serbia' ,
    'Singapore' = 'Singapore' ,
    'Slovenia' = 'Slovenia' ,
    'South Africa' = 'South Africa' ,
    'South Korea' = 'South Korea' ,
    'Spain' = 'Spain' ,
    'Sri Lanka' = 'Sri Lanka' ,
    'Sweden' = 'Sweden' ,
    'Switzerland' = 'Switzerland' ,
    'Thailand' = 'Thailand' ,
    'Tunisia' = 'Tunisia' ,
    'Turkey' = 'Turkey' ,
    'Ukraine' = 'Ukraine' ,
    'United Arab Emirates' = 'United Arab Emirates' ,
    'United Kingdom' = 'United Kingdom' ,
    'United States' = 'United States' ,
    'Uruguay' = 'Uruguay' ,
    'Uzbekistan' = 'Uzbekistan' ,
    'Venezuela' = 'Venezuela')
  
# Creating the Twitter Data Values list  

variables <- c("Country GDP (from the World Bank)" = "gdp", 
               "Freedom Score (from Freedom House)" = "score",
               "Withheld Content (Tweets and Accounts)" = "content_withheld", 
               "No Action" = "accounts_no_action", 
               "Accounts - Terms of Service Violations" = "accounts_tos", 
               "Reports Based on Local Laws" = "local_laws",
               "Time Period" = "time_period",
               "Country" = "country", 
               "Percentage Where Some Content Withheld" = "percentage_where_some_content_withheld", 
               "Total Requests Made (by various institutions, agencies)" = "total_requests_made", 
               "Accounts Specified" = "accounts_specified")

variablesvisual <- tibble("Accounts - Terms of Service Violations" = "accounts_tos",
                     "Total Requests Made (by various institutions, agencies)" = "total_requests_made",
                     "Accounts Specified" = "accounts_specified",
                     "Withheld Content (Tweets and Accounts)" = "total_withholding_accounts_and_tweets",
                     "Tweets Withheld" = "tweets_withheld",
                     "Accounts Withheld" = "accounts_withheld", 
                     "Requests where some content was withheld" = "requests_where_content_withheld"
               )


# Defining variables for the Variable explorer. 

score1 <- c("The Freedom Score is Freedom House's measure of the degree of freedom 
a country allows. A country or territory is awarded 0 to 4 points for each of 10
political rights indicators and 15 civil liberties indicators, which take
the form of questions; a score of 0 represents the smallest degree of freedom
and 4 the greatest degree of freedom. The political rights questions are grouped
into three subcategories: Electoral Process (3 questions), Political Pluralism 
and Participation (4), and Functioning of Government (3). The civil liberties
questions are grouped into four subcategories: Freedom of Expression and Belief 
(4 questions), Associational and Organizational Rights (3), Rule of Law (4), and
Personal Autonomy and Individual Rights (4). The political rights section also 
contains an additional discretionary question addressing forced demographic change.
For the discretionary question, a score of 1 to 4 may be subtracted, as applicable 
(the worse the situation, the more points may be subtracted). The highest overall 
score that can be awarded for political rights is 40 (or a score of 4 for each of the
10 questions). The highest overall score that can be awarded for civil liberties is 60 
(or a score of 4 for each of the 15 questions). The scores from the previous edition are
used as a benchmark for the current year under review. A score is typically changed only
if there has been a real-world development during the year that warrants a decline or
improvement (e.g., a crackdown on the media, the country’s first free and fair elections),
though gradual changes in conditions—in the absence of a signal event—are occasionally 
registered in the scores. (Source: Freedom House)")

gdp1 <- "Each country's GDP in 2010 United States Dollars."

content_withheld1 <- "This data includes all legal demands where Twitter employed its 
Country Withheld Content (CWC) tool. Since Twitter's first transparency
report in 2012, it has used CWC in 18 countries in response to legal demands: Argentina, 
Australia, Belgium, Brazil, Canada, France, Germany, India, Ireland, Israel, Japan, 
Netherlands, New Zealand, Russia, South Korea, Spain, Turkey, and the United Kingdom.
During this reporting period, Twitter withheld content at the account or Tweet level in nine
of those 18 countries, including Canada for the first time. (Source: Twitter)"

accounts_no_action1 <- "This section includes instances where, in response to a legal demand,
no accounts or Tweets were withheld nor was any content removed due to TOS violations.
Twitter states that it does not take action on newsworthy content or political speech protected
under UN-recognized principles of free expression consistent with Twitter values.(Source: Twitter)"

accounts_tos1 <- "This section includes instances where, in response to legal demands
identifying the specified accounts or Tweets, Twitter completely removed the content 
from Twitter after determining it violated Twitter’s TOS. It states that it reviews alls reported 
content for violations of Twitter’s TOS before assessing it further. (Source: Twitter)"

local_laws1 <- "This section includes removal requests from Twitter's trusted reporters and 
non-governmental organizations identified by the European Commission (EC) that alleged violations of local law(s). 
All reported content is first reviewed for potential violations of Twitter's TOS
and any content that is found to be violating is removed from the platform. Content
that does not violate Twitter's TOS is then reviewed for potential withholding based 
on the local law(s) of the reporting jurisdiction. (Source: Twitter)"

time_period1 <- "The data is grouped by time periods of January to June, and July to
December for each year, from January 2012 to June 2019."

country1 <- "The country that registered content removal requests. Since Twitter's first transparency
report in 2012, it has used CWC in 18 countries in response to legal demands: Argentina, 
Australia, Belgium, Brazil, Canada, France, Germany, India, Ireland, Israel, Japan, 
Netherlands, New Zealand, Russia, South Korea, Spain, Turkey, and the United Kingdom. Requests
from other countries have not yet been complied with. (Source: Twitter)"

percentage_where_some_content_withheld1 <- "This includes instances where Tweets and/or accounts were 
withheld in response to requests Twitter reviewed under local law(s). It does not include situations 
here a person removed the content at issue themselves or where the content was removed from 
violating Twitter’s TOS. (Source: Twitter)"

total_requests_made1 <- "This  includes the total number of legal requests from government agencies, 
police departments, or other authorized requesters within a country."

accounts_specified1 <- "This includes the unique number of accounts identified in 
the requests Twitter received. (Source: Twitter)"

# Defining countries where some content was withheld.

big18countries <- c("Argentina",
           "Australia",
           "Belgium",
           "Brazil",
           "Canada",
           "France",
           "Germany",
           "India",
           "Ireland",
           "Israel",
           "Japan",
           "Netherlands",
           "New Zealand",
           "Russia",
           "South Korea",
           "Spain",
           "Turkey",
           "United Kingdom")

regionsfreedom <- c("Asia",			
"Americas",				
"Europe",				
"Eurasia",				
"MENA",				
"SSA")


# UI setup

ui <- fluidPage(
    
    navbarPage(
        "Government Censorship on Twitter: Requests and Response (2012-2019)",
        theme = shinytheme("slate"),
        tabPanel("About the Project",
                 
                 h3("Motivation"), 
                 
                 p("On the 5th of August 2019, the Indian Government imposed a 
                 communications lockdown on the disputed region of Kashmir. This 
                 lockdown has been going on for over 8 months now, with very limited
                 information about the on-ground events being made available to the
                 outside world. Twitter was particularly instrumental for the government
                 in India, which, according to the Berkman Klein Center for Internet 
                 and Society at Harvard, received thousands of requests from the Indian
                 government to withdraw tweets and accounts from Kashmiri journalists, 
                 activists and news organizations operating across the world."), 
                 
                 p("Several articles regarding the censorship of Kashmiri activists and
                   journalists on Twitter alleged that Twitter’s actions were the consequence
                   of its desire to appease the rising global power, especially given its 
                   economic might and massive markets. Others alleged that Indian Government’s
                   requests were only an extension of its authoritarian overreach outside of 
                   the virtual world."),
                 
                 p("Given this threat to free speech, I seek to use this project to investigate 
                 the requests governments across the world make for the censorship of data on 
                 Twitter, and to explore trends in this activity over the years. I will particularly
                 look at if a country’s GDP has an effect on the percentage of requests Twitter ends
                 up upholding, as well as the relationship between a country’s level of freedom and
                 the number of requests it makes for content to be withheld on Twitter."),
                 
                 h3("About Me:"), 
                 
                 p("My name is Asmer Asrar Safi, and I'm a rising Sophomore in Leverett House 
                 planning to pursue Social Studies as a concentration. I was raised in Pakistan,
                 which fostered in me a great interest in South Asian Politics and various human
                 rights issues in the region."), 
                 
                 p("You can reach me at asmersafi@college.harvard.edu, or my",
                   tags$a(href = "https://www.linkedin.com/in/asmer-asrar-safi-1b06b3185/", "LinkedIn.")),
                 
                 h3("Acknowledgments"),
                 
                 p("I would like to thank Preceptor Kane, my TF Kaneesha, and the Gov 1005 community for 
                   helping me develop a passion for something I had never previously thought of pursuing.
                   I would specifically like to thank Hamid Khan, a fellow Gov 1005 student and mentor for 
                   helping me get back up every time a problem set or an R Studio error discouraged me from
                   pursuing data science further.")),
        
        tabPanel("Data",
                 sidebarLayout(
                   
                   sidebarPanel( 
                 
                 h3("Sources"), 
                 
                 p("The primary data for this final project is from the", 
                   tags$a(href = "https://transparency.twitter.com/", "Twitter Transparency Reports."),"
                   The secondary data for this Project was obtained from the Berkman Klein
                   Center for Internet and Society's", tags$a(href = "https://cyber.harvard.edu/research/lumen", "Lumen Project"),
                   "at Harvard University. I also obtained data on each country’s GDP from the",
                   tags$a(href = "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD", "World Bank"), "over the course of 2012 
                   to 2018, since data on 2019 is not yet available. I also obtained Data from", 
                   tags$a(href = "https://freedomhouse.org/report/freedom-world/2020/leaderless-struggle-democracy", "Freedom House"),
                   "for the Global Freedom Scores for each country to 
                   visualize trends between a country’s overall level of freedom does correlate with 
                   the number of requests it makes for content withdrawal. This data was only available
                   from 2013 to 2019."),
                 
                 p("The Twitter Transparency Report data is further subdivided into content removal requests and 
                 information requests made by governments from January 2012 to June 2019. However, for the purposes of this project,
                 since I seek to investigate censorship on twitter only, I am only using the removal requests data. 
                 The latest reports (July 2019 - December 2019) are yet to be released."), 
                 
                 p("The data is grouped by time periods of January to June, and July to December for each year, with 
                 requests made by courts, government institutions, law-enforcement agencies being documented in each data set.
                 Each data set also documents the requests complied with (in terms of both accounts and tweets), with URLs to 
                 the withheld data."), 
                 
                 p("All the data analysis is conducted using information from the aforementioned data sources."),
                 
                 p("Access my GitHub Repository", tags$a(href = "https://github.com/asmersafi/Censorship-On-Social-Media", "here."))), 
                 
                 # Creating Variable Explorer for more details about the dataset variables: 
                 
                 mainPanel(h3("Variable Explorer"),
                           h4("Learn more about the dataset variables:"),
                           tabsetPanel(
                             tabPanel("Withheld Content",
                                      h5("From Twitter:"),
                                      paste(content_withheld1)),
                             tabPanel("Freedom Score",
                                      h5("From Freedom House:"),
                                      paste(score1)),
                             tabPanel("Country GDP",
                                      h5("From the World Bank:"),
                                      paste(gdp1)),
                             tabPanel("No Action",
                                      h5("From Twitter:"),
                                      paste(accounts_no_action1)),
                             tabPanel("TOS Violations",
                                      h5("From Twitter:"),
                                      paste(accounts_tos1)),
                             tabPanel("Time Period",
                                      h5("From Twitter:"),
                                      paste(time_period1)),
                             tabPanel("Content Withheld Percentage",
                                      h5("From Twitter:"),
                                      paste(percentage_where_some_content_withheld1)),
                             tabPanel("Total Requests Made",
                                      h5("From Twitter:"),
                                      paste(total_requests_made1)), 
                             tabPanel("Country",
                                      h5("From Twitter:"),
                                      paste(country1)), 
                             tabPanel("Accounts Specified",
                                      h5("From Twitter:"),
                                      paste(accounts_specified1)),
                             tabPanel("Reports based on Local Laws",
                                      h5("From Twitter:"),
                                      paste(local_laws1))
                           )
                        )
    )),
    
    tabPanel("Data Visualizations",
             tabsetPanel(
               tabPanel("Overall Requests and Response data:",
                        sidebarLayout(
                          sidebarPanel(
                            br(),
                            
                            # Plotting overall variables over the time-period 2012-2019
                            
                            pickerInput(inputId = "overallvars",
                                        label = "Choose Variable:",
                                        choices = variablesvisual,
                                        multiple = FALSE
                                        
                          ),
                          helpText("The plot tracks overall changes in the selected variable made over 
                                   the time period January 2012 to June 2019."),
                          br(),
                          
                          helpText("The first three variabeles, namely 'Accounts - TOS Violations', 
                          Accounts Specified', and 'Total Requests Made (by various institutions, agencies)'
                          present data for all of the countries mentioned in the drop down menu. Each of these
                          countries have, at some point over the period of seven years, lodged a request to 
                          Twitter for content withholding. However, the latter four variables concerning 
                          actual content withheld concerns only the 18 countries Twitter specified as ones that
                          were successful in having content removed."),
                          
                          br(),
                          
                          helpText("For more details, visit the 'Withheld Content' tab on the Variable Explorer (Data page), or
                                   'Track Requests/Response by Country' tab on the Visualizations page.")),
                          
                          mainPanel(
                            h4("Plot Display:"),
                            br(),
                            
                            # Plotting overall variables over the time-period 2012-2019
                            
                            plotOutput("visualoverall")
                          )
                        )),
               
               # Plotting the variables by filtering for the country
               
               tabPanel("Track Requests/Response by Country:",
                        sidebarLayout(
                          sidebarPanel(
                            br(),
                            
                            # Picker to choose country
                            
                            pickerInput(inputId = "countryid",
                                        label = "Choose Country:",
                                        choices = tg_countries,
                                        multiple = FALSE,
                                        select = "Turkey"
                                        
                            ),
                            br(),
                            
                            # Picker to choose variable 
                            
                            pickerInput(inputId = "overallvars1",
                                        label = "Choose Variable:",
                                        choices = variablesvisual,
                                        multiple = FALSE,
                                        select = "accounts_specified"
                          ),
                          
                          helpText("Note: The only countries where some content was withheld by Twitter were Argentina, 
                                   Australia, Belgium, Brazil, Canada, France, Germany, India, Ireland, Israel, Japan, 
                                   Netherlands, New Zealand, Russia, South Korea, Spain, Turkey, and the United Kingdom. Requests
                                   from other countries have not yet been complied with. 
                                   "),
                        br(),
                          helpText("Hence, the variables 'Tweets Withheld', 'Accounts Withheld', 'Requests where some content was
                          withheld', and 'Withheld Content (sum of accounts and tweets withheld)' will only display data visualizations for the 
                          aforementioned 18 countries. For the rest of the countries, we can only visualize accounts that violated TOS, accounts that were 
                          specified for withholding (but eventually were not withheld), and the total number of requests made by the country and its
                          various institutions."),
                        br(),
                          helpText("The countries available in the drop down menu are countries that have made at least one request to
                                   Twitter for content withholding. It is important to note that list does not feature all UN recognized states,
                                   since many have not yet approached Twitter for content withholding.")),
                        
                          mainPanel(
                            h4("Plot Display:"),
                            br(),
                            plotOutput("countryvisuals")
                          )
                        )),
               
               # Mapping Freedom Scores of individual countries over time, in comparison with
               # the mean freedom scores during that time period. 
               
               tabPanel("Freedom Scores over time:",
                        sidebarLayout(
                          sidebarPanel(
                            br(),
                            
                            # Picker Input to choose country
                            
                            pickerInput(inputId = "countryfreedom",
                                        label = "Choose Country:",
                                        choices = tg_countries,
                                        multiple = FALSE,
                                        select = "Turkey"),
                            
                            br(),
                            helpText("This section seeks to visualize changes in the freedom scores
                                     of countries over time. While there are no substantial decreases or
                                     increases in the freedom scores of countries over the seven year period
                                     we seek to investigate, it is interesting to nevertheless visualize these
                                     variations with respect to the number of Twitter content removal requests
                                     made in the respective years, and comparing whether or not the two correlate.
                                     We will put this relationship to test in the Models page."),
                  
                            
                            strong("A high freedom score indicates that the country is more free than one with
                                   a lower score."),
                            
                            br(),
                            
                            helpText("This plot allows comparison between the average freedom scores and total content withholding
                                     requests made by all countries in the specific year with the corresponding freedom score and content
                                     withholding requests of the selected country.The plot starts from the year the particular state began
                                     making Twitter content removal requests."),
                           
                            br(),
                            helpText("Note: Freedom Score data was only available for the years 2013 to 2019. 
                                     To read more about the Freedom Score, visit the Variable Explorer on the Data page.")
                           
                          
                          ),
                          mainPanel(
                            h4("Plot Display:"), 
                            br(),
                            
                            # Display plot for freedom scores over time. 
                            
                            plotOutput("freedom_time"))
                        )
             )
             )),
    
    
    tabPanel("Models",
             tabsetPanel(
               tabPanel("GDP and Percentage of Total Requests Complied with",
                        tabsetPanel(
                  
                          tabPanel("All Countries:",
                                   sidebarLayout(
                                     sidebarPanel(width = 5,
                                                  
                                       br(), 
                                       
                                       h4("Regression Results:"),
                                       
                                       helpText("In this section, we investigate the effect of a country's 
                                         GDP (indicative of a country's wealth, economic power) on whether or 
                                         not Twitter decides to comply with its content removal requests."),
                                       
                                       helpText("You can choose to visualize the relative number of total requests made
                                         whilst simultaneously looking at the relationship between the percentage of 
                                         requests complied with, against the GDP (in 2010 US dollars."),
                                       
                                       br(),
                                       
                                       # GT Table printed for first regression (linear model)
                                       
                                       tableOutput("regres1"), 
                                        
                                       br(),
                                       
                                       helpText("From our results, it can be interpreted that a $1 increase in the GDP of a country leads
                                         to a 6.232805e-13% increase in the percentage of requests that Twitter will comply
                                         with, upon receiving from a state This indicates a weak positive relationship 
                                         between the two variables."),
                                       
                                       
                                       # Checkbox input to display the size of
                                       # the total requests made. 
                                       
                                       checkboxInput(
                                         inputId = "total_requests_or_no",
                                         label = "Display total requests for content removal
                                         at each point (by size).",
                                         value = TRUE),
                                       
                                       br(), 
                                       
                                       h4("Twitter Compliance Predictor:"),
                                      
                                        # Slider input for predictor. Defining minimum and maximum values corresponding
                                        # with the GDP values in the dataset. 
                                       
                                       helpText("Use the slider above to select a GDP amount (in 2010 US Dollars). Based off
                                                this linear model, our predictor will return the percentage of requests for content
                                                removal Twitter will likely comply with. The minimum and maximum GDP values correspond
                                                to the minimum and maximum GDP amounts in our dataset, for the given time period and 
                                                countries."),
                                       
                                       
                                       sliderInput(
                                         inputId = "gdp_size",
                                         label = "Select GDP amount (in 2010 US Dollars):",
                                         min = 4.379e+09,
                                         max = 2.054e+13,  
                                         value = 3.533e+11  ,
                                         ticks = FALSE,
                                         sep = "")
                                       
                                     ),
                                     
                                     mainPanel(width = 7,
                                               
                                               # Creating variables to enable hovering
                                               
                                       plotOutput("allcountries",
                                                          click = "plot_click",
                                                          hover = "plot_hover"),
                                
                                  
                                               verbatimTextOutput("info"),
                                               br(), 
                                       br(),
                                       h5("Predictor Results:"),
                                               verbatimTextOutput("allcountriespredictor"))
                                     
                                     
                                   )
                                   
                                   
                                   ),
                          tabPanel("Countries where some content was withheld:",
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 5,
                                       
                                       br(),
                                       
                                       h4("Regression Results:"),
                                       
                                       helpText("In this section, we investigate the effect of a country's 
                                         GDP (indicative of a country's wealth, economic power) on whether or 
                                         not Twitter decides to comply with its content removal requests. However,
                                         unlike the previous section, in this model we will filter for the 18 countries
                                         which had some success in having content withheld by Twitter."),
                                       
                                       helpText("You can choose to visualize the relative number of total requests made
                                         whilst simultaneously looking at the relationship between the percentage of 
                                         requests complied with, against the GDP (in 2010 US dollars."),
                                       
                                       br(),
                                       
                                       # Print GT table for the second regression (selecting for 18 countries
                                       # and mapping their relationships w/ GDP)
                                       
                                       tableOutput("regres2"), 
                                       
                                       br(), 
                                       
                                       helpText("From our results, it can be interpreted that a $1 increase in the GDP of a country leads
                                         to a 3.413396e-12% increase in the percentage of requests that Twitter will comply
                                         with, upon receiving from a state This indicates a weak positive relationship 
                                         between the two variables."),
                                       
                                       # Checkbox Input to enable size value (Total Requests for Content Removal in that year)
                                       
                                       checkboxInput(
                                         inputId = "total_requests_or_no2",
                                         label = "Display total requests for content removal
                                         at each point (by size).",
                                         value = TRUE),
                                       
                                       br(), 
                                       
                                       h4("Twitter Compliance Predictor - Countries where content was withheld:"),
                                       
                                       # Slider input for predictor: 
                                       
                                       helpText("Use the slider above to select a GDP amount (in 2010 US Dollars). Based off
                                                this linear model, our predictor will return the percentage of requests for content
                                                removal Twitter will likely comply with. The minimum and maximum GDP values correspond
                                                to the minimum and maximum GDP amounts in our dataset, for the given time period and 
                                                filtered for the 18 countries where content was withheld."),
                                       
                                       
                                       sliderInput(
                                         inputId = "gdp_size2",
                                         label = "Select GDP amount (in 2010 US Dollars):",
                                         min = 1.772e+11,
                                         max = 6.203e+12,  
                                         value = 1.830e+12,
                                         ticks = FALSE,
                                         sep = "")
                                       
                                     ),
                                     
                                     mainPanel(
                                       width = 7,
                                       
                                       # Plotting the second linear regression
                                       
                                       plotOutput("bigeighteen",
                                                  click = "plot_click",
                                                  hover = "plot_hover"),
                                       
                                       
                                       verbatimTextOutput("info2"),
                                       br(), 
                                       
                                       helpText("Twitter has withheld content in the following 18 countries in response to legal
                                       demands: Argentina, Australia, Belgium, Brazil, Canada, France, Germany, India, Ireland, Israel, Japan, 
                                       Netherlands, New Zealand, Russia, South Korea, Spain, Turkey, and the United Kingdom."), 
                                       br(),
                                       h5("Predictor Results:"),
                                       verbatimTextOutput("big18predictor"))
            
                                       
                                     )
                                   ),
                          
                          tabPanel("By year:",
                                   sidebarLayout(
                                     sidebarPanel(width = 5,
                                       
                                       
                                       br(), 
                                       
                                       h4("Regression Results:"), 
                                       
                                       helpText("In this section, we investigate the effect of a country's 
                                         GDP (indicative of a country's wealth, economic power) on whether or 
                                         not Twitter decides to comply with its content removal requests. Filtering
                                         for the 18 countries where content was withheld, this section allows us the
                                         additional ability to compare the effect of GDP on Twitter removing
                                         content across years."),
                                       
                                       sliderInput(
                                         inputId = "model_year",
                                         label = "Select year:",
                                         min = 2012,
                                         max = 2018,  
                                         value = 2017,
                                         ticks = TRUE,
                                         sep = ""),
                                       
                                       br(),
                                       
                                       tableOutput("regres3"),
                                       
                                       br(),
                                       
                                       textOutput("explaingt")
                                       
                                       
                                       
                                     ),
                                     
                                     mainPanel(width = 7,
                                       plotOutput("byyear",
                                                  click = "plot_click"),
                                       
                                       
                                       verbatimTextOutput("info3"),
                                       
                                       
                                       )
                                     
                                     
                                   ))
                        
                        
                        )),
             
               tabPanel("Freedom Scores and Number of Requests Made",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Regression Results:"),
                            helpText("In this section, we investigate the effect of
                                     a country's level of freedom (a measure of how 
                                     authoritarian its government is) on the number of 
                                     requests it makes for Twitter content removal."),
                            tableOutput("regres4"),
                            helpText("From our results, it can be interpreted that a 1 point increase in the freedom score of a country leads
                                     to a 3.54 decrease of requests in the total content removal requests made by the state (a country that is more free will request for
                                     less content to be removed). This indicates a weak negative relationship between the two variables. Note that this is only a correlation, and that there may be several
                                     other variables involved in determining the number of requests made (legal notices, context specifities, hate speech,
                                     religious reasons etc.)"), 
                            helpText("However, upon further investigation, I found that there exist great regional disparities in the effect of 
                                     how free a country is on the total requests its government makes. Pick a region to visualize."),
                            pickerInput(inputId = "regionpicker",
                                        label = "Choose Region:",
                                        choices = regionsfreedom,
                                        multiple = FALSE
                                        
                            ),
                            tableOutput("regres5"),
                            textOutput("finaltext")
                            
                            
                          ),
                          
                          mainPanel(
                            plotOutput("freedomoverall",
                                       click = "plot_click"),
                            verbatimTextOutput("info4"),
                            br(),
                            plotOutput("regionfreedom",
                                       click = "plot_click1"),
                            verbatimTextOutput("info5")
                                    
                                    )
                          
                        ))
      
              
               
               
             ))
    
    
    ))


server <- function(input, output) {
  
  mydata1 <- read_csv("mydata1.csv") %>% 
    
    # Defining the levels to plot accurately
    
    mutate(year = factor(year, levels =
                           c("January - June 2012",
                             "July - December 2012",
                             "January - June 2013",
                             "July - December 2013",
                             "January - June 2014",
                             "July - December 2014",
                             "January - June 2015",
                             "July - December 2015",
                             "January - June 2016",
                             "July - December 2016",
                             "January - June 2017",
                             "July - December 2017",
                             "January - June 2018",
                             "July - December 2018",
                             "January - June 2019")))
  
  # First plot (overall variables mapped over time - all countries). Dark Red
  # Bar Chart. 
  
  output$visualoverall <- renderPlot({

    # Defining the value of the y-axis, the title, the y axis label and the caption
    # depending on the variable selected. I use if else multiple times to be able to 
    # achieve this - was rather taxing. Might want to look for better alternatives but this
    # did get the job done. 

    if(input$overallvars == "accounts_specified") {
      y_value <- mydata1$accounts_specified
      y_lab <- "Accounts specified for withholding
      by Governments"
      title1 <- "Accounts specified for withholding
       by Governments"
      caption <- "This includes the unique number of accounts identified in
      the requests Twitter received. (Source: Twitter)"
    }
    else if(input$overallvars == "total_requests_made") {
      y_value <- mydata1$total_requests_made
      y_lab <- "Total requests for content removal"
      title1 <- "Total requests made by Governments"
      caption <- "This  includes the total number of legal requests from government agencies,
      police departments, or other authorized requesters within a country."
    }
 
    else if(input$overallvars == "accounts_tos") {
      y_value <- mydata1$accounts_tos
      y_lab <- "Accounts in Violation of Twitter
    Terms of Service"
      title1 <- "Tweets/Accounts in Twitter Terms of Service (TOS) Violations"
      caption <- "Twitter completely removed the content from Twitter after determining it violated Twitter’s TOS.
    It states that it review all reported content for violations of Twitter’s TOS before assessing it further. (Source: Twitter)"
    }
    
    else if(input$overallvars == "accounts_withheld") {
      y_value <- mydata1$accounts_withheld
      y_lab <- "Accounts withheld by Twitter (from
       accounts specified)"
      title1 <- "Accounts withheld by Twitter"
      caption <- "This includes the number of accounts withheld from the
the accounts specified by Governments. (Source: Twitter)"
    }
    
    else if(input$overallvars == "tweets_withheld") {
      y_value <- mydata1$tweets_withheld
      y_lab <- "Tweets withheld by Twitter"
      title1 <- "Tweets withheld by Twitter"
      caption <- "This includes the number of tweets withheld by Twitter on
      government request. (Source: Twitter)"
    }
    
    else if(input$overallvars == "total_withholding_accounts_and_tweets") {
      y_value <- mydata1$total_withholding_accounts_and_tweets
      y_lab <- "Sum of Tweets and Accounts Withheld"
      title1 <- "Sum of tweets and accounts withheld by
Twitter over time"
      caption <-  "This includes the number of tweets and accounts withheld by Twitter on
      government request."
    }
    
    else if(input$overallvars == "requests_where_content_withheld") {
      y_value <- mydata1$requests_where_content_withheld
      y_lab <- "Government requests where some
      content withheld"
      title1 <- "Government requests where some
content withheld."
      caption <- "This  includes the total number of legal requests from government agencies,
police departments, or other authorized requesters within a country, where some content was withheld."
      
      
    }
     
    # I then use y_value to plot the data, customizing the plot for the variety of
    # captions, titles and subtitles that I specified depending on the variable selected. 

    ggplot(mydata1, aes(year, y_value)) +
      geom_col() +
      
      # I use stat summary to help avoid stacking of the cols in geom_col. 
      
      stat_summary(aes(x = year, y = y_value),
                   fun.y = sum,
                   geom = "col",
                   colour = "black",
                   fill = "darkred")+
      theme_minimal() +
      theme(legend.position = "none") +
      labs(y = y_lab,
           x = "Time Period",
           caption = caption,
           title = title1) +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8)) +
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.5))
      
})
  
  # Plotting specific to the country selected (second tab on the Data Visualizations page)
  
  output$countryvisuals <- renderPlot({
   
    # I filter for the country selected before creating the plots I require. 
    
    country_filter <-  mydata1 %>% 
      filter(country %in% input$countryid) 
    
    # Defining the value of the y-axis, the title, the y axis label and the caption
    # depending on the variable selected. I use if else multiple times to be able to 
    # achieve this - was rather taxing. Might want to look for better alternatives but this
    # did get the job done.
    
    # This is very similar to what I did in the previous output code.
    
    if(input$overallvars1 == "accounts_specified") {
      y_value <- country_filter$accounts_specified
      y_lab <- "Accounts specified for withholding
      by Governments"
      title1 <- "Accounts specified for withholding by Governments"
      caption <- "This includes the unique number of accounts identified in
      the requests Twitter received. (Source: Twitter)"
    }
    else if(input$overallvars1 == "total_requests_made") {
      y_value <- country_filter$total_requests_made
      y_lab <- "Total requests for content removal"
      title1 <- "Total requests made by Governments"
      caption <- "This  includes the total number of legal requests from government agencies,
      police departments, or other authorized requesters within a country."
    }
    
    else if(input$overallvars1 == "accounts_tos") {
      y_value <- country_filter$accounts_tos
      y_lab <- "Accounts in Violation of Twitter
    Terms of Service"
      title1 <- "Tweets/Accounts in Twitter Terms of Service (TOS) Violations"
      caption <- "Twitter completely removed the content from Twitter after determining it violated Twitter’s TOS.
    It states that it review all reported content for violations of Twitter’s TOS before assessing it further. (Source: Twitter)"
    }
    
    else if(input$overallvars1 == "accounts_withheld") {
      y_value <- country_filter$accounts_withheld
      y_lab <- "Accounts withheld by Twitter (from
       accounts specified)"
      title1 <- "Accounts withheld by Twitter"
      caption <- "This includes the number of accounts withheld from the
the accounts specified by Governments. (Source: Twitter)"
    }
    
    else if(input$overallvars1 == "tweets_withheld") {
      y_value <- country_filter$tweets_withheld
      y_lab <- "Tweets withheld by Twitter"
      title1 <- "Tweets withheld by Twitter"
      caption <- "This includes the number of tweets withheld by Twitter on
      government request. (Source: Twitter)"
    }
    
    else if(input$overallvars1 == "total_withholding_accounts_and_tweets") {
      y_value <- country_filter$total_withholding_accounts_and_tweets
      y_lab <- "Sum of Tweets and Accounts Withheld"
      title1 <- "Sum of tweets and accounts withheld by
Twitter over time"
      caption <-  "This includes the number of tweets and accounts withheld by Twitter on
      government request."
    }
    
    else if(input$overallvars1 == "requests_where_content_withheld") {
      y_value <- country_filter$requests_where_content_withheld
      y_lab <- "Government requests where some
      content was withheld"
      title1 <- "Government requests where some
content withheld."
      caption <- "This  includes the total number of legal requests from government agencies,
police departments, or other authorized requesters within a country, where some content was withheld."
      
      
    }
    
  # Plotting bar charts for each of the variables mentioned. 
  
    ggplot(country_filter, aes(year, y_value)) +
      geom_col(color = "darkgrey",
               fill = "#69b3a2") +
      
      # Using geom_label to label the maximum value of each column. 
      
      geom_label(aes(label = scales::comma(y_value)),
                 size = 2.5,
                 nudge_y = 0,
                 fill = "white") +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(y = y_lab,
           x = "Time Period",
           caption = caption,
           title = print(input$countryid)) +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8)) 
    
  
  })
  
  
  output$freedom_time <- renderPlot({
    
  # reading the joined data. This dataset contains the freedom_scores joined to
  # the specific country by year, thus allowing me to create the plots I want. This
  # is specifically important since I wanted to visualize the points with respect to the
  # number of total requests made as well, which is given to us by the Twitter data. Thus
  # joining the data (in the clean-fp.rmd) was absolutely important. 
    
  join_data <-  read_csv("freedom_twitter_gdp.csv")
  
  
  join_data %>% 
    
    # Grouping by year, and then using mutate to create the mean freedom score, as well
    # as finding the total requests made in both halves of the year. This was initially not
    # possible to find since the data was divided by Jan - Jul and June to Dec, but I renamed 
    # the halves just by the years in the clean data rmd, and used the year numbers to group 
    # and calculate the sum. 
    
    group_by(year) %>% 
    mutate(mean_score = mean(freedom_score)) %>%
    mutate(totalreqyr = sum(total_requests_made)) %>% 
    ungroup() %>%
    
    # I filter for the country selected
    
    filter(country == input$countryfreedom) %>%
    ggplot(aes(year, freedom_score)) +
    
    # Creating two different lines/scatter plots - one for the country selected, 
    # and one that remains constant throughout - the mean scores across each year. 
    
    geom_line(aes(year, freedom_score),
              color = "grey") +
    geom_point(aes(size = total_requests_made),
               shape=21,
               color="black",
               alpha = 0.5,
               fill="#69b3a2") +
    geom_line(aes(year, mean_score),
              color = "grey",
              linetype = "dashed") +
    geom_point(aes(year, mean_score, size = totalreqyr),
               color = "black",
               shape = 21,
               alpha = 0.5,
               fill = "darkred") +
    theme_minimal() +
    labs(y = "Freedom Score",
         x = "Year",
         title = print(input$countryfreedom),
         subtitle = "Freedom score and total requests made for content withholding (2013-2019)",
         caption = "
Dashed line representes the year's mean freedom score for all countries. The solid line represents
the specific country's freedom scores. Data from Freedom House.",
         size = "Total Requests Made") +
    theme(plot.title = element_text(face = "bold",
                                    size = 15,
                                    hjust = 0.5),
          plot.subtitle = element_text(face = "italic",
                                       size = 10,
                                       hjust = 0.5),
          plot.caption = element_text(face = "italic",
                                      hjust = 0.5,
                                      size = 12)) 
   
  
    
    
    
 })

# Plot for Model 1     
  
  output$allcountries <- renderPlot({
    
 twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv")
 
# Display Size option - this allows us to select for whether or not 
# we want to visualize the number of total requests made that correspond
# to the given plot. I do this using if else, as in the previous outputs. 

 if(input$total_requests_or_no == TRUE) {
   size_value <- twitter_gdp_model$sum_total_requests_made_yr
   size_lab <- "Total Requests Made"
   
 }

 else if(input$total_requests_or_no == FALSE) {
   size_value <- NULL
   size_lab <- NULL
 }
 
 # I then proceed to plot the data of the percentage of requests where content was 
 # withheld against the GDP of that specific country at that point in time. I use 
 # geom_smooth with method lm to determine the regression line. 
    
 twitter_gdp_model %>%  
   ggplot(aes(gdp, sum_percentage_where_content_withheld_yr, size = size_value)) +
   geom_point(alpha = 0.3,
              shape=21,
              color="black",
              fill="#69b3a2") +
   scale_x_log10() +
   geom_smooth(aes(gdp, 
                   sum_percentage_where_content_withheld_yr),
               method = "lm", 
               color = "black") +
   theme_minimal() +
   labs(y = "Percentage of requests where some
      content was withheld",
        x = "GDP (2010 US Dollars)",
        size = "Total Requests Made",
        title = "Percentage of Requests Where Some Content was Withheld 
    against GDP (2010 US Dollars)",
        subtitle = "For the years 2012-2018") +
 
 theme(plot.title = element_text(face = "bold",
                                 size = 15,
                                 hjust = 0.5),
       plot.subtitle = element_text(face = "italic",
                                    size = 10,
                                    hjust = 0.5))

    
    
  })
  
  # Text for the clicks. This was honestly just because I felt like my project lacked
  # cool visuals/fancy options. Clicking on any of the points on the plot states the 
  # y and x axis at the position where the plot was clicked. I specified the code to produce
  # exactly what I wanted to see in terms of the variables, replacing 'x' and 'y' as in the 
  # initial code that I found online. 
  
  output$info <- renderText({

    xy_str <- function(e) {
      if(is.null(e)) return("-\n")
      paste0("GDP (2010 US Dollars) = $", round(e$x, 1),"\n", "Percentage Requests where content withheld = ", round(e$y, 1), "%", "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0("Click on the points on the plot for specific values:",
           "\n",
      "Results: ", xy_str(input$plot_click)
      
       )

    



  })
  
  
  #Regression model GT Table
 
  output$regres1 <- renderTable(align = 'c', { 
    
    
    # Here, I print the GT table for the first regression that I run. Standard code, and edits. 
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv")
    
    twitter_gdp_model %>%
      lm(sum_percentage_where_content_withheld_yr ~ gdp, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) %>%
      gt() %>% 
      cols_label(term = "Term",
                 estimate = "Estimate",
                 conf.low = "Lower Bound",
                 conf.high = "Upper Bound")
    
    
    
  })
  
  #Predictor Model 1 
  
  output$allcountriespredictor <- renderText({
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv")
    
    # I feed the linear model to the allcountriesmodel object. 
    
   allcountriesmodel <-  twitter_gdp_model %>% 
      lm(sum_percentage_where_content_withheld_yr ~ gdp, data = .) 
    
   
   # I then use the predict function by specifying the gdp value to correspond to 
   # whatever value is selected on the slider input, and also retrieving confidence 
   # intervals. I then tidy this data using the tidy function, and pull for the fit value. 
   
   fit <-  predict(allcountriesmodel,
            newdata = tibble(gdp = input$gdp_size), 
            interval = "confidence") %>% 
            tidy() %>% 
            pull(fit) %>% 
     round(3)
   
   # As in the previous comment chunk, I now pull for the lower bound of the confidence interval.
   
   lwr <-  predict(allcountriesmodel,
                   newdata = tibble(gdp = input$gdp_size), 
                   interval = "confidence") %>% 
     tidy() %>% 
     pull(lwr) %>% 
     round(3)
   
   # Pulling for the upper bound of the confidence interval. 
   
   upr <-  predict(allcountriesmodel,
                   newdata = tibble(gdp = input$gdp_size), 
                   interval = "confidence") %>% 
     tidy() %>% 
     pull(upr) %>% 
     round(3)
  
   # Creating the output generated upon changing the GDP on the slider input. This customizes 
   # the statement to values corresponding with the GDP value on the model.
   
   paste0("According to this linear regression model, the GDP amount (in US Dollars) that you selected
indicates that Twitter is likely to comply with ", fit, "% of your requests. The 95% prediction interval of Twitter's 
compliance with your requests is between ", lwr, "% and ", upr, "%."," Note that this is only a correlation, since
there are likely other factors involved in determining the number of requests where some content is withheld.
This relationship does not prove causation.") 

   
  })
  
  # Plot Model 2 

  output$bigeighteen <- renderPlot({

    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv") %>% 
      
      # Filtering for countries where some content was withheld (or as I like to call them,
      # the big 18).
      
      filter(country %in% big18countries)
    
    # Display Size option - this allows us to select for whether or not 
    # we want to visualize the number of total requests made that correspond
    # to the given plot. I do this using if else, as in the previous outputs. 
    
    if(input$total_requests_or_no2 == TRUE) {
      size_value <- twitter_gdp_model$sum_total_requests_made_yr
      size_lab <- "Total Requests Made"

    }

    else if(input$total_requests_or_no2 == FALSE) {
      size_value <- NULL
      size_lab <- NULL
    }
    
    # I then proceed to plot the data of the percentage of requests where content was 
    # withheld against the GDP of that specific country at that point in time. I use 
    # geom_smooth with method lm to determine the regression line. 
    
 twitter_gdp_model %>% 
      ggplot(aes(gdp, sum_percentage_where_content_withheld_yr, size = size_value)) +
      geom_point(alpha = 0.3,
                 shape = 21,
                 color = "black",
                 fill = "darkred") +
      theme_minimal() +
      scale_x_log10() +
      geom_smooth(method = "lm", color = "blue") +
      labs(y = "Percentage of requests where some
      content was withheld",
           x = "Log of GDP (2010 US Dollars)",
           size = "Total Requests Made",
           title = "Percentage of Requests Where Some Content was Withheld 
    against GDP (2010 US Dollars)",
           subtitle = "Includes data only for the 18 countries where content was withheld, in the years 2012-2018") +
      
      theme(plot.title = element_text(face = "bold",
                                      size = 15,
                                      hjust = 0.5),
            plot.subtitle = element_text(face = "italic",
                                         size = 10,
                                         hjust = 0.5))
    
    
    



  })
 
  #Predictor Model 2 (Big 18)

  output$big18predictor <- renderText({
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv") %>% 
      filter(country %in% big18countries)
    
    filtercountrymodel <-  twitter_gdp_model %>% 
      lm(sum_percentage_where_content_withheld_yr ~ gdp, data = .) 
    
    
    
    # I then use the predict function by specifying the gdp value to correspond to 
    # whatever value is selected on the slider input, and also retrieving confidence 
    # intervals. I then tidy this data using the tidy function, and pull for the fit value. 
    # The rest of the code follows as in the previous output code (for the first predictor).
    
    fit <-  predict(filtercountrymodel,
                    newdata = tibble(gdp = input$gdp_size2), 
                    interval = "confidence") %>% 
      tidy() %>% 
      pull(fit) %>% 
      round(3)
    
    lwr <-  predict(filtercountrymodel,
                    newdata = tibble(gdp = input$gdp_size2), 
                    interval = "confidence") %>% 
      tidy() %>% 
      pull(lwr) %>% 
      round(3)
    
    upr <-  predict(filtercountrymodel,
                    newdata = tibble(gdp = input$gdp_size2), 
                    interval = "confidence") %>% 
      tidy() %>% 
      pull(upr) %>% 
      round(3)
    
    # The 95% prediction interval of the eruption duration for the waiting time of 80 minutes is between 3.1961 and 5.1564 minutes.
    paste0("According to this linear regression model, the GDP amount (in US Dollars) that you selected
indicates that Twitter is likely to comply with ", fit, "% of your requests. The 95% prediction interval of Twitter's 
compliance with your requests is between ", lwr, "% and ", upr, "%."," Note that this is only a correlation, since
there are likely other factors involved in determining the number of requests where some content is withheld.
This relationship does not prove causation.") 
    
    
  })
  
  # Printing the second regression Gt table. Pretty standard. Just filtered for 
  # the big 18 here. 
  
  output$regres2 <- renderTable(align = 'c', { 
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv") %>% 
      filter(country %in% big18countries)
    
    twitter_gdp_model %>%
      lm(sum_percentage_where_content_withheld_yr ~ gdp, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) %>%
      gt() %>% 
      cols_label(term = "Term",
                 estimate = "Estimate",
                 conf.low = "Lower Bound",
                 conf.high = "Upper Bound")
    
    
    
  })
  
  
  # Same hover/clicker fancy option to make plot look better. Refer to previous comments. 
  
  output$info2 <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("-\n")
      paste0("GDP (2010 US Dollars) = $", round(e$x, 1),"\n", "Percentage Requests where content withheld = ", round(e$y, 1), "%", "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0("Click on the points on the plot for specific values:",
           "\n",
           "Results: ", xy_str(input$plot_click)
           
    )

  })
  
  # The third linear regression models. Here I change the models by year, thus filtering
  # for the year selected in the slider input. I also filter for the big 18 countries. 
  
  output$byyear <- renderPlot({
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv") %>% 
      filter(country %in% big18countries) %>% 
      filter(year == input$model_year)
    
    # Plotting the scatter plot and using geom_smooth to plot the linear model. 
    
    twitter_gdp_model %>% 
      ggplot(aes(gdp, sum_percentage_where_content_withheld_yr, size = sum_total_requests_made_yr)) +
      geom_point(alpha = 0.6,
                 shape = 21,
                 color = "black",
                 fill = "darkgreen") +
      theme_minimal() +
      scale_x_log10() +
      geom_smooth(method = "lm", color = "blue") +
      labs(y = "Percentage of requests where some
      content was withheld",
           x = "Log of GDP (2010 US Dollars)",
           size = "Total Requests Made",
           title = "Percentage of Requests Where Some Content was Withheld 
    against GDP (2010 US Dollars)",
           subtitle = print(input$model_year)) +
      
      theme(plot.title = element_text(face = "bold",
                                      size = 15,
                                      hjust = 0.5),
            plot.subtitle = element_text(face = "italic",
                                         size = 10,
                                         hjust = 0.5))
    
    
    
    
  })
  
  # Printing the third gt (this one is subject to change depending on the year selected by the 
  # user.)
  
  output$regres3 <- renderTable(align = 'c', { 
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv") %>% 
      filter(country %in% big18countries) %>% 
      filter(year == input$model_year)
    
    # Standard gt print for regression. 
    
    twitter_gdp_model %>%
      lm(sum_percentage_where_content_withheld_yr ~ gdp, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) %>%
      gt() %>% 
      cols_label(term = "Term",
                 estimate = "Estimate",
                 conf.low = "Lower Bound",
                 conf.high = "Upper Bound")
    
  })
  
  # Fancy click stuff. 
  
  output$info3 <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("-\n")
      paste0("GDP (2010 US Dollars) = $", round(e$x, 1),"\n", "Percentage Requests where content withheld = ", round(e$y, 1), "%", "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0("Click on the points on the plot for specific values:",
           "\n",
           "Results: ", xy_str(input$plot_click)
           
    )
    
  })
  
  # Here I print the interpretation of the results put forth by the plot and the gt
  # both. I pull for the estimate term, which is of relevance to this passage, since I 
  # seek to explain what a $1 rise in the GDP is going to look like in terms of the 
  # percentage of requests where some content will be withheld. 
  
  output$explaingt <- renderText({
    
    twitter_gdp_model <- read_csv("twitter_gdp_modeling.csv") %>% 
      filter(country %in% big18countries) %>% 
      filter(year == input$model_year)
    
  estimate <-
    twitter_gdp_model %>% 
      lm(sum_percentage_where_content_withheld_yr ~ gdp, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) %>%  
      select(estimate) %>% 
      tail(1) %>% 
      pull(estimate)
  
  paste0("From our results, it can be interpreted that in the year ", print(input$model_year), " a $1
  increase in the GDP of a country leads to a ", estimate, "% increase in the percentage of requests that
  Twitter will comply with, upon receiving from a state.")
    
  })
  
  
  output$freedomoverall <- renderPlot({
    
    # reading freedom score and twitter joined data. 
    
     freedom <- read_csv("freedom_twitter copy.csv")
     
     # plotting freedom score against the sum of total requests made per year
     # by each country. 
     
     freedom %>% 
       ungroup() %>% 
       ggplot(aes(freedom_score, sum_total_requests_made_yr)) +
       scale_y_log10() +
       geom_point(alpha = 0.6,
                  shape = 21,
                  color = "black",
                  fill = "gold") +
       geom_smooth(method = "lm") + 
       theme_classic() +
       labs(y = "Total Requests Made",
            x = "Freedom Score",
            size = "Total Requests Made",
            title = "Effect of Country Freedom Score on Total Requests Made",
            subtitle = "For the years 2013-2019", 
            caption = "Data from Freedom House.") +
       theme(plot.title = element_text(face = "bold",
                                       size = 15,
                                       hjust = 0.5),
             plot.subtitle = element_text(face = "italic",
                                          size = 10,
                                          hjust = 0.5))
     
     
    
  })
  
  
  # Printing GT for regression 4. 
  
  output$regres4 <- renderTable({
    
    freedom <- read_csv("freedom_twitter copy.csv")
    
    freedom %>%
      lm(sum_total_requests_made_yr ~ freedom_score, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) 
    
    
  })
  
  output$info4 <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("-\n")
      paste0("Freedom Score = ", round(e$x, 1),"\n", "Total Requests Made = ", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0("Click on the points on the plot for specific values:",
           "\n",
           "Results: ", xy_str(input$plot_click)
           
    )
    
  })
  
  
  output$regionfreedom <- renderPlot({
    
    freedom <- read_csv("freedom_twitter copy.csv")
    
    
    freedom %>% 
      filter(region == input$regionpicker) %>% 
      ggplot(aes(freedom_score, sum_total_requests_made_yr, fill = input$region)) +
      scale_y_log10() +
      geom_point(alpha = 0.6,
                 shape = 25,
                 color = "black",
                 fill = "red") +
      geom_smooth(method = "lm") + 
      theme_classic() +
      labs(y = "Total Requests Made",
           x = "Freedom Score",
           title = "Effect of Country Freedom Score on Total Requests Made - Region Specific",
           subtitle = print(input$regionpicker), "For the years 2013-2019", 
           caption = "Data from Freedom House.") +
      theme(plot.title = element_text(face = "bold",
                                      size = 15,
                                      hjust = 0.5),
            plot.subtitle = element_text(face = "italic",
                                         size = 14,
                                         hjust = 0.5))
    
    
    
  })
  
  
  output$info5 <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("-\n")
      paste0("Freedom Score = ", round(e$x, 1),"\n", "Total Requests Made = ", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0("Click on the points on the plot for specific values:",
           "\n",
           "Results: ", xy_str(input$plot_click1)
           
    )
    
  })
  
  
  output$regres5 <- renderTable({
    
    freedom <- read_csv("freedom_twitter copy.csv")
    
    freedom %>%
      filter(region == input$regionpicker) %>% 
      lm(sum_total_requests_made_yr ~ freedom_score, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) 
    
    
  })
  
  
  output$finaltext <- renderText({
    
    
    freedom <- read_csv("freedom_twitter copy.csv") %>% 
    filter(region == input$regionpicker) 
    
     estimate <-
      freedom %>%
      lm(sum_total_requests_made_yr ~ freedom_score, data = .) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high) %>%  
      select(estimate) %>% 
      tail(1) %>% 
      pull(estimate) %>% 
      round(3)

    
    paste0("From our results, it can be interpreted that in the region ", print(input$regionpicker), " a 1 point
  increase in the freedom score of a country leads to a ", estimate, " increase in the total number of requests that
  country's government will make. Note that this is only a correlation, and that there may be several other region,
           country, or context specific factors at play that determine the number of requests made.")
    
    
  })
  
}


# Run the App

shinyApp(ui = ui, server = server)
