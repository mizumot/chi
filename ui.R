library(shiny)
library(shinyAce)


shinyUI(bootstrapPage(


    headerPanel("Chi-square Test"),


########## Adding loading message #########

tags$head(tags$style(type="text/css", "
#loadmessage {
position: fixed;
top: 0px;
left: 0px;
width: 100%;
padding: 10px 0px 10px 0px;
text-align: center;
font-weight: bold;
font-size: 100%;
color: #000000;
background-color: #CCFF66;
z-index: 105;
}
")),

conditionalPanel(condition="$('html').hasClass('shiny-busy')",
tags$div("Loading...",id="loadmessage")),

########## Added up untill here ##########



    mainPanel(
        tabsetPanel(position = "left", selected = "Test of Independence (Tabulated data)",

        tabPanel("Test of goodness of fit (Raw data)",

            h2("Test of goodness of fit (Raw data)"),

            h4("One nominal variable"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text1", value="L1\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data1.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test1.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot1"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info1.out")
            ),









        tabPanel("Test of goodness of fit (Tabulated data)",

            h2("Test of goodness of fit (Tabulated data)"),

            h4("One nominal variable"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text2", value="Japanese\tThai\tChinese\n18\t24\t48", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data2.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test2.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot2"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info2.out")
            ),










        tabPanel("Test of Independence (Raw data)",

            h2("Test of Independence (Raw data)"),

            h4("Two or more than two nominal variables"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text3", value="Sex\tEffect\nM\tNo\nW\tNo\nW\tNo\nM\tNo\nM\tYes\nM\tYes\nM\tYes\nM\tNo\nW\tYes\nM\tNo\nW\tYes\nM\tNo\nM\tYes\nM\tNo\nM\tNo\nM\tYes\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nM\tYes\nM\tNo\nM\tNo\nM\tYes\nM\tYes\nW\tYes\nM\tNo\nM\tYes\nW\tYes\nM\tNo\nM\tNo\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nM\tNo\nW\tNo\nW\tYes\nM\tYes\nW\tYes\nM\tNo\nM\tYes\nW\tYes\nM\tYes\nW\tYes\nM\tYes\nM\tNo\nM\tNo\nW\tNo\nW\tNo\nM\tYes\nW\tNo\nM\tYes\nW\tYes\nW\tYes\nM\tNo\nM\tNo\nM\tYes\nW\tYes\nM\tNo\nW\tYes\nW\tYes\nM\tYes\nW\tNo\nW\tYes\nM\tNo\nW\tYes\nW\tNo\nM\tYes",mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data3.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test3.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot3"),

            br(),

            plotOutput("mPlot3", height = "550px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info3.out")
            ),









        tabPanel("Test of Independence (Tabulated data)",

            h2("Test of Independence (Tabulated data)"),

            h4("Two or more than two nominal variables"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text4", value="\tNo\tYes\nM\t20\t18\nW\t8\t24", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data4.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test4.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot4"),

            br(),

            plotOutput("mPlot4", height = "550px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info4.out")
            ),










        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(pwr)'),br(),
            code('library(vcd)'),br(),

            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/chi', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("chi","mizumot")')
            ),

            p('I referred to',
            a("js-STAR", href="http://www.kisnet.or.jp/nappa/software/star/", target="_blank"),
            'for some parts of the codes. I would like to thank the authors of js-STAR, the very fast and excellent online software.'),

            br(),

            strong('Citation in Publications'),
            p('Mizumoto, A. (2015). Langtest (Version 1.0) [Web application]. Retrieved from http://langtest.jp'),

            br(),

            strong('Article'),
            p('Mizumoto, A., & Plonsky, L. (2015).', a("R as a lingua franca: Advantages of using R for quantitative research in applied linguistics.", href='http://applij.oxfordjournals.org/content/early/2015/06/24/applin.amv025.abstract', target="_blank"), em('Applied Linguistics,'), 'Advance online publication. doi:10.1093/applin/amv025'),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="https://sites.google.com/site/casualmacr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

)
)
)
))