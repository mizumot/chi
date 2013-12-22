library(shiny)


shinyUI(bootstrapPage(


    headerPanel("Chi-square Test"),


    sidebarPanel(

        radioButtons("type", strong("Test and data input type:"),
                    list("Test of goodness of fit (Raw data)" = "goodraw",
                         "Test of goodness of fit (Tabulated data)" = "goodtab",
                         "Test of Independence (Raw data)" = "indraw",
                         "Test of Independence (Tabulated data)" = "indtab"
                        ),'Test of Independence (Raw data)'
        ),

        br()

    ),




mainPanel(



    tabsetPanel(

        tabPanel("Main",

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

            aceEditor("text", value="Sex\tEffect\nM\tNo\nW\tNo\nW\tNo\nM\tNo\nM\tYes\nM\tYes\nM\tYes\nM\tNo\nW\tYes\nM\tNo\nW\tYes\nM\tNo\nM\tYes\nM\tNo\nM\tNo\nM\tYes\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nM\tYes\nM\tNo\nM\tNo\nM\tYes\nM\tYes\nW\tYes\nM\tNo\nM\tYes\nW\tYes\nM\tNo\nM\tNo\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nM\tNo\nW\tNo\nW\tYes\nM\tYes\nW\tYes\nM\tNo\nM\tYes\nW\tYes\nM\tYes\nW\tYes\nM\tYes\nM\tNo\nM\tNo\nW\tNo\nW\tNo\nM\tYes\nW\tNo\nM\tYes\nW\tYes\nW\tYes\nM\tNo\nM\tNo\nM\tYes\nW\tYes\nM\tNo\nW\tYes\nW\tYes\nM\tYes\nW\tNo\nW\tYes\nM\tNo\nW\tYes\nW\tNo\nM\tYes",
                mode="r", theme="cobalt"),

            br(),

            h3("Checking the input data (Contingency table)"),
            verbatimTextOutput("data.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test.out"),

            br(),

            h3("Plot"),
            downloadButton('downloadpPlot', 'Download the plot as pdf'),

            plotOutput("pPlot"),

            br(),

            conditionalPanel(condition = "input.type == 'indraw'&&'indtab'",
                downloadButton('downloadmPlot', 'Download the plot as pdf')
            ),

            plotOutput("mPlot", height = "550px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info.out")
            ),



    tabPanel("Input Examples",

p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

br(),

p(strong("Test of goodness of fit (Raw data)")),
aceEditor("text1", value="L1\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nJapanese\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nThai\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese\nChinese", mode="r", theme="solarized_light"),


br(),
p(strong("Test of goodness of fit (Tabulated data)")),
aceEditor("text2", value="Japanese\tThai\tChinese\n18\t24\t48", mode="r", theme="solarized_light"),


br(),
p(strong("Test of Independence (Raw data)")),
aceEditor("text3", value="Sex\tEffect\nM\tNo\nW\tNo\nW\tNo\nM\tNo\nM\tYes\nM\tYes\nM\tYes\nM\tNo\nW\tYes\nM\tNo\nW\tYes\nM\tNo\nM\tYes\nM\tNo\nM\tNo\nM\tYes\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nM\tYes\nM\tNo\nM\tNo\nM\tYes\nM\tYes\nW\tYes\nM\tNo\nM\tYes\nW\tYes\nM\tNo\nM\tNo\nW\tYes\nW\tYes\nW\tYes\nW\tYes\nM\tNo\nW\tNo\nW\tYes\nM\tYes\nW\tYes\nM\tNo\nM\tYes\nW\tYes\nM\tYes\nW\tYes\nM\tYes\nM\tNo\nM\tNo\nW\tNo\nW\tNo\nM\tYes\nW\tNo\nM\tYes\nW\tYes\nW\tYes\nM\tNo\nM\tNo\nM\tYes\nW\tYes\nM\tNo\nW\tYes\nW\tYes\nM\tYes\nW\tNo\nW\tYes\nM\tNo\nW\tYes\nW\tNo\nM\tYes",mode="r", theme="solarized_light"),


br(),
p(strong("Test of Independence (Tabulated data)")),
aceEditor("text4", value="No\tYes\nM\t20\t18\nW\t8\t24", mode="r", theme="solarized_light"),

br()

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

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
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
