library(shiny)
library(shinyAce)
library(pwr)
library(vcd)



shinyServer(function(input, output) {



#----------------------------------------------------
# 1. Test of goodness of fit (Raw data)
#----------------------------------------------------

    data1 <- reactive({
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            n <- sum(x)
            x <- c(x, Sum=n)
            
            print(x)
        })
    
        output$data1.out <- renderPrint({
            data1()
        })
    
    
    
    
    
    test1 <- reactive({
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
        
            x <- table(dat)
            chi <- chisq.test(x)
            print(chi)
        
            P0 <- rep(1/length(x), times = length(x))  # Expected ratio
            P1 <- x/sum(x)                             # Observed ratio
            w <- ES.w1(P0, P1)                         # Effect size ｗ (Large=0.5, Medium=0.3, Small=0.1)
            
            cat("Effect size w =", round(w, 3), "\n")


            cat("\n", "---", "\n", "Multiple comparisons (p-value adjusted with Bonferroni method):", "\n", "\n")
            
            itemNum <- 0
            for (i in 1:length(chi$observed)) {
                for (j in 1:length(chi$observed)) {
                    if (i <= j) {next}
                    z <- (abs(chi$observed[[i]]-chi$observed[[j]])-1)/sqrt(chi$observed[[i]]+chi$observed[[j]])
                    p <- pnorm(z, lower.tail=FALSE)*2
                    p <- p.adjust(p, method = "bonferroni", n = length(x))
                    n <- chi$observed[[i]] + chi$observed[[j]]
                    p00 <- c(0.5, 0.5) # 母比率同等
                    p11 <- c(chi$observed[[i]]/n, chi$observed[[j]]/n) # 標本比率
                    p0  <- p00[1]
                    p1  <- p11[1]
                    ESg <- p1-p0 # 効果量g
                    cat(names(x)[j],"vs",names(x)[i],":", "z =", sprintf("%.3f",round(z,3)), ",", "p =", sprintf("%.3f",round(p,3)), ",", "Effect size g =", round(ESg,3), "\n")
                    itemNum <- itemNum + 1
                }
            }
    })
    
    output$test1.out <- renderPrint({
        test1()
    })
    
    
    
    
    
    makepPlot1 <- function(){
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            
            P0 <- rep(1/length(x), times = length(x))  # Expected ratio
            P1 <- x/sum(x)                             # Observed ratio
            
            par(mar=c(5,6,2,4))
            z <- matrix(c(P0, P1), nc=length(x), by=1)
            colnames(z) <- names(x)
            rownames(z) <- c("Expected", "Observed")
            barplot(t(z), hor=1, las=1, xlab="Percentage", col=gray.colors(length(x)))
            
            legend("bottomright",legend=colnames(z), fill=gray.colors(length(x)))
            
    }
    
    output$pPlot1 <- renderPlot({
        print(makepPlot1())
    })
    
    
    
    
    
    info1 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info1.out <- renderPrint({
        info1()
    })










#----------------------------------------------------
# 2. Test of goodness of fit (Tabulated data)
#----------------------------------------------------

    data2 <- reactive({
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
            n <- sum(dat)
            x <- data.frame(dat, Sum = n)
            rownames(x) <- ""
            
            print(x)
        })
    
        output$data2.out <- renderPrint({
            data2()
        })
    
    
    
    
    
    test2 <- reactive({
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
            x <- dat
            chi <- chisq.test(x)
            print(chi)
            
            P0 <- rep(1/length(x), times = length(x))  # Expected ratio
            P1 <- x/sum(x)                             # Observed ratio
            w <- ES.w1(P0, P1)                         # Effect size ｗ (Large=0.5, Medium=0.3, Small=0.1)
        
        cat("Effect size w =", round(w, 3), "\n")
        
        
        cat("\n", "---", "\n", "Multiple comparisons (p-value adjusted with Bonferroni method):", "\n", "\n")
        
        itemNum <- 0
        for (i in 1:length(chi$observed)) {
            for (j in 1:length(chi$observed)) {
                if (i <= j) {next}
                z <- (abs(chi$observed[[i]]-chi$observed[[j]])-1)/sqrt(chi$observed[[i]]+chi$observed[[j]])
                p <- pnorm(z, lower.tail=FALSE)*2
                p <- p.adjust(p, method = "bonferroni", n = length(x))
                n <- chi$observed[[i]] + chi$observed[[j]]
                p00 <- c(0.5, 0.5) # 母比率同等
                p11 <- c(chi$observed[[i]]/n, chi$observed[[j]]/n) # 標本比率
                p0  <- p00[1]
                p1  <- p11[1]
                ESg <- p1-p0 # 効果量g
                cat(names(x)[j],"vs",names(x)[i],":", "z =", sprintf("%.3f",round(z,3)), ",", "p =", sprintf("%.3f",round(p,3)), ",", "Effect size g =", round(ESg,3), "\n")
                itemNum <- itemNum + 1
            }
        }
    })
    
    output$test2.out <- renderPrint({
        test2()
    })
    
    
    
    
    
    makepPlot2 <- function(){
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
            x <- dat
        
            P0 <- rep(1/length(x), times = length(x))  # 期待比率
            P1 <- x/sum(x)               # 標本比率
        
            par(mar=c(5,6,2,4))
            z <- matrix(c(P0, P1), nc=length(x), by=1)
            colnames(z) <- names(x)
            rownames(z) <- c("Expected", "Observed")
            barplot(t(z), hor=1, las=1, xlab="Percentage", col=gray.colors(length(x)))
        
            legend("bottomright",legend=colnames(z), fill=gray.colors(length(x)))
    }
    
    output$pPlot2 <- renderPlot({
        print(makepPlot2())
    })
    
    
    
    
    
    info2 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info2.out <- renderPrint({
        info2()
    })










#----------------------------------------------------
# 3. Test of Independence (Raw data)
#----------------------------------------------------

    data3 <- reactive({
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            x <- addmargins(x)
        
            print(x)
        })
    
        output$data3.out <- renderPrint({
            data3()
        })
    
    
    
    
    
    test3 <- reactive({
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            
            a <- chisq.test(x, correct=F)           # Pearson's Chi-squared
            b <- chisq.test(x)                      # Yates
            c <- assocstats(x)                      # Likelihood Ratio
            
            
            aa <- data.frame(a[4], a[1], a[2], a[3])
            aa[1] <- c("Pearson's Chi-squared")
            row.names(aa) <- NULL
            
            bb <- data.frame(b[4], b[1], b[2], b[3])
            bb[1] <- c("Yates' Continuity Correction")
            row.names(bb) <- NULL
            
            cc <- data.frame(c[2])[1,]
            cc <- data.frame(c("Likelihood Ratio"), cc[,1], cc[,2], cc[,3])
            names(cc) <- c("method", "statistic", "parameter", "p.value")
            
            
            # Fisher's Exact Test
            e <- try(fisher.test(x, workspace=3000000), silent = FALSE)   # try, fisher.test
            if (class(e) == "try-error") {
                dd <- data.frame(c(""), c(""), c(""), c("data too large"))  # if error
                dd[1] <- c("Fisher's Exact Test")
                names(dd) <- c("method", "statistic", "parameter", "p.value")
                row.names(dd) <- NULL
            } else {
                d <- fisher.test(x, workspace=3000000)    # no error
                if (nrow(x) * ncol(x) == 4) { # Only for the 2×2 table
                    dd <- data.frame(d[6], c(""), c(""), d[1])
                } else {
                    dd <- data.frame(d[3], c(""), c(""), d[1])
                }
                dd[1] <- c("Fisher's Exact Test")
                names(dd) <- c("method", "statistic", "parameter", "p.value")
                row.names(dd) <- NULL
            }
            
            
            res <- rbind(aa, bb, cc, dd)
            res[2] <- round(as.numeric(res[,2]), 4)
            res[2][4,] <- ""
            names(res) <- c("Test", "X-squared", "df", "p-value")
            print(res)
            
            
            cat("\n")
            cat("\n", "------------------------------------------------------", "\n",
            "Effect size:", "\n")
            
            # Cramer's V [95%CI]
            V <- sqrt(chisq.test(x, correct=F)$statistic[[1]]/(sum(x)*(min(nrow(x),ncol(x)-1))))
            fisherZ <- 0.5 * log((1 + V)/(1 - V))
            fisherZ.se <- 1/sqrt(sum(x) - 3) * qnorm(1 - ((1 - 0.95)/2))
            fisherZ.ci <- fisherZ + c(-fisherZ.se, fisherZ.se)
            ci.V <- (exp(2 * fisherZ.ci) - 1)/(1 + exp(2 * fisherZ.ci))
            
            cat("\n", "Cramer's V [95%CI] =", V, "[", ci.V[1], ",",ci.V[2],"]", "\n")

            # Odds Ratio
            if (nrow(x) * ncol(x) == 4) { # Print odds ratio only for the 2×2 table
                
                oddsrt <- (x[1,1]/x[1,2]) / (x[2,1]/x[2,2])
                oddsrt.log <- log((x[1,1]/x[1,2]) / (x[2,1]/x[2,2]))
                oddsrt.log.var <- 1/x[1,1] + 1/x[1,2] + 1/x[2,1] + 1/x[2,2]
                ci.lwrupr <- exp(oddsrt.log + qnorm(c(0.025,0.975)) * sqrt(oddsrt.log.var))
                cat("\n", "Odds Ratio [95%CI] =", oddsrt, "[", ci.lwrupr[1], ",",ci.lwrupr[2],"]", "\n")
                
            } else {
                NULL
            }
            
            
            
            cat("\n", "\n", "------------------------------------------------------", "\n", "Residual analysis:", "\n")
            res <- chisq.test(x)  # 検定結果を代入
            
            cat("\n", "[Expected values]", "\n")
            print(res$expected) # 期待値
            
            cat("\n", "[Standardized residuals]", "\n")
            print(res$residuals) # 標準化残差
            
            cat("\n", "[Adjusted standardized residuals]", "\n")
            print(res$residuals/sqrt(outer(1-rowSums(x)/sum(x), 1-colSums(x)/sum(x)))) # 調整済み標準化残差
            
            cat("\n", "[p-values of adjusted standardized residuals (two-tailed)]", "\n")
            print(round(2*(1-pnorm(abs(res$residuals/sqrt(outer(1-rowSums(x)/sum(x), 1-colSums(x)/sum(x)))))),3)) # 残差の調整後有意確率（両側確率）
            
            
            
            # 多重比較
            if (nrow(x) < 3) { # Not showing if the number of row is 2
                
                cat("\n")
                
            } else {
                
                cat("\n", "\n", "------------------------------------------------------", "\n", "Multiple comparisons (p-value adjusted with Bonferroni method):", "\n", "\n")
                
                levI <- nrow(x)
                levJ <- ncol(x)
                N <- sum(x)
                dosu <- as.vector(t(x))
                M <- min(c(levI, levJ))
                
                V.95CI <- function(x) {
                    V <- sqrt(chisq.test(x, correct=F)$statistic[[1]]/(sum(x)*(min(nrow(x),ncol(x)-1))))
                    fisherZ <- 0.5 * log((1 + V)/(1 - V))
                    fisherZ.se <- 1/sqrt(sum(x) - 3) * qnorm(1 - ((1 - 0.95)/2))
                    fisherZ.ci <- fisherZ + c(-fisherZ.se, fisherZ.se)
                    ci.V <- (exp(2 * fisherZ.ci) - 1)/(1 + exp(2 * fisherZ.ci))
                    return(ci.V)
                }
                
                # エラーメッセージが出る場合の例外処理
                e <- try(fisher.test(x, workspace=3000000), silent = FALSE)   # try でfisher.test関数を実行
                if (class(e) == "try-error") { # if error, without fisher.p
                    
                    a <- c()
                    stat <- c()
                    jiyu <- c()
                    pchi <- c()
                    v <- c()
                    v.lower <- c()
                    v.upper <- c()
                    for(i in 1:(levI-1))
                    {
                        for(k in (i+1):levI)
                        {
                            ds <- c()
                            for(j in 1:levJ)
                            {
                                ds <- c(ds, dosu[(i-1)*levJ+j])
                            }
                            for(j in 1:levJ)
                            {
                                ds <- c(ds, dosu[(k-1)*levJ+j])
                            }
                            
                            kaik <- c()
                            kaik <- chisq.test(matrix(ds, nr=2, by=1), correct=F)
                            stat <- c(stat,kaik$stat)
                            jiyu <- c(jiyu,kaik$para)
                            pchi <- c(pchi,kaik$p.va)
                            v <- c(v, sqrt(kaik$stat/(N*(M-1))))
                            ci.values <- V.95CI(matrix(ds, nr=2, by=1))
                            lower <- ci.values[1]
                            upper <- ci.values[2]
                            v.lower <- c(v.lower, lower)
                            v.upper <- c(v.upper, upper)
                        } }
                    padj <- c()
                    padj <- p.adjust(pchi, "bonferroni")
                    
                    kochi<-c(); aite<-c()
                    for(i in 1:(levI-1))
                    {
                        for(j in (i+1):levI)
                        {
                            kochi <- c(kochi, rownames(x)[i])
                            aite  <- c(aite,  rownames(x)[j])
                        }
                    }
                    
                    a <- data.frame(Comparisons=paste(kochi, aite, sep=" vs "), "X^2"=round(stat, 4),
                    df=round(jiyu, 4),
                    p=round(padj, 4),
                    Cramers.V=round(v, 4),
                    lwr.V=round(v.lower, 4),
                    upr.V=round(v.upper, 4))
                    rownames(a) <- cat("\n")
                    
                    print(a)
                    
                } else { # with fisher.p
                    
                    
                    a <- c()
                    stat <- c()
                    jiyu <- c()
                    pchi <- c()
                    fishp <- c()
                    v <- c()
                    v.lower <- c()
                    v.upper <- c()
                    for(i in 1:(levI-1))
                    {
                        for(k in (i+1):levI)
                        {
                            ds <- c()
                            for(j in 1:levJ)
                            {
                                ds <- c(ds, dosu[(i-1)*levJ+j])
                            }
                            for(j in 1:levJ)
                            {
                                ds <- c(ds, dosu[(k-1)*levJ+j])
                            }
                            
                            kaik <- c()
                            kaik <- chisq.test(matrix(ds, nr=2, by=1), correct=F)
                            stat <- c(stat,kaik$stat)
                            jiyu <- c(jiyu,kaik$para)
                            pchi <- c(pchi,kaik$p.va)
                            fishp <- c(fishp,fisher.test(matrix(ds, nr=2, by=1))$p.va)
                            v <- c(v, sqrt(kaik$stat/(N*(M-1))))
                            ci.values <- V.95CI(matrix(ds, nr=2, by=1))
                            lower <- ci.values[1]
                            upper <- ci.values[2]
                            v.lower <- c(v.lower, lower)
                            v.upper <- c(v.upper, upper)
                        } }
                    padj <- c()
                    padj <- p.adjust(pchi, "bonferroni")
                    fishpadj <- p.adjust(fishp, "bonferroni")
                    
                    kochi<-c(); aite<-c()
                    for(i in 1:(levI-1))
                    {
                        for(j in (i+1):levI)
                        {
                            kochi <- c(kochi, rownames(x)[i])
                            aite  <- c(aite,  rownames(x)[j])
                        }
                    }
                    
                    a <- data.frame(Comparisons=paste(kochi, aite, sep=" vs "), "X^2"=round(stat, 4),
                    df=round(jiyu, 4),
                    p=round(padj, 4),
                    Fisher.p=round(fishpadj, 4),
                    Cramers.V=round(v, 4),
                    lwr.V=round(v.lower, 4),
                    upr.V=round(v.upper, 4))
                    rownames(a) <- cat("\n")
                    
                    print(a)
            }
        }
    })
    
    output$test3.out <- renderPrint({
        test3()
    })
    
    
    
    
    
    makepPlot3 <- function(){
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            
            levI <- nrow(x) # 行の水準数
            levJ <- ncol(x) # 列の水準数
            dosu <- as.vector(t(x))
            
            # 標本比率の計算
            gokei <- c()
            bunbo <- c()
            for(i in 1:levI) # 各群の度数を集計
            {
                ds <- c()
                for(j in 1:levJ)
                {
                    ds <- c(ds, dosu[(i-1)*levJ+j])
                }
                gokei <- c(gokei, sum(ds))
                bunbo <- c(bunbo, rep(sum(ds), levJ))
            }
            hyohir <- dosu/bunbo # 群別の各値の比率
            
            zuhir <- c()
            for(i in levI:1) # 群ｉ→群１と逆順に並べ替える
            {
                for(j in 1:levJ)
                {
                    zuhir <- c(zuhir, hyohir[(i-1)*levJ+j] )
                }
            }
            
            zubar <- matrix(c(zuhir), nc=levJ, by=1)
            rownames(zubar) <- rev(rownames(x))
            colnames(zubar) <- colnames(x)
            #zubar <- zubar[nrow(zubar):1,]
            
            # プロット
            par(mar=c(5,6,2,4))
            barplot(t(zubar), hor=1, las=1, xlab="Percentage", col=gray.colors(ncol(x)))
            legend("bottomright", legend=colnames(zubar), fill=gray.colors(ncol(x)))
    }
    
    output$pPlot3 <- renderPlot({
        print(makepPlot3())
    })
    
    
    
    
    
   makemPlot3 <- function(){
       
       dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
       
           x <- table(dat)
           mosaic(x, gp = shading_max, main="Mosaic plot")
       
    }
    
    output$mPlot3 <- renderPlot({
        print(makemPlot3())
    })
    
    



    info3 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info3.out <- renderPrint({
        info3()
    })










#----------------------------------------------------
# 4. Test of Independence (Tabulated data)
#----------------------------------------------------

    data4 <- reactive({
        
        dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
        
            x <- as.matrix(dat)
            x <- addmargins(x)
        
            print(x)
        })
    
        output$data4.out <- renderPrint({
            data4()
        })
    
    
    
    
    
    test4 <- reactive({
        
        dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
        
            x <- as.matrix(dat)
            
            
            a <- chisq.test(x, correct=F)           # Pearson's Chi-squared
            b <- chisq.test(x)                      # Yates
            c <- assocstats(x)                      # Likelihood Ratio
            
            aa <- data.frame(a[4], a[1], a[2], a[3])
            aa[1] <- c("Pearson's Chi-squared")
            row.names(aa) <- NULL
            
            bb <- data.frame(b[4], b[1], b[2], b[3])
            bb[1] <- c("Yates' Continuity Correction")
            row.names(bb) <- NULL
            
            cc <- data.frame(c[2])[1,]
            cc <- data.frame(c("Likelihood Ratio"), cc[,1], cc[,2], cc[,3])
            names(cc) <- c("method", "statistic", "parameter", "p.value")
            
            
            # Fisher's Exact Test
            e <- try(fisher.test(x, workspace=3000000), silent = FALSE)   # try, fisher.test
            if (class(e) == "try-error") {
                dd <- data.frame(c(""), c(""), c(""), c("data too large"))  # if error
                dd[1] <- c("Fisher's Exact Test")
                names(dd) <- c("method", "statistic", "parameter", "p.value")
                row.names(dd) <- NULL
            } else {
                d <- fisher.test(x, workspace=3000000)    # no error
                if (nrow(x) * ncol(x) == 4) { # Only for the 2×2 table
                    dd <- data.frame(d[6], c(""), c(""), d[1])
                } else {
                    dd <- data.frame(d[3], c(""), c(""), d[1])
                }
                dd[1] <- c("Fisher's Exact Test")
                names(dd) <- c("method", "statistic", "parameter", "p.value")
                row.names(dd) <- NULL
            }
            
            
            res <- rbind(aa, bb, cc, dd)
            res[2] <- round(as.numeric(res[,2]), 4)
            res[2][4,] <- ""
            names(res) <- c("Test", "X-squared", "df", "p-value")
            cat("\n")
            print(res)
            
            
            
            
            cat("\n")
            cat("\n", "------------------------------------------------------", "\n",
            "Effect size:", "\n")

            # Cramer's V [95%CI]
            V <- sqrt(chisq.test(x, correct=F)$statistic[[1]]/(sum(x)*(min(nrow(x),ncol(x)-1))))
            fisherZ <- 0.5 * log((1 + V)/(1 - V))
            fisherZ.se <- 1/sqrt(sum(x) - 3) * qnorm(1 - ((1 - 0.95)/2))
            fisherZ.ci <- fisherZ + c(-fisherZ.se, fisherZ.se)
            ci.V <- (exp(2 * fisherZ.ci) - 1)/(1 + exp(2 * fisherZ.ci))
            
            cat("\n", "Cramer's V [95%CI] =", V, "[", ci.V[1], ",",ci.V[2],"]", "\n")
            
            
            # Odds Ratio
            if (nrow(x) * ncol(x) == 4) { # Print odds ratio only for the 2×2 table
               
               oddsrt <- (x[1,1]/x[1,2]) / (x[2,1]/x[2,2])
               oddsrt.log <- log((x[1,1]/x[1,2]) / (x[2,1]/x[2,2]))
               oddsrt.log.var <- 1/x[1,1] + 1/x[1,2] + 1/x[2,1] + 1/x[2,2]
               ci.lwrupr <- exp(oddsrt.log + qnorm(c(0.025,0.975)) * sqrt(oddsrt.log.var))
               cat("\n", "Odds Ratio [95%CI] =", oddsrt, "[", ci.lwrupr[1], ",",ci.lwrupr[2],"]", "\n")
               
            } else {
                NULL
            }
            
            
            
            cat("\n", "\n", "------------------------------------------------------", "\n", "Residual analysis:", "\n")
            res <- chisq.test(x)  # 検定結果を代入
            
            cat("\n", "[Expected values]", "\n")
            print(res$expected) # 期待値
            
            cat("\n", "[Standardized residuals]", "\n")
            print(res$residuals) # 標準化残差
            
            cat("\n", "[Adjusted standardized residuals]", "\n")
            print(res$residuals/sqrt(outer(1-rowSums(x)/sum(x), 1-colSums(x)/sum(x)))) # 調整済み標準化残差
            
            cat("\n", "[p-values of adjusted standardized residuals (two-tailed)]", "\n")
            print(round(2*(1-pnorm(abs(res$residuals/sqrt(outer(1-rowSums(x)/sum(x), 1-colSums(x)/sum(x)))))),3)) # 残差の調整後有意確率（両側確率）
            
            
            
            # 多重比較
            if (nrow(x) < 3) { # Not showing if the number of row is 2
                
                cat("\n")
                
            } else {
                
            cat("\n", "\n", "------------------------------------------------------", "\n", "Multiple comparisons (p-value adjusted with Bonferroni method):", "\n", "\n")
            
            levI <- nrow(x)
            levJ <- ncol(x)
            N <- sum(x)
            dosu <- as.vector(t(x))
            M <- min(c(levI, levJ))
            
            V.95CI <- function(x) {
                V <- sqrt(chisq.test(x, correct=F)$statistic[[1]]/(sum(x)*(min(nrow(x),ncol(x)-1))))
                fisherZ <- 0.5 * log((1 + V)/(1 - V))
                fisherZ.se <- 1/sqrt(sum(x) - 3) * qnorm(1 - ((1 - 0.95)/2))
                fisherZ.ci <- fisherZ + c(-fisherZ.se, fisherZ.se)
                ci.V <- (exp(2 * fisherZ.ci) - 1)/(1 + exp(2 * fisherZ.ci))
                return(ci.V)
            }
            
            # エラーメッセージが出る場合の例外処理
            e <- try(fisher.test(x, workspace=3000000), silent = FALSE)   # try でfisher.test関数を実行
            if (class(e) == "try-error") { # if error, without fisher.p
                
                a <- c()
                stat <- c()
                jiyu <- c()
                pchi <- c()
                v <- c()
                v.lower <- c()
                v.upper <- c()
                for(i in 1:(levI-1))
                {
                    for(k in (i+1):levI)
                    {
                        ds <- c()
                        for(j in 1:levJ)
                        {
                            ds <- c(ds, dosu[(i-1)*levJ+j])
                        }
                        for(j in 1:levJ)
                        {
                            ds <- c(ds, dosu[(k-1)*levJ+j])
                        }
                        
                        kaik <- c()
                        kaik <- chisq.test(matrix(ds, nr=2, by=1), correct=F)
                        stat <- c(stat,kaik$stat)
                        jiyu <- c(jiyu,kaik$para)
                        pchi <- c(pchi,kaik$p.va)
                        v <- c(v, sqrt(kaik$stat/(N*(M-1))))
                        ci.values <- V.95CI(matrix(ds, nr=2, by=1))
                        lower <- ci.values[1]
                        upper <- ci.values[2]
                        v.lower <- c(v.lower, lower)
                        v.upper <- c(v.upper, upper)
                    } }
                padj <- c()
                padj <- p.adjust(pchi, "bonferroni")
                
                kochi<-c(); aite<-c()
                for(i in 1:(levI-1))
                {
                    for(j in (i+1):levI)
                    {
                        kochi <- c(kochi, rownames(x)[i])
                        aite  <- c(aite,  rownames(x)[j])
                    }
                }
                
                a <- data.frame(Comparisons=paste(kochi, aite, sep=" vs "), "X^2"=round(stat, 4),
                df=round(jiyu, 4),
                p=round(padj, 4),
                Cramers.V=round(v, 4),
                lwr.V=round(v.lower, 4),
                upr.V=round(v.upper, 4))
                rownames(a) <- cat("\n")
                
                print(a)
                
            } else { # with fisher.p
                
                
                a <- c()
                stat <- c()
                jiyu <- c()
                pchi <- c()
                fishp <- c()
                v <- c()
                v.lower <- c()
                v.upper <- c()
                for(i in 1:(levI-1))
                {
                    for(k in (i+1):levI)
                    {
                        ds <- c()
                        for(j in 1:levJ)
                        {
                            ds <- c(ds, dosu[(i-1)*levJ+j])
                        }
                        for(j in 1:levJ)
                        {
                            ds <- c(ds, dosu[(k-1)*levJ+j])
                        }
                        
                        kaik <- c()
                        kaik <- chisq.test(matrix(ds, nr=2, by=1), correct=F)
                        stat <- c(stat,kaik$stat)
                        jiyu <- c(jiyu,kaik$para)
                        pchi <- c(pchi,kaik$p.va)
                        fishp <- c(fishp,fisher.test(matrix(ds, nr=2, by=1))$p.va)
                        v <- c(v, sqrt(kaik$stat/(N*(M-1))))
                        ci.values <- V.95CI(matrix(ds, nr=2, by=1))
                        lower <- ci.values[1]
                        upper <- ci.values[2]
                        v.lower <- c(v.lower, lower)
                        v.upper <- c(v.upper, upper)
                    } }
                padj <- c()
                padj <- p.adjust(pchi, "bonferroni")
                fishpadj <- p.adjust(fishp, "bonferroni")
                
                kochi<-c(); aite<-c()
                for(i in 1:(levI-1))
                {
                    for(j in (i+1):levI)
                    {
                        kochi <- c(kochi, rownames(x)[i])
                        aite  <- c(aite,  rownames(x)[j])
                    }
                }
                
                a <- data.frame(Comparisons=paste(kochi, aite, sep=" vs "), "X^2"=round(stat, 4),
                df=round(jiyu, 4),
                p=round(padj, 4),
                Fisher.p=round(fishpadj, 4),
                Cramers.V=round(v, 4),
                lwr.V=round(v.lower, 4),
                upr.V=round(v.upper, 4))
                rownames(a) <- cat("\n")
                
                print(a)
            }
            }
    })
    
    output$test4.out <- renderPrint({
        test4()
    })
    
    
    
    
    
    makepPlot4 <- function(){
        
        dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
        
           x <- as.matrix(dat)
            
            levI <- nrow(x) # 行の水準数
            levJ <- ncol(x) # 列の水準数
            dosu <- as.vector(t(x))
            
            # 標本比率の計算
            gokei <- c()
            bunbo <- c()
            for(i in 1:levI) # 各群の度数を集計
            {
                ds <- c()
                for(j in 1:levJ)
                {
                    ds <- c(ds, dosu[(i-1)*levJ+j])
                }
                gokei <- c(gokei, sum(ds))
                bunbo <- c(bunbo, rep(sum(ds), levJ))
            }
            hyohir <- dosu/bunbo # 群別の各値の比率
            
            zuhir <- c()
            for(i in levI:1) # 群ｉ→群１と逆順に並べ替える
            {
                for(j in 1:levJ)
                {
                    zuhir <- c(zuhir, hyohir[(i-1)*levJ+j] )
                }
            }
            
            zubar <- matrix(c(zuhir), nc=levJ, by=1)
            rownames(zubar) <- rev(rownames(x))
            colnames(zubar) <- colnames(x)
            #zubar <- zubar[nrow(zubar):1,]
            
            # プロット
            par(mar=c(5,6,2,4))
            barplot(t(zubar), hor=1, las=1, xlab="Percentage", col=gray.colors(ncol(x)))
            legend("bottomright", legend=colnames(zubar), fill=gray.colors(ncol(x)))
    
    }
    
    output$pPlot4 <- renderPlot({
        print(makepPlot4())
    })
    
    
    
    
    
    makemPlot4 <- function(){
       
       dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
       
            x <- as.matrix(dat)
            mosaic(x, gp = shading_max, main="Mosaic plot")
       
    }
    
    output$mPlot4 <- renderPlot({
        print(makemPlot4())
    })
    
    



    info4 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info4.out <- renderPrint({
        info4()
    })






})
