library(shiny)
library(shinyAce)
library(pwr)
library(vcd)



shinyServer(function(input, output) {
    
    options(warn=-1)
    
    
    
    
    data <- reactive({
        
        
        data <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "goodraw") {

            x <- table(data)
            n <- sum(x)
            x <- c(x, Sum=n)
            
        }
        
        else if (input$type == "goodtab") {
            
            x <- data
            n <- sum(x)
            x <- data.frame(x, Sum=n)
            rownames(x) <- ""
            
        }
        
        else if (input$type == "indraw") {
            
            x <- table(data)
            x <- addmargins(x)
            
        }
        
        else if (input$type == "indtab") {
            
            x <- as.matrix(data)
            x <- addmargins(x)
            
        }

        print(x)

    })
    
    
    
    
    
    test <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "goodraw") {
            
            x <- table(dat)
            
            chi <- chisq.test(x)
            
            print(chi)
            
            
            P0 <- rep(1/length(x), times = length(x))  # 期待比率
            P1 <- x/sum(x)               # 標本比率
            w <- ES.w1(P0, P1)      # 効果量ｗを求める関数 大=0.5, 中=0.3, 小=0.1
            
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
        }
        
        
        else if (input$type == "goodtab") {
            
            x <- dat
            
            chi <- chisq.test(x)
            
            print(chi)
            
            
            P0 <- rep(1/length(x), times = length(x))  # 期待比率
            P1 <- x/sum(x)               # 標本比率
            w <- ES.w1(P0, P1)      # 効果量ｗを求める関数 大=0.5, 中=0.3, 小=0.1
            
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
        }
        
        
        else if (input$type == "indraw") {
            
            x <- table(dat)
            
            print(chisq.test(x, correct=F)) #イエーツの補正なし
            print(chisq.test(x)) #イエーツの補正あり
            print(fisher.test(x, workspace=3000000)) #フィッシャーの正確確率検定
            
            cat("\n", "---", "\n", "Effect size indices:", "\n", "\n")
            print(assocstats(x))
            
            cat("\n", "\n", "---", "\n", "Residual analysis:", "\n")
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
            cat("\n", "\n", "---", "\n", "Multiple comparisons (p-value adjusted with Bonferroni method):", "\n", "\n")
            
            levI <- nrow(x) # 行の水準数
            levJ <- ncol(x) # 列の水準数
            N <- sum(x)
            dosu <- as.vector(t(x))
            M <- min(c(levI, levJ))
            
            a <- c()
            stat <- c()
            jiyu <- c()
            pchi <- c()
            fishp <- c()
            v <- c()
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
                    kaik <- chisq.test(matrix(ds, nr=2, by=1), correct=F) # イェーツの補正なし
                    stat <- c(stat,kaik$stat)
                    jiyu <- c(jiyu,kaik$para)
                    pchi <- c(pchi,kaik$p.va)
                    fishp <- c(fishp,fisher.test(matrix(ds, nr=2, by=1))$p.va)
                    v <- c(v, sqrt(kaik$stat/(N*(M-1))))
                } }
            padj <- c()
            padj <- p.adjust(pchi, "bonferroni")
            fishpadj <- p.adjust(fishp, "bonferroni")
            
            kochi<-c(); aite<-c()
            for(i in 1:(levI-1))
            {
                for(j in (i+1):levI)
                {
                    kochi <- c(kochi, paste("Row", i, sep=""))
                    aite  <- c(aite,  paste("Row", j, sep=""))
                }
            }
            
            a <- data.frame(Comparisons=paste(kochi, aite, sep=" vs "), "X^2"=round(stat, 4), 
                            df=round(jiyu, 4),
                            p=round(padj, 4),
                            Fisher.p=round(fishpadj, 4),
                            CramerV=round(v, 4))
            rownames(a) <- cat("\n")
            
            print(a)
        }
        
        
        else if (input$type == "indtab") {
            
            x <- as.matrix(dat)
            
            print(chisq.test(x, correct=F)) #イエーツの補正なし
            print(chisq.test(x)) #イエーツの補正あり
            print(fisher.test(x, workspace=3000000)) #フィッシャーの正確確率検定
            
            cat("\n", "---", "\n", "Effect size indices:", "\n", "\n")
            print(assocstats(x))
            
            cat("\n", "\n", "---", "\n", "Residual analysis:", "\n")
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
            cat("\n", "\n", "---", "\n", "Multiple comparisons (p-value adjusted with Bonferroni method):", "\n", "\n")
            
            levI <- nrow(x) # 行の水準数
            levJ <- ncol(x) # 列の水準数
            N <- sum(x)
            dosu <- as.vector(t(x))
            M <- min(c(levI, levJ))
            
            a <- c()
            stat <- c()
            jiyu <- c()
            pchi <- c()
            fishp <- c()
            v <- c()
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
                    kaik <- chisq.test(matrix(ds, nr=2, by=1), correct=F) # イェーツの補正なし
                    stat <- c(stat,kaik$stat)
                    jiyu <- c(jiyu,kaik$para)
                    pchi <- c(pchi,kaik$p.va)
                    fishp <- c(fishp,fisher.test(matrix(ds, nr=2, by=1))$p.va)
                    v <- c(v, sqrt(kaik$stat/(N*(M-1))))
                } }
            padj <- c()
            padj <- p.adjust(pchi, "bonferroni")
            fishpadj <- p.adjust(fishp, "bonferroni")
            
            kochi<-c(); aite<-c()
            for(i in 1:(levI-1))
            {
                for(j in (i+1):levI)
                {
                    kochi <- c(kochi, paste("Row", i, sep=""))
                    aite  <- c(aite,  paste("Row", j, sep=""))
                }
            }
            
            a <- data.frame(Comparisons=paste(kochi, aite, sep=" vs "), "X^2"=round(stat, 4),
            df=round(jiyu, 4),
            p=round(padj, 4),
            Fisher.p=round(fishpadj, 4),
            CramerV=round(v, 4))
            rownames(a) <- cat("\n")
            print(a)
            
        }
    })
    
    
    
    
    makepPlot <- function(){
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "goodraw") {
            
            x <- table(dat)
            
            P0 <- rep(1/length(x), times = length(x))  # 期待比率
            P1 <- x/sum(x)               # 標本比率
            
            par(mar=c(5,6,2,4))
            z <- matrix(c(P0, P1), nc=length(x), by=1)
            colnames(z) <- names(x)
            rownames(z) <- c("Expected", "Observed")
            barplot(t(z), hor=1, las=1, xlab="Percentage", col=gray.colors(length(x)))
            
            legend("bottomright",legend=colnames(z), fill=gray.colors(length(x)))
            
        }
        
        
        else if (input$type == "goodtab") {
            
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
        
        
        else if (input$type == "indraw") {
            
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
        
        
        else if (input$type == "indtab") {
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
    }
    
    output$pPlot <- renderPlot({
        print(makepPlot())
    })
    
    
    
    makemPlot <- function(){
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$type == "indraw") {
            
            x <- table(dat)
            mosaic(x, gp = shading_max, main="Mosaic plot")
        }
        
        
        else if (input$type == "indtab") {
            x <- as.matrix(dat)
            mosaic(x, gp = shading_max, main="Mosaic plot")
            
        }
    }
    
    output$mPlot <- renderPlot({
        print(makemPlot())
    })
    
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })





    output$data.out <- renderPrint({
        data()
    })
    
    output$test.out <- renderPrint({
        test()
    })
    
    output$downloadpPlot <- downloadHandler( # 名前変更
    filename = function() {
        paste('Percentage-', Sys.Date(), '.pdf', sep='') # 名前変更
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makepPlot()) # 名前変更
		dev.off()
	})
    
    output$downloadmPlot <- downloadHandler( # 名前変更
    filename = function() {
        paste('Mosaicplot-', Sys.Date(), '.pdf', sep='') # 名前変更
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makemPlot()) # 名前変更
		dev.off()
	})

})
