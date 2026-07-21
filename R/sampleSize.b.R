sampleSizeClass <- R6::R6Class("sampleSizeClass",
                          inherit=sampleSizeBase,
    private = list(

        .run = function() {
          
          tipo  <- self$options$tipo
          #e     <- self$options$e
          #level <- self$options$level
          #alpha <- 1-level
          #z     <- qnorm(1-alpha/2)
          
          if (tipo == "media") {
            
            S <- self$options$S
            e_OM <- self$options$e_OM

            level_OM <- self$options$level_OM
            alpha <- 1-level_OM
            z <- qnorm(1-alpha/2)
            
            n <- ceiling((z*S/e_OM)^2)
            
            #One-sample mean Normal approximation
            #self$results$text$setContent(paste("Required sample size:", n))
            
            html <- paste0(
              "<h3>One-Sample Mean</h3>
              <tr><td colspan='2'>&nbsp;</td></tr>     

              <table style='border-collapse:collapse;'>

              <tr>
                <td style='padding-right:30px;'><b>Estimated sample size</b></td>
                <td>", n, "</td>
              </tr>
              
              <tr><td colspan='2'>&nbsp;</td></tr>

              <tr>
                <td><b>Precision</b></td>
                <td>", e_OM, "</td>
              </tr>

              <tr>
                <td><b>Standard deviation</b></td>
                <td>", S, "</td>
              </tr>

              <tr>
                <td><b>Confidence level</b></td>
                <td>", round(level_OM*100, 1), "%</td>
              </tr>
              
              <tr><td colspan='2'>&nbsp;</td></tr>              

              </table>"
            )

            self$results$text$setContent(html)       

          }
          
          else if (tipo == "proporcion") {
            
            P <- self$options$P            
            e_OP <- self$options$e_OP

            level_OP <- self$options$level_OP
            alpha <- 1-level_OP
            z <- qnorm(1-alpha/2)

            n <- ceiling(z^2*P*(1-P)/e_OP^2)
            
            #One-sample proportion Normal approximation           
            #self$results$text$setContent(paste("Required sample size:", n))

            html <- paste0(
              "<h3>One-Sample Proportion</h3>
              <tr><td colspan='2'>&nbsp;</td></tr>     

              <table style='border-collapse:collapse;'>

              <tr>
                <td style='padding-right:30px;'><b>Estimated sample size</b></td>
                <td>", n, "</td>
              </tr>
              
              <tr><td colspan='2'>&nbsp;</td></tr>
          
              <tr>
                <td><b>Precision</b></td>
                <td>", e_OP, "</td>
              </tr>

              <tr>
                <td><b>Expected proportion</b></td>
                <td>", P, "</td>
              </tr>

              <tr>
                <td><b>Confidence level</b></td>
                <td>", round(level_OP*100, 1), "%</td>
              </tr>
              
              <tr><td colspan='2'>&nbsp;</td></tr>              

              </table>"
            )

            self$results$text$setContent(html)

          }
          
          else if (tipo == "dosMediasVD") {
            
            #Dos medias independientes
            S1    <- self$options$S1
            S2    <- self$options$S2
            delta <- self$options$dif_IM
            power <- self$options$pow_IM
            
            zb <- qnorm(power)

            level_IM <- self$options$level_IM
            alpha <- 1-level_IM
            z <- qnorm(1-alpha/2)            
            
            n1 <- ceiling(((z + zb)^2 * (S1^2 + S2^2)) / delta^2)
            n2 <- n1
            
            #Two independent means Welch approximation (o Equal variance assumption, según la fórmula definitiva)
            #self$results$text$setContent(paste("Group 1:", n1,"\nGroup 2:", n2,"\nTotal:", n1+n2,"\nWelch"))
            
            html <- paste0(
              "<h3>Two Independent Means (different variances)</h3>
              <tr><td colspan='2'>&nbsp;</td></tr>     

              <table style='border-collapse:collapse;'>

              <tr>
                <td style='padding-right:30px;'><b>Estimated sample size</b></td>
                <td>", n1+n2, "</td>
              </tr>
              
              <tr>
                <td style='padding-right:30px;'><b>Estimated group 1 size</b></td>
                <td>", n1, "</td>
              </tr>

              <tr>
                <td style='padding-right:30px;'><b>Estimated group 2 size</b></td>
                <td>", n2, "</td>
              </tr>              

              <tr><td colspan='2'>&nbsp;</td></tr>

              <tr>
                <td><b>Statistical power</b></td>
                <td></td>
                <td>", power, "</td>
              </tr>

              <tr>
                <td><b>Minimum detectable difference</b></td>
                <td></td>
                <td>", delta, "</td>
              </tr>

              <tr>
                <td><b>Group 1 standard deviation</b></td>
                <td></td>
                <td>", S1, "</td>
              </tr>

              <tr>
                <td><b>Group 2 standard deviation</b></td>
                <td></td>
                <td>", S2, "</td>
              </tr>

              <tr>
                <td><b>Confidence level</b></td>
                <td></td>
                <td>", round(level_IM*100, 1), "%</td>
              </tr>

              <tr><td colspan='2'>&nbsp;</td></tr>              

              </table>

              <b>Note.</b> Sample size was estimated using the Welch approximation<br>"
            )

            self$results$text$setContent(html)

            }
          
          else if (tipo == "dosMediasVC") {
            
            #Dos medias emparejadas
            SC    <- self$options$SC
            delta <- self$options$dif_PM
            power <- self$options$pow_PM
            
            zb <- qnorm(power)

            level_PM <- self$options$level_PM
            alpha <- 1-level_PM
            z <- qnorm(1-alpha/2)            
            
            n1 <- ceiling(2*(SC^2)*(z+zb)^2/delta^2)
            n2 <- n1
            
            #Two paired means Paired t-test approximation
            #self$results$text$setContent(paste("Group 1:", n1,"\nGroup 2:", n2,"\nTotal:", n1+n2,"\nPooled method"))
          
            html <- paste0(
              "<h3>Two Independent Means (common variances)</h3>
              <tr><td colspan='2'>&nbsp;</td></tr>     

              <table style='border-collapse:collapse;'>

              <tr>
                <td style='padding-right:30px;'><b>Estimated sample size</b></td>
                <td>", n1+n2, "</td>
              </tr>
              
              <tr>
                <td style='padding-right:30px;'><b>Estimated group 1 size</b></td>
                <td>", n1, "</td>
              </tr>

              <tr>
                <td style='padding-right:30px;'><b>Estimated group 2 size</b></td>
                <td>", n2, "</td>
              </tr>              

              <tr><td colspan='2'>&nbsp;</td></tr>

              <tr>
                <td><b>Statistical power</b></td>
                <td></td>
                <td>", power, "</td>
              </tr>

              <tr>
                <td><b>Minimum detectable difference</b></td>
                <td></td>
                <td>", delta, "</td>
              </tr>

              <tr>
                <td><b>Common Standard deviation</b></td>
                <td></td>
                <td>", SC, "</td>
              </tr>

              <tr>
                <td><b>Confidence level</b></td>
                <td></td>
                <td>", round(level_PM*100, 1), "%</td>
              </tr>

              <tr><td colspan='2'>&nbsp;</td></tr>              

              </table>

              <b>Note.</b> Sample size was estimated using the Pooled method<br>"
            )

            self$results$text$setContent(html)
          }
          
          else if (tipo == "dosProporciones") {
            
            #Dos proporciones
            P1 <- self$options$P1
            P2 <- self$options$P2
            
            delta <- self$options$dif_IP
            power <- self$options$pow_IP
            
            zb <- qnorm(power)

            level_IP <- self$options$level_IP
            alpha <- 1-level_IP
            z <- qnorm(1-alpha/2)            
            
            pbar <- (P1+P2)/2
            
            n1 <- ceiling((
              z*sqrt(2*pbar*(1-pbar)) +
                zb*sqrt(P1*(1-P1)+P2*(1-P2))
            )^2/(P1-P2)^2)
            n2 <- n1
            
            #Según Chatgpt es Fleiss method (without continuity correction) - REVISAR
            #self$results$text$setContent(paste("Group 1:", n1,"\nGroup 2:", n2,"\nTotal:", n1+n2,"\nChi-square without continuity correction"))
            
            html <- paste0(
              "<h3>Two Independent Proportions</h3>
              <tr><td colspan='2'>&nbsp;</td></tr>     

              <table style='border-collapse:collapse;'>

              <tr>
                <td style='padding-right:30px;'><b>Estimated sample size</b></td>
                <td>", n1+n2, "</td>
              </tr>
              
              <tr>
                <td style='padding-right:30px;'><b>Estimated group 1 size</b></td>
                <td>", n1, "</td>
              </tr>

              <tr>
                <td style='padding-right:30px;'><b>Estimated group 2 size</b></td>
                <td>", n2, "</td>
              </tr>              

              <tr><td colspan='2'>&nbsp;</td></tr>

              <tr>
                <td><b>Statistical power</b></td>
                <td></td>                
                <td>", power, "</td>
              </tr>

              <tr>
                <td><b>Minimum detectable difference</b></td>
                <td></td>
                <td>", delta, "</td>
              </tr>

              <tr>
                <td><b>Expected proportion in group 1</b></td>
                <td></td>
                <td>", P1, "</td>
              </tr>

              <tr>
                <td><b>Expected proportion in group 2</b></td>
                <td></td>
                <td>", P2, "</td>
              </tr>

              <tr>
                <td><b>Confidence level</b></td>
                <td></td>
                <td>", round(level_IP*100, 1), "%</td>
              </tr>

              <tr><td colspan='2'>&nbsp;</td></tr>              

              </table>

              <b>Note.</b> Sample size was estimated using the Chi-square method (without continuity correction).<br>"
            )

            self$results$text$setContent(html)
          }
          
        }
          )
   )