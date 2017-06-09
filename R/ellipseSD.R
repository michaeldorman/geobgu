ellipseSD = function(id = 1, centre.xy = NULL, 
    calccentre = TRUE, weighted = FALSE, weights = NULL, points = activities) 
{
    errorcode <- 1000
    if (length(dim(points)) != 2) {
        errorcode <- 61
        cat("\n\nWARNING: Provided points input matrix has fewer than 2 columns.")
        cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
        return("ERROR")
    }
    if (dim(points)[2] != 2) {
        errorcode <- 60
        cat("\n\nWARNING: Provided points input matrix has too many columns, only 2 are allowed.")
        cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
        return("ERROR")
    }
    else {
        n <- dim(points)[1]
        if (calccentre) {
            if (length(centre.xy) == 2) {
                errorcode <- 21
                cat("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
                cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
                return("ERROR")
            }
            else {
                if (weighted) {
                  wt.x <- points[, 1] * weights
                  wt.y <- points[, 2] * weights
                  WMC.x <- c(sum(wt.x)/sum(weights))
                  WMC.y <- c(sum(wt.y)/sum(weights))
                  centre.xy[1] <- WMC.x
                  centre.xy[2] <- WMC.y
                }
                else {
                  meanx <- sum(points[, 1])/n
                  meany <- sum(points[, 2])/n
                  centre.xy[1] <- meanx
                  centre.xy[2] <- meany
                }
            }
        }
    }
    points <- cbind(points, points[, 1]^2, points[, 2]^2)
    points <- cbind(points, points[, 1] - centre.xy[1], points[, 
        2] - centre.xy[2])
    points <- cbind(points, points[, 5]^2, points[, 6]^2, points[, 
        5] * points[, 6])
    names(points) <- c("x", "y", "x2", "y2", "x'", "y'", "x'2", 
        "y'2", "x'y'")
    if (weighted) {
        top1 <- sum(weights * points[, 7]) - sum(weights * points[, 
            8])
        top2 <- sqrt((sum(weights * points[, 7]) - sum(weights * 
            points[, 8]))^2 + 4 * (sum(weights * points[, 9]))^2)
        bottom <- (2 * sum(weights * points[, 9]))
        tantheta <- (top1 + top2)/bottom
    }
    else {
        top1 <- sum(points[, 7]) - sum(points[, 8])
        top2 <- sqrt((sum(points[, 7]) - sum(points[, 8]))^2 + 
            4 * (sum(points[, 9]))^2)
        bottom <- (2 * sum(points[, 9]))
        tantheta <- (top1 + top2)/bottom
    }
    if (tantheta < 0) {
        theta <- 180 + (atan_d(tantheta))
    }
    else {
        theta <- atan_d(tantheta)
    }
    sintheta <- sin_d(theta)
    costheta <- cos_d(theta)
    sin2theta <- sintheta^2
    cos2theta <- costheta^2
    sinthetacostheta <- sintheta * costheta
    if (weighted) {
        sigmax <- sqrt(2) * sqrt(((sum(weights * points[, 7])) * 
            (cos2theta) - 2 * (sum(weights * points[, 9])) * 
            (sinthetacostheta) + (sum(weights * points[, 8])) * 
            (sin2theta))/((sum(weights)) - 2))
        sigmay <- sqrt(2) * sqrt(((sum(weights * points[, 7])) * 
            (sin2theta) + 2 * (sum(weights * points[, 9])) * 
            (sinthetacostheta) + (sum(weights * points[, 8])) * 
            (cos2theta))/((sum(weights)) - 2))
    }
    else {
        sigmax <- sqrt(2) * sqrt(((sum(points[, 7])) * (cos2theta) - 
            2 * (sum(points[, 9])) * (sinthetacostheta) + (sum(points[, 
            8])) * (sin2theta))/(n - 2))
        sigmay <- sqrt(2) * sqrt(((sum(points[, 7])) * (sin2theta) + 
            2 * (sum(points[, 9])) * (sinthetacostheta) + (sum(points[, 
            8])) * (cos2theta))/(n - 2))
    }
    if (sigmax > sigmay) {
        Major <- "SigmaX"
        Minor <- "SigmaY"
    }
    else {
        Major <- "SigmaY"
        Minor <- "SigmaX"
    }
    lengthsigmax <- 2 * sigmax
    lengthsigmay <- 2 * sigmay
    areaSDE <- pi * sigmax * sigmay
    eccentricity <- sqrt(1 - ((min(sigmax, sigmay)^2)/(max(sigmax, 
        sigmay)^2)))
    B <- min(sigmax, sigmay)
    A <- max(sigmax, sigmay)
    d2 <- (A - B) * (A + B)
    phi <- 2 * pi * seq(0, 1, len = 360)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- sigmax * sigmay/sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    al <- (90 - theta) * pi/180
    ca <- cos(al)
    sa <- sin(al)
    coordsSDE <- xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(centre.xy[1], 
        360), rep(centre.xy[2], 360))
   
    if (sigmax < sigmay) {
        Theta.Corr <- theta
    }
    else {
        Theta.Corr <- theta + 90
    }
    r.SDE <- list(id = id, points = points, coordsSDE = coordsSDE, 
        calccentre = calccentre, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
        Major = Major, Minor = Minor, theta = theta, Sigma.x = sigmax, 
        Sigma.y = sigmay, Eccentricity = eccentricity, Area.sde = areaSDE, 
        TanTheta = tantheta, SinTheta = sintheta, CosTheta = costheta, 
        SinThetaCosTheta = sinthetacostheta, Sin2Theta = sin2theta, 
        Cos2Theta = cos2theta, ThetaCorr = Theta.Corr, weighted = weighted, 
        weights = weights)
    assign("r.SDE", r.SDE, pos = 1)
    result.sde <- list(id = id, CALCCENTRE = calccentre, weighted = weighted, 
        CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], Sigma.x = sigmax, 
        Sigma.y = sigmay, Major = Major, Minor = Minor, Theta = theta, 
        Eccentricity = eccentricity, Area.sde = areaSDE, TanTheta = tantheta, 
        SinTheta = sintheta, CosTheta = costheta, SinThetaCosTheta = sinthetacostheta, 
        Sin2Theta = sin2theta, Cos2Theta = cos2theta, ThetaCorr = Theta.Corr)
    print(result.sde)
    result.sde <- as.data.frame(result.sde)
    assign("sdeatt", result.sde, pos = 1)
    sdeloc <- as.data.frame(cbind(id, coordsSDE))
    colnames(sdeloc) = c("id", "x", "y")
    assign("sdeloc", sdeloc, pos = 1)
}
