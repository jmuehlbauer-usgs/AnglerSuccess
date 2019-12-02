##### Bug Flows Weekday vs. Weekend Angler Success #####
	## Based on code September 2019 created by A.N. Metcalfe and J.D. Muehlbauer. 
	## Code last modified 2 December 2019 by J.D. Muehlbauer.


##### Read in data #####

## Dataframe with angler success (fish catch per angler, per day)
dat0 <- read.csv('C:/Users/jmuehlbauer/Desktop/AnglerSuccess.csv')
	## Note: Dataframe contains only "true" anglers. Removes the lavage crew that fished only sparingly, etc.


##### Convert data for paired weekday-weekend analysis #####

## Get a matrix of angler success by day
dat1 <- as.data.frame(with(dat0, tapply(Fish,list(Angler,Date), FUN = function(x) sum(na.omit(x)))))
wd1 <- c(dat1[, 1], dat1[, 4], dat1[, 5], dat1[, 8])
we1 <- c(dat1[, 2], dat1[, 3], dat1[, 6], dat1[, 7])

## Convert data to paired weekday-weekend differences
dat2 <- data.frame(Angler = rep(rownames(dat1, 4)), Weekend = we1, Weekday = wd1, Diff = we1 - wd1)
	dat2$Month <- as.factor(c(rep(6, dim(dat2)[1] / 2), rep(8, dim(dat2)[1] / 2)))
	dat2$Dates <- as.factor(c(rep('Sat-Fri', dim(dat2)[1] / 4), rep('Sun-Mon', dim(dat2)[1] / 4), 
		rep('Sat-Fri', dim(dat2)[1] / 4), rep('Sun-Mon', dim(dat2)[1] / 4)))

## Remove NAs from dataset (non-existent paired weekday-weekend comparisons) 
dat3 <- dat2[!is.na(dat2$Diff),]
	row.names(dat3) <- 1 : dim(dat3)[1]


##### Run some statistics #####

## Get means and standard error (real data)
avgs <- data.frame(Mean = round(sapply(list(dat3$Weekday, dat3$Weekend), mean), 1))
	rownames(avgs) <- c('Weekday', 'Weekend')
	avgs$SE <- round(sapply(list(dat3$Weekday, dat3$Weekend), function(x) sd(x)/sqrt(length(x))), 1)
	## 6.8 +/- 1.0 fish per angler per day on weekends, 5.1 +/- 0.6 on weekdays.

## Run t-test on dataset
out1 <- t.test(dat3$Diff, alternative = 'greater')
diffs <- data.frame(Mean = round(out1$estimate, 1))
	rownames(diffs) <- 'Weekend-Weekday Difference'
	## Weekend catch significantly higher than weekday catch (1.74 more fish per angler per day).

## Get confidence intervals
	## Note that using an alpha of 0.1 because only interested in the one-sided result (0.1 / 2 = 0.05)
	## lm is statistically identical to t test above, except this is two-sided.
out2 <- lm(dat3$Diff ~ 1)
CIs <- confint(out2, level = 0.9)
	diffs$CILower <- round(CIs[1], 1)
	diffs$CIUpper <- round(CIs[2], 1)


##### Plot results #####

## Set plotting parameters
par(mfrow = c(2, 1), mar = c(3, 4, 0.1, 0.1))

## Plot weekday vs. weekend
plot(1:2, c(min(avgs$Mean - avgs$SE), max(avgs$Mean + avgs$SE)), type = 'n', axes = FALSE, xlab = '', ylab = 'Average catch per angler, per day', xlim = c(0.5, 2.5))
	axis(1, at = 1:2, labels = c('Weekday', 'Weekend'))
	axis(2, las = 2)
	box(bty = 'l')
	arrows(1:2, avgs$Mean - avgs$SE, y1 = avgs$Mean + avgs$SE, code = 3, length = 0.1, angle = 90)
	points(1:2, avgs$Mean, col = c(2, 4), pch = 16:17, cex = 2)
	text(1:2 + 0.07, avgs$Mean, avgs$Mean)
	legend('topleft', 'Error bars are standard error', bty = 'n')

## Plot modeled difference
plot(c(1.5, 1.5), c(diffs$CILower, diffs$CIUpper), type = 'n', axes = FALSE, xlab = '', ylab = 'Mean difference in Weekend - weekday catch, per angler', xlim = c(0.5, 2.5))
	axis(1, at = 1.5, labels = c('Weekend - Weekday'))
	axis(2, las = 2)
	box(bty = 'l')
	arrows(1.5, diffs$CILower, y1 = diffs$CIUpper, code = 3, length = 0.1, angle = 90)
	points(1.5, diffs$Mean, col = 3, pch = 18, cex = 2.5)
	text(1.5 + 0.07, diffs$Mean, diffs$Mean)
	legend('topleft', 'Error bars are 95% confidence intervals', bty = 'n')