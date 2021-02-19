library(tidyverse)

# ----------------------------------------
# set up
# ----------------------------------------
# set work directory (modify if necessary)
setwd("/home/jordi/PycharmProjects/chessstats/")

# ----------------------------------------
# read and prepare data
# ----------------------------------------
# read data
fname <- './data/twic_2012_2020.txt'
pdata <- read.table(fname, sep='\t', header=TRUE)

# get GM games, only complete chess games (and no chess960), known result,
# Elo known for both players, from 2012
# GM plays as white
white_gm <- pdata %>% filter(WhiteTitle %in% c('GM'), is.na(FEN),
                             Result %in% c('0-1', '1-0', '1/2-1/2'),
                             !is.na(WhiteElo), !is.na(BlackElo),
                             !is.na(WhiteFideId), ECO != '?', !is.na(ECO)) %>% 
  select(Date, White, Black, Result, WhiteTitle, BlackTitle, WhiteElo, BlackElo,
         WhiteFideId, BlackFideId, Opening, Variation, ECO) %>%
  mutate(EloDif = WhiteElo - BlackElo, Year = as.integer(str_sub(Date, 1, 4))) %>%
  filter(Year>=2012) %>% arrange(Date)
# GM plays with black
black_gm <- pdata %>% filter(BlackTitle %in% c('GM'), is.na(FEN),
                             Result %in% c('0-1', '1-0', '1/2-1/2'),
                             !is.na(WhiteElo), !is.na(BlackElo),
                             !is.na(BlackFideId), ECO != '?', !is.na(ECO)) %>% 
  select(Date, White, Black, Result, WhiteTitle, BlackTitle, WhiteElo, BlackElo,
         WhiteFideId, BlackFideId, Opening, Variation, ECO) %>%
  mutate(EloDif = BlackElo - WhiteElo, Year = as.integer(str_sub(Date, 1, 4))) %>%
  filter(Year>=2012) %>% arrange(Date)

# Summarise result by player with white pieces and black pieces
white_res <- white_gm %>% group_by(WhiteFideId) %>%
  summarise(WhiteN=n(), WhiteWins=sum(Result=='1-0'), WhiteDraws=sum(Result=='1/2-1/2'),
            WhiteWinsAvg=sum(Result=='1-0')/WhiteN,
            WhiteDrawsAvg=sum(Result=='1/2-1/2')/WhiteN) %>%
  mutate(WhitePerformance=(WhiteWins + 0.5*WhiteDraws)/WhiteN)
black_res <- black_gm %>% group_by(BlackFideId) %>%
  summarise(BlackN=n(), BlackWins=sum(Result=='0-1'), BlackDraws=sum(Result=='1/2-1/2'),
            BlackWinsAvg=sum(Result=='0-1')/BlackN,
            BlackDrawsAvg=sum(Result=='1/2-1/2')/BlackN) %>%
  mutate(BlackPerformance=(BlackWins + 0.5*BlackDraws)/BlackN)

# ----------------------------------------
# How many games?
# ----------------------------------------
# calculate average score and std deviation with white and black pieces,
# use that information to calculate necessary sample size 
# to estimate mean with an error of 0.01 for alpha/2 = 0.05
WScoreAvg <- mean(white_res$WhitePerformance)
WScoreSd <- sd(white_res$WhitePerformance)
BScoreAvg <- mean(black_res$BlackPerformance)
BScoreSd <- sd(black_res$BlackPerformance)

z005 <- 1.6449
err <- 0.01
nexp <- (z005*WScoreSd/err)^2
# 239, maybe too many

# alternative approach
# which is the expected error of the mean using 50 games
WErr <- z005 * WScoreSd/sqrt(50)
BErr <- z005 * BScoreSd/sqrt(50)
# an error of about 2.2%, acceptable

# filter results to select players with at least 50 games per color
min_n_games <- 50
white_res <- white_res %>% filter(WhiteN>min_n_games)
black_res <- black_res %>% filter(BlackN>min_n_games)

# ----------------------------------------
# Calculate the difference of scores
# ----------------------------------------
# Group results (merge datasets for white and black pieces)
game_res <- merge(white_res, black_res, by.x='WhiteFideId', by.y='BlackFideId')
game_res <- game_res %>% rename(FideId=WhiteFideId) %>%
  mutate(PerformanceDif=BlackPerformance-WhitePerformance,
         WinsDif=BlackWins/BlackN-WhiteWins/WhiteN,
         DrawsDif=BlackDraws/BlackN-WhiteDraws/WhiteN,
         LossDif=(1-(BlackDraws+BlackWins)/BlackN)-(1-(WhiteDraws+WhiteWins)/WhiteN))
num_players <- length(unique(game_res$FideId))  # number of players
num_games <- sum(game_res$WhiteN) + sum(game_res$BlackN)  # number of games
avg_white_score <- mean(game_res$WhitePerformance)  # average score with white
avg_black_score <- mean(game_res$BlackPerformance)  # average score with black
sd_perf_dif <- sd(game_res$PerformanceDif)  # std dev of the difference in performances

# plot score difference
hist(game_res$PerformanceDif*100, breaks=40, prob=TRUE,
     xlab='Black Score - White Score',
     main='Score difference for GMs', xlim=c(-30,10))
abline(v=0)
text(x=5, y=0.08, 'Best results\n with black')
text(x=-20, y=0.08, 'Best results\n with white')

# ----------------------------------------
# Normality tests
# ----------------------------------------
# In this section I test whether the difference of scores
# with black and white follows a normal distribution.

# First, a Monte Carlo experience to test whether we should expect
# the difference of scores to follow a normal distribution
n_players <- length(unique(white_res$WhiteFideId))  # number of players in our dataset
n_games <- round(mean(white_res$WhiteN))  # average number of games in our dataset
wpw <- median(white_res$WhiteWins/white_res$WhiteN)  # median probability to win with white
wpd <- median(white_res$WhiteDraws/white_res$WhiteN)  # median probability to draw with white
wpl <- 1 - (wpw + wpd)  # probability to lose with white
bpw <- median(black_res$BlackWins/black_res$BlackN)  # median probability to win with black
bpd <- median(black_res$BlackDraws/black_res$BlackN)  # median probability to draw with black
bpl <- 1 - (bpw + bpd)  # probability to lose with black
# Monte Carlo experience, assuming the probabilities above
WhitePerfDist <- rep(0, n_players)
for (i in 0:n_players){
  extract <- rmultinom(n=n_games, size=1, prob=c(wpw, wpd, wpl))
  perf <- mean(extract[1,]) + 0.5*mean(extract[2,])
  WhitePerfDist[i] <- perf
}
BlackPerfDist <- rep(0, n_players)
for (i in 0:n_players){
  extract <- rmultinom(n=n_games, size=1, prob=c(bpw, bpd, bpl))
  perf <- mean(extract[1,]) + 0.5*mean(extract[2,])
  BlackPerfDist[i] <- perf
}
# Q-Q plot of the Monte Carlo experience results
ggplot() + geom_qq(aes(sample=BlackPerfDist-WhitePerfDist)) +
  stat_qq_line(aes(sample=BlackPerfDist-WhitePerfDist))
# Shapiro-Wilks normality test
shapiro.test(BlackPerfDist-WhitePerfDist)
# Conclusion of the experience: assuming the winning and drawing probabilities
# above, we would actually expect the difference of scores between black and white
# followed a normal distribution if only random factors intervened.

# Now I test if the observed score difference follows a normal distribution
# Q-Q plot
ggplot(data=game_res, aes(sample=PerformanceDif)) + geom_qq() + stat_qq_line()
# Shapiro-Wilks test
shapiro.test(game_res$PerformanceDif)
# Both Q-Q plot and Shapiro-Wilks test are inconsistent with normality.
# There are too many players with high and low values of the score difference.
# This seems to indicate the existence of non-random factors taking place.

# ----------------------------------------
# Best black players
# ----------------------------------------
# select players with better performance with black pieces
black_best <- game_res %>% filter(PerformanceDif>0)

# get their names, Fide IDs, number of games per player and openings used
best_black_players <- inner_join(black_gm, black_best, by=c('BlackFideId'='FideId')) %>%
  mutate(GamesN=WhiteN+BlackN)
best_black_names <- unique(best_black_players[c('Black', "BlackTitle")])
best_black_ID <- unique(best_black_players[c('BlackFideId', 'Black', 'WhitePerformance',
                                             'BlackPerformance', 'PerformanceDif')]) %>%
  arrange(desc(PerformanceDif))
best_black_ngames <- unique(best_black_players[c('BlackFideId', 'GamesN')])
best_black_openings <- unique(best_black_players['Opening'])
best_black_variations <- unique(best_black_players[c('Opening', 'Variation')])
best_black_ECO <- unique(best_black_players['ECO'])

# ----------------------------------------
# Trends for best black players
# ----------------------------------------
# Calculate accumulated statistics and plot trend for best black players
for(id in best_black_ID$BlackFideId){
  dsb <- black_gm[black_gm$BlackFideId==id,]
  dsb <- dsb %>% mutate(CumBlackGames=cumsum(BlackFideId==id),
                        CumBlackWins=cumsum(Result=='0-1'),
                        CumBlackDraws=cumsum(Result=='1/2-1/2'),
                        CumBlackScore=(CumBlackWins+0.5*CumBlackDraws)/CumBlackGames)
  dsw <- white_gm[white_gm$WhiteFideId==id,]
  dsw <- dsw %>% mutate(CumWhiteGames=cumsum(WhiteFideId==id),
                        CumWhiteWins=cumsum(Result=='1-0'),
                        CumWhiteDraws=cumsum(Result=='1/2-1/2'),
                        CumWhiteScore=(CumWhiteWins+0.5*CumWhiteDraws)/CumWhiteGames)
  # merge both datasets
  ds <- full_join(dsb, dsw) %>% arrange(Date)
  ds <- ds %>% mutate(CumBlackScore = zoo::na.fill(CumBlackScore, "extend"),
                      CumWhiteScore = zoo::na.fill(CumWhiteScore, "extend"),
                      CumPerfDif = CumBlackScore - CumWhiteScore)
  for(i in 1:length(ds$CumBlackGames)){
    if (i == 1){
      if(is.na(ds$CumBlackGames[i])){
        ds$CumBlackGames[i] <- 0
      }
      if(is.na(ds$CumWhiteGames[i])){
        ds$CumWhiteGames[i] <- 0
      }
    }else{
      if(is.na(ds$CumBlackGames[i])){
        ds$CumBlackGames[i] <- ds$CumBlackGames[i-1]
      }
      if(is.na(ds$CumWhiteGames[i])){
        ds$CumWhiteGames[i] <- ds$CumWhiteGames[i-1]
      }
    }
  }
  plot(ds$CumPerfDif, main=id, type='l', ylim=c(-0.5, 0.5))
  abline(h=0)
  lines(ds$CumPerfDif + z005 * sd_perf_dif/sqrt(ds$CumBlackGames+ds$CumWhiteGames),
        lty=2, col=2)
  lines(ds$CumPerfDif - z005 * sd_perf_dif/sqrt(ds$CumBlackGames+ds$CumWhiteGames),
        lty=2, col=2)
}

# Calculate accumulated statistics and plot trend for selection of best black players
id_list <- c(2017083, 10600140, 600075, 3206882, 1203363, 1303422)
for(id in id_list){
  player_name <- unique(black_gm$Black[black_gm$BlackFideId==id])
  dsb <- black_gm[black_gm$BlackFideId==id,]
  dsb <- dsb %>% mutate(CumBlackGames=cumsum(BlackFideId==id),
                        CumBlackWins=cumsum(Result=='0-1'),
                        CumBlackDraws=cumsum(Result=='1/2-1/2'),
                        CumBlackScore=(CumBlackWins+0.5*CumBlackDraws)/CumBlackGames)
  dsw <- white_gm[white_gm$WhiteFideId==id,]
  dsw <- dsw %>% mutate(CumWhiteGames=cumsum(WhiteFideId==id),
                        CumWhiteWins=cumsum(Result=='1-0'),
                        CumWhiteDraws=cumsum(Result=='1/2-1/2'),
                        CumWhiteScore=(CumWhiteWins+0.5*CumWhiteDraws)/CumWhiteGames)
  # merge both datasets
  ds <- full_join(dsb, dsw) %>% arrange(Date)
  ds <- ds %>% mutate(CumBlackScore = zoo::na.fill(CumBlackScore, "extend"),
                      CumWhiteScore = zoo::na.fill(CumWhiteScore, "extend"),
                      CumPerfDif = CumBlackScore - CumWhiteScore)
  if(id == id_list[1]){
    plot(ds$CumPerfDif*100, type='l',
         xlim=c(0, 250), ylim=c(-50, 50), xlab='Num. games',
         ylab='Black Score - White Score', col=match(id, id_list),
         main='Score difference as the number of games increases')
    abline(h=0)
    abline(v=50, lty=2)
  } else {
    lines(ds$CumPerfDif*100, col=match(id, id_list))
  }
}
legend('bottomright', legend=c('Smith', 'El Gindy', 'Chabanon',
                               'Tan', 'Suba', 'Gallagher'),
       lty=1, col=1:6)

# ----------------------------------------
# Openings
# ----------------------------------------
# find openings used by best black players
best_black_openings2 <- best_black_players %>% group_by(ECO) %>%
  summarise(N=n(), PerfDifAvg=mean(PerformanceDif), PerfDifStd=sd(PerformanceDif)) %>%
  arrange(desc(N))

# plot histogram of most used openings by best black players
df1 <- best_black_openings2 %>% head(20)
ggplot(data=df1, aes(x=reorder(ECO, -N), y=N)) + geom_bar(stat='identity') +
  xlab('ECO') + ggtitle('Most used openings by best black players')
# plot histogram of most used openings with black pieces
df2 <- black_gm %>% group_by(ECO) %>% summarise(N=n()) %>% filter(N>50) %>%
  arrange(desc(N)) %>% head(20)
ggplot(data=df2, aes(x=reorder(ECO, -N), y=N)) + geom_bar(stat='identity') +
  xlab('ECO') + ggtitle('Most used openings with black pieces')

# Compare best black players versus best white players and rest
summary(game_res$PerformanceDif)
p005 <- quantile(game_res$PerformanceDif, probs=0.05)
p095 <- quantile(game_res$PerformanceDif, probs=0.95)
game_res <- game_res %>%
  mutate(player_class=cut(PerformanceDif, breaks=c(-Inf,p005, p095, Inf),
                          labels=c('much better with white', 'rest',
                                   'better with black')))

bbest <- game_res
bbest <- inner_join(black_gm, bbest, by=c('BlackFideId'='FideId')) %>%
  mutate(ECO1=str_sub(ECO, 1, 1), ECO3=str_sub(ECO, 1, 3)) 

cas1 <- bbest %>%
  group_by(ECO1, player_class) %>%
  summarize(count= n()) %>% 
  group_by(player_class) %>% 
  mutate(percent = count/sum(count)*100)
ggplot(data=cas1, aes(x=ECO1, y=percent, fill=player_class)) + 
  geom_bar(stat='identity', position='dodge') +
  theme(legend.position = c(0.8, 0.8)) +
  xlab('ECO category')
