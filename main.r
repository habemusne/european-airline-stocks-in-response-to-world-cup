require('lubridate')
require('gridExtra')
require('ggplot2')
NUM_COL = 3
DATA_DIR = 'data_max/combined.csv'
STOCKS = c('RYA.L', 'AFRAF', 'DLAKF', 'TKHVY', 'AFLT.ME', 'IAG.L')
SELECTED_DATE_START = '2017-10-01'
SELECTED_NUM_DAYS_AFTER = '180'

round_to_nearest = function(x, target) {
  return(target * round(x / target))
}

# Plot normal probability plot
# @param data: data frame
# @param title: title of the plot
# @param dep_name: name of dependent variable. Must exists in column names of data
# @param indep_name: name of independent variable. Must exists in column names of data
plot_normal_probability = function(data, title, dep_name='y', indep_name='x') {
  formula = as.formula(paste(dep_name, ' ~ ', indep_name))
  model = lm(formula, data=data)
  stdres = rstandard(model)
  tryCatch({
    qqnorm(stdres,
           main=title,
           xlab='standardized residuals',
           ylab='normal scores'
    )
    qqline(stdres)
  }, error=function(error_message){
    message(error_message)
    plot.new()
  })
}

# Plot the observation pairs and also a regression line
# @param data: data frame
# @param title: title of the plot
# @param dep_name: name of dependent variable. Must exists in column names of data
# @param indep_name: name of independent variable. Must exists in column names of data
#
# return: linear regression model
plot_regression = function(data, title, dep_name='y', indep_name='x') {
  formula = as.formula(paste(dep_name, ' ~ ', indep_name))
  model = lm(formula, data=data)
  plot(
    unlist(data[indep_name]),
    unlist(data[dep_name]),
    main=paste(indep_name, ' vs ', dep_name),
    xlab=indep_name,
    ylab=dep_name
  )
  abline(model)
  return(model)
}

# Calculate confidence intervals for mean or variance
# @param numbers: vector, observations
# @param mode: 'mean' or 'variance'
# @param level: numeric, significance level
# 
# return: vector, c(left bound, right bound)
confidence_interval = function(numbers, mode='mean', level=0.95) {
  n = length(numbers)
  sample_var = var(numbers)
  sample_mean = mean(numbers)
  alpha = 1 - level
  if (mode == 'mean') {
    error = qt(alpha/2, n-1, lower.tail=FALSE) * sample_var / sqrt(n)
    return(c(sample_mean-error, sample_mean+error))
  } else if (mode == 'variance') {
    left = (n-1) * sample_var^2 / qchisq(alpha/2, n-1, lower.tail=FALSE)
    right = (n-1) * sample_var^2 / qchisq(1 - alpha/2, n-1, lower.tail=FALSE)
    return(c(left, right))
  }
}

# Test mean of two samples using t-test
# @param numbers_x: vector, independent observations
# @param numbers_y: vector, dependent observations
# @param level: level of significance
#
# return: numberic, the p-value
test_2_sample_mean = function(numbers_x, numbers_y, level=0.95) {
  alpha = 1 - level
  n = length(numbers_x)
  m = length(numbers_y)
  sample_mean_x = mean(numbers_x)
  sample_mean_y = mean(numbers_y)
  sample_var_x = var(numbers_x)
  sample_var_y = var(numbers_y)
  test_stat = (sample_mean_x-sample_mean_y) / sqrt(sample_var_x^2/n + sample_var_y^2/m)
  p_value = 2 * pt(abs(test_stat), lower.tail=FALSE, df=min(n, m)-1)
  return(p_value)
}

# This is a basic function to read and subset data of certain date range
# @param date_start: a string of format '%Y-%m-%d'
# @param num_days_after: a number representing day range. If it is 0, then the maximum amount of days will be returned
#
# return: data frame, the subset of data
preprocess_data_single_range = function(date_start, num_days_after) {
  if (num_days_after != -1)
    date_end = as.Date(date_start) + days(num_days_after)
  else
    date_end = '2030-12-31'
  
  combined_data = read.csv(DATA_DIR)
  combined_data$date = as.Date(combined_data$date, '%Y-%m-%d')
  result = subset(combined_data,
                  date_start <= combined_data$date &
                    combined_data$date <= date_end)
  result$index = result$index - result$index[1]
  return(result)
}

# This is a general function to combine different date rage data.
# @param dates_start: a vector of dates of format '%Y-%m-%d'
# @param num_days_after: a number representing day range. If it is 0, then the maximum amount of days will be returned
#
# return: data frame, the combined subset of data
preprocess_data_multiple_range = function(dates_start, num_days_after) {
  result = preprocess_data_single_range(
    dates_start[1],
    num_days_after
  )
  for (i in 2:length(dates_start)) {
    rbind(
      result,
      preprocess_data_single_range(
        dates_start[i],
        num_days_after
      )
    )
  }
  return(result)
}

stat_single_stock = function(stocks, dates_start=c('1970-01-01'), num_days_after=365, draw=TRUE) {
  combined_data = preprocess_data_multiple_range(dates_start, num_days_after)
  
  if (draw == TRUE) {
    num_plot_slots = round_to_nearest(length(stocks), NUM_COL)
    layout(matrix(c(1:num_plot_slots), nc=NUM_COL, byrow=TRUE))
    par(mar=c(1,1,1,1))
    # par(mfrow=c(3,2),oma = c(0, 0, 2, 0))
    for (stock in stocks) {
      curr_data = combined_data[c('index', stock)]
      colnames(curr_data) = c('day', 'log_return')
      if (draw == TRUE) {
        plot_normal_probability(curr_data, stock, dep_name='log_return', indep_name='day')
      }
    }
    mtext(paste('', num_days_after, ' days after ', paste(dates_start, sep=',', collapse=',')), outer=TRUE, cex=1)
    # mtext('sdsdsdsds', outer=TRUE)
  }
  stocks_stat = sapply(stocks, function(stock){
    numbers = unlist(combined_data[stock])
    ci_mean = confidence_interval(numbers)
    ci_var = confidence_interval(numbers, mode='variance')
    formula = as.formula(paste(stock, ' ~ ', 'index'))
    model = lm(formula, data=combined_data[c('index', stock)])
    result = c(ci_mean, ci_var)
    names(result) = c('ci_mean_l', 'ci_mean_r', 'ci_var_l', 'ci_var_r')
    return(result)
  })
  names(stocks_stat) = stocks
  return(stocks_stat)
}

# Single stock analysis: generate 9 normal prob plots for all stocks
report_singles_1 = function() {
  for (date_start in c('2017-07-01', '2017-10-01', '2018-01-01')) {
    for (num_days_after in c(90, 180, 360)) {
      stat_single_stock(STOCKS, dates_start=c(date_start), num_days_after=num_days_after)
    }
  }
}

# Single stock analysis: generate statistical report for all stocks
report_singles_2 = function() {
  for (date_start in c('2017-10-01', '2018-01-01')) {
    for (num_days_after in c(90, 180)) {
      stat = stat_single_stock(STOCKS, dates_start=c(date_start), num_days_after=num_days_after, draw=FALSE)
      stat_table = apply(stat, 2, function(row){
        return(c(
          paste(
            round(row['ci_mean_l'], 8),
            round(row['ci_mean_r'], 8),
            sep=',', collapse=','
          ),
          paste(
            round(row['ci_var_l'], 8),
            round(row['ci_var_r'], 8),
            sep=',', collapse=','
          )
        ))
      })
      stat_table = t(stat_table)
      colnames(stat_table) = c('ci_mean', 'ci_var')
      print(paste('CI for ', num_days_after, ' days after ', date_start, sep=''))
      print(stat_table)
      if (date_start == SELECTED_DATE_START &
          num_days_after == SELECTED_NUM_DAYS_AFTER) {
        grid.table(stat_table)
      }
      print('')
    }
  }  
}

# Single stock analysis: linear regression on log return against time
report_singles_3 = function() {
  num_plot_slots = round_to_nearest(6, NUM_COL)
  selected_stat_table = matrix(NA, length(STOCKS), 6)
  for (date_start in c('2017-10-01', '2018-01-01')) {
    for (num_days_after in c(90, 180)) {
      layout(matrix(c(1:6), nc=NUM_COL, byrow=TRUE))
      par(mar=c(1,1,1,1))
      for (i in 1:length(STOCKS)) {
        combined_data = preprocess_data_multiple_range(c(date_start), num_days_after)
        model = plot_regression(
          combined_data[c('index', STOCKS[i])],
          paste(num_days_after, ' days after ', date_start),
          dep_name=STOCKS[i],
          indep_name='index'
        )
        print(paste('model for', num_days_after, 'days after', date_start, sep=' ', collapse=' '))
        print(model)
        if (date_start == SELECTED_DATE_START & num_days_after == SELECTED_NUM_DAYS_AFTER) {
          selected_stat_table[i,] = c(
            round(model[[1]][1], 6),
            round(model[[1]][2], 6),
            round(summary(model)[[6]], 6),
            round(summary(model)[[8]], 6),
            round(summary(model)[[4]][[6]], 6),
            round(summary(model)[[4]][[8]], 6)
          )
        }
      }
      title = paste(num_days_after, ' days after ', c(date_start))
      mtext(title, outer=TRUE, cex=1)
    }
  }
  rownames(selected_stat_table) = STOCKS
  colnames(selected_stat_table) = c('b0', 'b1', 'R.std.err', 'R2', 't.b1', 'p.b1')
  plot.new()
  grid.table(selected_stat_table)
  
  print(min(selected_stat_table[,'R.std.err']/abs(selected_stat_table[,'b0'])))
  print(max(selected_stat_table[,'R.std.err']/abs(selected_stat_table[,'b0'])))
  print(mean(selected_stat_table[,'R.std.err']/abs(selected_stat_table[,'b0'])))
  print(var(selected_stat_table[,'R.std.err']/abs(selected_stat_table[,'b0'])))
}

# Multiple Stock Analysis
report_pairs_1 = function() {
  combined_data = preprocess_data_multiple_range(c(SELECTED_DATE_START), SELECTED_NUM_DAYS_AFTER)
  
  layout(matrix(c(1:6), nc=NUM_COL, byrow=TRUE))
  par(mar=c(1,1,1,1))
  stat = matrix(NA, 5, 3)
  
  selected_stat_table = matrix(NA, length(STOCKS)-1, 4)
  stock1 = STOCKS[1]
  for (i in 2:length(STOCKS)) {
    stock2 = STOCKS[i]
    model = plot_regression(
      combined_data[c(stock1, stock2)],
      paste(num_days_after, ' days after ', dates_start[1]),
      dep_name=stock2,
      indep_name=stock1
    )
    selected_stat_table[i-1,] = c(
      stock2,
      test_2_sample_mean(unlist(combined_data[stock1]), unlist(combined_data[stock2])),
      round(model[[1]][2], 6),
      round(summary(model)[[8]], 6)
    )
  }
  colnames(selected_stat_table) = c('stock2', 'p_value', 'slope', 'r2')
  rownames(selected_stat_table) = STOCKS[c(2:length(STOCKS))]
  plot.new()
  plot.new()
  grid.table(selected_stat_table)
}

# Analyze a single stock
report_single = function(stock, date_start, num_days_after) {
  combined_data = preprocess_data_multiple_range(c(date_start), num_days_after)
  
  print('Displaying histogram...')
  hist(combined_data[,stock], main='Log Return Frequency')
  readline(prompt='Press [enter] to continue')
  
  print('Displaying normal probability plot...')
  curr_data = combined_data[c('index', stock)]
  colnames(curr_data) = c('day', 'log_return')
  plot_normal_probability(curr_data, stock, dep_name='log_return', indep_name='day')
  readline(prompt='Press [enter] to continue')
  
  level = readline(prompt='Enter significance level (e.g. 0.95): ')
  level = as.numeric(level)
  ci_mean = confidence_interval(unlist(combined_data[,stock]), 'mean', level)
  ci_var = confidence_interval(unlist(combined_data[,stock]), 'variance', level)
  print('Confidence interval for mean: ')
  print(ci_mean)
  print('Confidence interval for var: ')
  print(ci_var)
  readline(prompt='Press [enter] to continue')
  
  print('Displaying linear regression...')
  model = lm(log_return ~ day, data=curr_data)
  curr_data$predicted = predict(model)
  curr_data$residuals = residuals(model)
  p = ggplot(curr_data, aes(x=day, y=log_return)) +
    geom_smooth(method='lm', se=FALSE, color='lightgrey') +
    geom_segment(aes(xend=day, yend=predicted), alpha=.2) +
    geom_point(aes(color=abs(residuals), size=abs(residuals))) +
    scale_color_continuous(low="black", high="red") +
    guides(color=FALSE, size=FALSE) +
    geom_point() +
    geom_point(aes(y=predicted), shape=1) +
    theme_bw()
  print(p)
  print(paste('Intercept: ', model[[1]][[1]], ', slope: ', model[[1]][[2]], ' R2: ', summary(model)[[8]]))
}

# Analyze a two stock2
report_pair = function(stock1, stock2, date_start, num_days_after) {
  combined_data = preprocess_data_multiple_range(c(date_start), num_days_after)
  p_value = test_2_sample_mean(unlist(combined_data[stock1]), unlist(combined_data[stock2]))
  print(paste('p-value for 2-sample means: ', p_value))
  
  print('Displaying linear regression...')
  curr_data = combined_data[c(stock1, stock2)]
  model = plot_regression(curr_data, 'Linear Regression', stock2, stock1)
  
  readline(prompt='Press [enter] to continue')
  print('Displaying graphical depiction of residuals...')
  plot(density(resid(model)))
  qqnorm(resid(model))
  qqline(resid(model))
  print(paste('Intercept: ', model[[1]][[1]], ', slope: ', model[[1]][[2]], ' R2: ', summary(model)[[8]]))
}

run = function() {
  cat('Available Stocks: RYA.L, AFRAF, DLAKF, TKHVY, AFLT.ME, IAG.L\nAvailable commands: \n  1. report_single: analyze a single stock\n  2. report_pair: analyze two stocks\n  report_singles_1: first step in my report; normal probability plots for all 6 stocks available\n  3. report_singles_2: second step in my report; statistical report for all 6 stocks available\n  4. report_singles_3: third step in my report; display linear regression result for all 6 stocks available\n\n  5. report_pairs_1: last step in my report; statistical analysis on pairs of stocks')
  command = readline(prompt='Enter the command: ')
  if (command == 'report_single') {
    args = readline(prompt='Enter "symbol,date_start,num_days"(e.g. RYA.L,2017-10-01,180): ')
    args = strsplit(args, ',')
    report_single(args[[1]][1], args[[1]][2], as.integer(args[[1]][3]))
  } else if (command == 'report_pair') {
    args = readline(prompt='Enter "symbol1,symbol2,date_start,num_days"(e.g. RYA.L,IAG.L,2017-10-01,180): ')
    args = strsplit(args, ',')
    report_pair(args[[1]][1], args[[1]][2], args[[1]][3], as.integer(args[[1]][4]))
  } else {
    do.call(command, list())
  }
}

run()
