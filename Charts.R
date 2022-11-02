mfun1 <- function(x) {
  return((x ^ 3 - 2) / (x - 1) ^ 2)
}
#ОТОБРАЖЕНИЕ ГРАФИКА [-3;3]
{
  x.data <- c(seq(-3, 3, by = 0.1))
  y.data <- mfun1(x = x.data)
  
  par(bg = 'snow2',
      fg = 'black',
      bty = 'O',
      lwd = 3)
  
  plot(
    x = x.data,
    y = y.data,
    col = 1,
    bg = 'green',
    type = 'p',
    cex = ifelse(x.data %% 0.5 == 0 | y.data < -6, 1.5, 0),
    pch = 21,
    lwd = 2,
    main = 'График функции y',
    cex.main = 2,
    col.main = 'green4',
    font.main = 8,
    xlab = 'ось X',
    ylab = 'ось Y',
    font.lab = 8,
    cex.lab = 1.75,
    col.lab = 'olivedrab',
    cex.axis = 1.5,
    col.axis = 'green4',
    font.axis = 8,
    sub = 'Область определения графика [-3, 3]',
    col.sub = 'red4',
    cex.sub = 1.25,
    font.sub = 8,
    ylim = c(-66.9 + 25, 6.25 + 10),
    xlim = c(-3 + 0.15, 3 - 0.15)
  )
  
  abline(v = 1,
         lwd = 3,
         lty = 2,
         col = 'black')
  
  abline(
    v = c(-3:0, 2:3),
    h = seq(-40, 10, by = 10),
    col = 'grey',
    lwd = 2,
    lty = 1
  )
  
  lines(
    x = x.data,
    y = y.data,
    col = 'green',
    lty = 1,
    lwd = 2
  )
  
  legend(
    x = 'bottomleft',
    inset = 0.01,
    legend = 'Асимптота (x = 1)',
    text.font = 4,
    cex = 1,
    title = expression('y' == frac(x ^ 3 - 2, (x - 1) ^ 2)),
    title.cex = 1.2,
    lwd = 2,
    lty = 2,
    bg = rgb(0, 1, 0, alpha = 0.2),
    box.lwd = 2,
    box.col = 'green4',
    box.lty = 1
  )
}

#ОТОБРАЖЕНИЕ ГРАФИКА [-7;10]
{
  x.data <- c(seq(-7, 10, by = 0.1))
  y.data <- mfun1(x = x.data)
  
  par(bg = 'bisque',
      fg = 'bisque',
      bty = 'O')
  
  plot(
    x = x.data,
    y = y.data,
    xaxp = c(-7, 10, 17),
    yaxp = c(-40, 20, 12),
    col = 'red',
    bg = 'black',
    type = 'p',
    cex = ifelse(x.data %% 1 == 0 | y.data < -6, 1.5, 0),
    pch = 23,
    lwd = 2,
    main = 'График функции y',
    cex.main = 2,
    col.main = 'brown4',
    font.main = 15,
    xlab = 'ось X',
    ylab = 'ось Y',
    font.lab = 15,
    cex.lab = 1.75,
    col.lab = 'red2',
    cex.axis = 0.75,
    col.axis = 'brown4',
    font.axis = 15,
    sub = 'Область определения графика [-7, 10]',
    col.sub = 'black',
    cex.sub = 1.25,
    font.sub = 15,
    ylim = c(-66.9 + 40, 6.25 + 5),
    xlim = c(-7 + 0.5, 10 - 0.5)
  )
  
  abline(v = 1,
         lwd = 3,
         lty = 2,
         col = 'black')
  
  abline(
    v = c(-7:0, 2:10),
    h = seq(-40, 20, by = 5),
    col = 'brown',
    lwd = 1,
    lty = 2
  )
  
  lines(
    x = x.data,
    y = y.data,
    col = 'red',
    lty = 1,
    lwd = 3
  )
  
  legend(
    x = 3,
    y = -15,
    legend = 'Асимптота (x = 1)',
    text.font = 15,
    text.col = 'black',
    title = expression('y' == frac(x ^ 3 - 2, (x - 1) ^ 2)),
    title.cex = 1.25,
    title.col = 'black',
    lwd = 3,
    lty = 2,
    cex = 1,
    col = 'black',
    bg = rgb(1, 0, 0, alpha = 0.2),
    box.col = rgb(1, 0, 0, alpha = 0),
  )
}

mfun2 <- function(x) {
  return((x ^ 2 - 9) / (x + 5))
}
#ОТОБРАЖЕНИЕ ГРАФИКА №2 [-3;3]
{
  x.data <- c(seq(-3, 3, by = 0.1))
  y.data <- mfun2(x = x.data)
  
  par(bg = 'black',
      fg = 'white',
      lwd = 2,
      bty = 'n')
  
  plot(
    x = x.data,
    y = y.data,
    col = sample(c('blue', 'red', 'yellow', 'purple', 'green'), size = length(x.data), replace = T),
    type = 'p',
    cex = ifelse(x.data <= - 2 | x.data >= 0, 2, 1),
    pch = 21,
    lwd = 2,
    main = expression('График функции y' == frac(x ^ 2 - 9, (x + 5))),
    cex.main = 1.5,
    col.main = 'green',
    xlab = 'X',
    ylab = 'Y',
    font.lab = 13,
    cex.lab = 1.75,
    col.lab = 'green',
    cex.axis = 1.5,
    col.axis = 'green4',
    font.axis = 13,
    sub = 'Область определения графика [-3, 3]',
    col.sub = 'white',
    cex.sub = 1.25,
    font.sub = 13,
    ylim = c(-2.25, 0.25)
  )
  
  abline(
    v = c(-3:3),
    h = seq(-2, 0, by = 0.5),
    col = 'green4',
    lwd = 0.5,
    lty = 2
  )
  
  abline(
    a = -5,
    b = 1,
    col = 'white',
    lty = 2,
    lwd = 2
  )
  
  lines(
    x = x.data,
    y = y.data,
    col = 'white',
    lty = 1,
    lwd = 0.5
  )
  
  legend(
    x = 'top',
    legend = 'Асимптота (y = x - 5)',
    text.font = 13,
    cex = 1,
    text.col = 'black',
    col = 'white',
    title = 'Легенда',
    title.cex = 1.2,
    lwd = 2,
    lty = 2,
    bg = 'green4',
    box.lwd = 2,
    box.lty = 1,
    box.col = 'white'
  )
}
#ОТОБРАЖЕНИЕ ГРАФИКА [-7;10]
{
  x.data <- c(seq(-7, 10, by = 0.1))
  y.data <- mfun2(x = x.data)
  
  par(bg = 'brown4',
      fg = 'white',
      lwd = 3,
      bty = 'O')
  
  plot(
    x = x.data,
    y = y.data,
    xaxp = c(-7, 10, 17),
    yaxp = c(-90, 70, 8),
    col = 'white',
    bg = 'black',
    type = 'p',
    cex = ifelse(x.data %% 0.5 == 0 | y.data <= -30 | y.data >= 20, 1.25, 0),
    pch = 21,
    lwd = 1,
    main = expression('График функции y' == frac(x ^ 2 - 9, (x + 5))),
    cex.main = 1.5,
    col.main = 'white',
    xlab = 'ОСЬ X',
    ylab = 'ОСЬ Y',
    font.lab = 7,
    cex.lab = 1.5,
    col.lab = 'white',
    cex.axis = 0.75,
    col.axis = 'white',
    font.axis = 7,
    sub = 'Область определения графика [-7, 10]',
    col.sub = 'white',
    cex.sub = 1.25,
    font.sub = 7,
    ylim = c(-90, 70),
    xlim = c(-7 + 0.3, 10 - 0.3)
  )
  
  abline(
    v = c(-7:-6, -4:10),
    h = seq(-170, 150, by = 10),
    col = 'black',
    lwd = 0.5,
    lty = 1
  )
  
  abline(v = -5,
         lwd = 2,
         lty = 2,
         col = 'white')
  
  abline(
    a = -5,
    b = 1,
    col = 'grey',
    lty = 2,
    lwd = 2
  )
  
  lines(
    x = x.data,
    y = y.data,
    col = 'white',
    lty = 1,
    lwd = 1
  )
  
  legend(
    x = 'bottomright',
    inset = 0.05,
    legend = c('Асимптота (x = -5)', 'Асимптота (y = x - 5)'),
    text.font = 7,
    cex = 1,
    text.col = 'black',
    col = c('white', 'grey'),
    title = 'Легенда',
    title.cex = 1.2,
    lwd = c(2, 2),
    lty = c(2, 2),
    bg = 'brown',
    box.lwd = 2,
    box.col = 'black',
    box.lty = 1
  )
}


mfun3 <- function(x) {
  return((9 * x - 9) / (x - 2) ^ 2)
}
#ОТОБРАЖЕНИЕ ГРАФИКА [-3;3]
{
  x.data <- c(seq(-3, 3, by = 0.1))
  y.data <- mfun3(x = x.data)
  
  par(bg = 'royalblue',
      fg = 'royalblue4',
      bty = 'O',
      lwd = 3)
  
  plot(
    x = x.data,
    y = y.data,
    yaxp = c(-10, 130, 14),
    col = 1,
    bg = 'blue',
    type = 'p',
    cex = ifelse(x.data %% 0.5 == 0 | y.data >= 10, 1.5, 0),
    pch = sample(x = c(21, 22), size = length(x.data), replace = T),
    lwd = 1,
    main = 'ГРАФИК ФУНКЦИИ Y',
    cex.main = 2,
    col.main = 'white',
    font.main = 6,
    xlab = 'X',
    ylab = 'Y',
    font.lab = 8,
    cex.lab = 1.5,
    col.lab = 'gray99',
    cex.axis = 1.25,
    col.axis = 'black',
    font.axis = 8,
    sub = 'Область определения графика [-3, 3]',
    col.sub = 'black',
    cex.sub = 1.25,
    font.sub = 8,
    ylim = c(-10, 130),
    xlim = c(-3 + 0.15, 3 - 0.15)
  )
  
  abline(v = 2,
         lwd = 3,
         lty = 2,
         col = 'black')
  
  abline(
    v = c(-3:1, 3:3),
    h = seq(-10, 150, by = 10),
    col = 'grey',
    lwd = 2,
    lty = 1
  )
  
  lines(
    x = x.data,
    y = y.data,
    col = 'blue',
    lty = 3,
    lwd = 2
  )
  
  legend(
    x = 'topleft',
    inset = 0.03,
    legend = 'Асимптота (x = 2)',
    text.font = 4,
    text.col = 'white',
    col = 'black',
    cex = 1,
    title = expression('y' == frac(9*x - 9, (x - 2) ^ 2)),
    title.cex = 1.2,
    title.col = 'skyblue',
    lwd = 2,
    lty = 2,
    bg = rgb(0, 0, 0.5, alpha = 0.4),
    box.col = rgb(0, 0, 0.5, alpha = 0)
  )
}

mfun4 <- function(x) {
  return((x ^ 3 - 4) / x ^ 2)
}
#ОТОБРАЖЕНИЕ ГРАФИКА [-3;3]
{
  x.data <- c(seq(-3, 3, by = 0.1))
  y.data <- mfun4(x = x.data)
  
  par(bg = 'pink',
      fg = 'purple',
      bty = 'O',
      lwd = 2)
  
  plot(
    x = x.data,
    y = y.data,
    yaxp = c(-30, 10, 8),
    col = c(sample(x = 1:657, size = length(x.data))),
    bg = c(sample(x = 1:657, size = length(x.data))),
    type = 'b',
    pch = 21,
    cex = 1,
    lwd = 2, 
    main = expression('ГРАФИК ФУНКЦИИ Y' == frac(x ^ 3 - 4, x ^ 2)),
    cex.main = 1.5,
    col.main = 'violetred',
    font.main = 6,
    xlab = '--------- X --------->',
    ylab = '--------- Y --------->',
    font.lab = 8,
    cex.lab = 1.5,
    col.lab = 'violetred',
    cex.axis = 1.25,
    col.axis = 'purple',
    font.axis = 8,
    sub = 'Область определения графика [-3, 3]',
    col.sub = 'black',
    cex.sub = 1.25,
    font.sub = 8,
    ylim = c(-30, 10),
    xlim = c(-3 + 0.15, 3 - 0.15)
  )
  
  abline(v = 0,
         lwd = 3,
         lty = 1,
         col = 'purple')
  
  abline(
    v = c(-3:-1, 1:3),
    h = seq(-30, 10, by = 5),
    col = 'plum',
    lwd = 2,
    lty = 4
  )
  
  abline(
    a = 0,
    b = 1,
    col = 'purple4',
    lty = 2,
    lwd = 2
  )
  
  legend(
    x = 'bottomleft',
    legend = c('Асимптота (x = 0)', 'Асимптота (y = x)'),
    text.font = 4,
    text.col = 'purple',
    col = c('purple', 'purple4'),
    cex = 0.75,
    title = 'Легенда',
    title.cex = 1,
    title.col = 'violetred',
    lwd = c(2, 2),
    lty = c(1, 2),
    bty = 'n'
  )
}

mfun5 <- function(x) {
  return((x - 2) ^ 2 / (x ^ 2 - 4 * x))
}
#ОТОБРАЖЕНИЕ ГРАФИКА [-3;3]
{
  x.data <- c(seq(-3, 3, by = 0.1))
  y.data <- mfun5(x = x.data)
  
  par(bg = 'springgreen3',
      fg = 'green4',
      bty = 'n',
      lwd = 1)
  
  plot(
    x = x.data,
    y = y.data,
    yaxp = c(-10, 10, 10),
    col = 1,
    type = 'p',
    pch = 21,
    cex = ifelse(x.data %% 0.5 == 0 | y.data <= -2 | y.data >= 2, 1, 0),
    lwd = 2, 
    main = expression('ГРАФИК ФУНКЦИИ Y' == frac((x - 2)^2, x ^ 2 - 4*x)),
    cex.main = 1.5,
    col.main = 'black',
    font.main = 6,
    xlab = '------------------ X ------------------>',
    ylab = '------------------ Y ------------------>',
    font.lab = 8,
    cex.lab = 1.5,
    col.lab = 'green4',
    cex.axis = 1.25,
    col.axis = 'black',
    font.axis = 8,
    sub = 'Область определения графика [-3, 3]',
    col.sub = 'black',
    cex.sub = 1.25,
    font.sub = 8,
    ylim = c(-8.5, 10),
    xlim = c(-3 + 0.15, 3 - 0.15)
  )
  
  abline(v = 0,
         lwd = 2,
         lty = 5,
         col = 'black')
  
  abline(
    v = c(-3:-1, 1:3),
    h = seq(-10, 10, by = 2),
    col = 'green4',
    lwd = 2,
    lty = 3
  )
  
  abline(
    h = 1,
    col = 'black',
    lty = 2,
    lwd = 2
  )
  
  lines(
    x = x.data,
    y = y.data,
    col = 'black',
    lty = 1,
    lwd = 1
  )
  
  legend(
    x = 'topright',
    legend = c('Асимптота (x = 0)', 'Асимптота (y = 1)'),
    text.font = 4,
    text.col = 'black',
    col = 'black',
    cex = 0.75,
    title = 'Легенда',
    title.cex = 1,
    title.col = 'black',
    lwd = 2,
    lty = c(5, 2),
    box.col = rgb(0, 1, 0, alpha = 0), 
    bg = rgb(0, 1, 0, alpha = 0.15)
  )
}