### About
`ggpol` adds parliament diagrams and various other visualizations and convenience functions to `ggplot2`.

### Installation
`ggpol` can be installed via GitHub:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('erocoar/ggpol')
```
### Selected Features
Below are two functions added by `ggpol`. For a full overview with applications, please refer to the [vignette](https://erocoar.github.io/ggpol/).


`geom_parliament` draws a parliament diagram, clustering points along an arc by parties with each point representing a single member of parliament. 

```r
bt <- data.frame(
 parties = factor(c("CDU", "CSU", "AfD", "FDP", "SPD", 
                    "Linke", "Gruene", "Fraktionslos"),
                  levels = c("CDU", "CSU", "AfD", "FDP", "SPD", 
                             "Linke", "Gruene", "Fraktionslos")),
 seats   = c(200, 46, 92, 80, 153, 69, 67, 2),
 colors  = c("black", "blue", "lightblue", "yellow", 
             "red","purple", "green", "grey"),
 stringsAsFactors = FALSE)

ggplot(bt) + 
  geom_parliament(aes(seats = seats, fill = parties), color = "black") + 
  scale_fill_manual(values = bt$colors, labels = bt$parties) +
  coord_fixed() + 
  theme_void()
```

![Parliament Diagram](https://i.imgur.com/gvsCvBH.png)

`geom_boxjitter` produces a hybrid of box- and scatterplot. 

```r
df <- data.frame(score = rgamma(150, 4, 1), 
                 gender = sample(c("M", "F"), 150, replace = TRUE), 
                genotype = factor(sample(1:3, 150, replace = TRUE)))

ggplot(df) + geom_boxjitter(aes(x = genotype, y = score, fill = gender),
                            jitter.shape = 21, jitter.color = NA, 
                            jitter.height = 0, jitter.width = 0.04,
                            outlier.color = NA, errorbar.draw = TRUE) +
  scale_fill_manual(values = c("#ecb21e", "#812e91")) +
  theme_minimal()
```

![Hybrid Boxplot](https://i.imgur.com/zlwIs14.png)

### Roadmap