---
  title: "AP4"
---
  
  ## Quarto
  
  Quarto enables you to weave together content and executable code into a finished document. To learn more about Quarto see <https://quarto.org>.

```{r}
#| label: setup
#| include: false

lapply(
  c("effectsize","rstatix","emmeans","tidyverse"),
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE, ask = FALSE)
    }
    library(x, character.only = TRUE)
  }
)
```

```{r}
#| label: simulacao

set.seed(42)
dat <- tibble(
  age = factor(rep(c("6dpf","15dpf","adult"), each=20)),
  outcome = c(rnorm(20, 5, 1), rnorm(20, 6, 1.2), rnorm(20, 7.2, 1.1))
)
```

```{r}
#| label: anova

aov_res <- aov(outcome ~ age, data = dat)
summary(aov_res)
```
```{r}
#| label: anova via rstatix

rstatix::anova_test(data = dat, dv = outcome, between = age)
```

```{r}
#| label: effect size

effectsize::eta_squared(aov_res, partial = FALSE)   # eta²
effectsize::eta_squared(aov_res, partial = TRUE)    # eta² parcial
```
```{r}
#| label: comparacao paredata

emm <- emmeans(aov_res, specs = ~ age)
pairs(emm, adjust = "tukey")  # comparações e p ajustado
```

```{r}
#| label: recuperar t a partir do p e df

p <- 0.057
df <- 40
t <- qt(p/2, df, lower.tail = FALSE)  # teste bicaudal
t
```

```{r}
#| label: t do effect size

library(effectsize)

t_to_d(t, df = df)
```

```{r}
#| label: tamanho de amostra


library(pwr)
p <- pwr.t.test(d = 0.62, sig.level = 0.05, power = 0.80, type = "two.sample")
p
```
```{r}
library(ggplot2)

quartis <- data.frame(
  quartil = factor(c("Q1","Q2","Q3","Q4"), 
                   levels = c("Q1","Q2","Q3","Q4")),
  nivel = c(4,3,2,1)
)

ggplot(quartis, aes(x = quartil, y = nivel, fill = quartil)) +
  geom_col() +
  geom_text(aes(label = quartil), vjust = -0.5, size = 6) +
  scale_fill_manual(values = c("gray80","steelblue","gray80","gray80")) +
  ggtitle("Posição do periódico: Q2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
```
fasdfsad

```{r}
library(ggplot2)

value <- 0.969

ggplot(data.frame(x = value), aes(x)) +
  geom_bar(aes(y = x), stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(x = "", y = "SJR") +
  theme_minimal(base_size = 14)
```

fdasf
