---
title: "Untitled"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
```

Column {data-width=650}
-----------------------------------------------------------------------

### Chart A

```{r}
ggplot(artists_play_count, aes(x = reorder(Artist, total_plays), y = total_plays)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 10 Artists by Total Play Count") +
  xlab("Artist") +
  ylab("Total Play Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
```

Column {data-width=350}
-----------------------------------------------------------------------

### Chart B

```{r}

```

### Chart C

```{r}

```

