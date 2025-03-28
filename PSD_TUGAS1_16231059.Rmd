---
title: "Brexit"
date: "2025-03-23"
output: html_document
---

Brexit <- read_csv("C:/Users/FYRA/Downloads/brexit.csv")

### Exercise 1 - Free scales
Add `scales = "free_x"` as an argument to the `facet_wrap()` function. How does the visualisation change? How is the story this visualisation telling different than the story the original plot tells?

```{r}
library(tidyverse)

brexit <- read_csv("C:/Users/FYRA/Downloads/brexit.csv")

# memasukkan reegion
brexit <- brexit %>%
  mutate(
    region = fct_relevel(region, "london", "rest_of_south", "midlands_wales", "north", "scot"),
    region = fct_recode(region, 
                        London = "london", 
                        `Rest of South` = "rest_of_south", 
                        `Midlands / Wales` = "midlands_wales", 
                        North = "north", 
                        Scotland = "scot")
  )

# bikin plot dengan free_x scale
ggplot(brexit, aes(y = opinion, fill = opinion)) +
  geom_bar() +
  facet_wrap(~region, nrow = 1, labeller = label_wrap_gen(width = 12), scales = "free_x") +  # Adding scales = "free_x"
  guides(fill = FALSE) +
  labs(
    title = "Was Britain right/wrong to vote to leave EU? (Free Scales)",
    subtitle = "YouGov Survey Results, 2-3 September 2019",
    caption = "Source: bit.ly/2lCJZVg",
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c(
    "Wrong" = "#ef8a62",
    "Right" = "#67a9cf",
    "Don't know" = "gray"
  )) +
  theme_minimal()
```

**Jawaban:** Dengan menambahkan `scales = "free_x"`, ukuran sumbu X di setiap panel bisa berbeda sesuai jumlah responden dalam setiap kategori di masing-masing region. Ini membuat perbandingan absolut antara region menjadi lebih sulit, tetapi lebih jelas dalam melihat distribusi internal tiap region.

### Exercise 2 - Comparing proportions across facets
First, calculate the proportion of wrong, right, and don't know answers in each category and then plot these proportions (rather than the counts) and then improve axis labeling. How is the story this visualisation telling different than the story the original plot tells?

```{r}
library(tidyverse)

brexit <- read_csv("C:/Users/FYRA/Downloads/brexit.csv")

brexit_prop <- brexit %>%
  group_by(region, opinion) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count))  # Menghitung proporsi

# memasukkan nama region
brexit_prop <- brexit_prop %>%
  mutate(
    region = fct_relevel(region, "london", "rest_of_south", "midlands_wales", "north", "scot"),
    region = fct_recode(region, 
                        London = "london", 
                        `Rest of South` = "rest_of_south", 
                        `Midlands / Wales` = "midlands_wales", 
                        North = "north", 
                        Scotland = "scot")
  )

# bikin plot pakai proporsi
ggplot(brexit_prop, aes(x = region, y = proportion, fill = opinion)) +
  geom_col(position = "fill") +  # Menggunakan stacked bar dengan tinggi 1
  scale_y_continuous(labels = scales::percent_format()) +  # Menampilkan dalam format persen
  labs(
    title = "Proportion of Opinions on Brexit by Region",
    subtitle = "YouGov Survey Results, 2-3 September 2019",
    caption = "Source: bit.ly/2lCJZVg",
    x = "Region", 
    y = "Proportion of Responses"
  ) +
  scale_fill_manual(values = c(
    "Wrong" = "#ef8a62",
    "Right" = "#67a9cf",
    "Don't know" = "gray"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Memiringkan label sumbu x agar lebih mudah dibaca
```

**Jawaban:** Visualisasi ini menampilkan perbandingan proporsional, bukan jumlah absolut, sehingga lebih mudah melihat perbedaan proporsi jawaban antara region tanpa terpengaruh jumlah responden total.

### Exercise 3 - Comparing proportions across bars
Recreate the same visualisation from the previous exercise, this time dodging the bars for opinion proportions for each region, rather than faceting by region and then improve the legend. How is the story this visualisation telling different than the story the previous plot tells?

```{r}
library(tidyverse)
library(scales)

brexit <- read_csv("C:/Users/FYRA/Downloads/brexit.csv")

# menghitung proporsi masing-masing opini dalam setiap region
brexit_prop <- brexit %>%
  group_by(region, opinion) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(proportion = count / sum(count))

# bikin plot dengan dodge bars
ggplot(brexit_prop, aes(x = region, y = proportion, fill = opinion)) +
  geom_col(position = "dodge") +
  labs(
    title = "Proportion of Opinions on Brexit by Region (Dodge Bars)",
    subtitle = "YouGov Survey Results, 2-3 September 2019",
    x = "Region",
    y = "Proportion",
    fill = "Opinion"
  ) +
  scale_y_continuous(labels = percent) + # Ubah skala Y menjadi persen
  scale_fill_manual(values = c(
    "Wrong" = "#ef8a62",
    "Right" = "#67a9cf",
    "Don't know" = "gray"
  )) +
  theme_minimal()
```

**Jawaban:** Dengan menggunakan dodge bars, kita bisa lebih mudah membandingkan kategori opini antar region dalam satu grafik tanpa harus melihat beberapa panel seperti sebelumnya. Ini membuat perbandingan antar region lebih langsung terlihat.
