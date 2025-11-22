#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#read in the data


df <- read.csv("C:\\Users\\avund\\OneDrive\\Documents\\R Studio\\Project 1\\Electric_Vehicle_Population_Data.csv")

clean_df <- df |>
  rename_all(~ gsub("\\.", "_", .)) |>
  mutate(
    Base_MSRP = as.numeric(Base_MSRP),
    Tesla = ifelse(Make == "TESLA", "TESLA", "OTHER")
  )


# 1. What percentage of EVs in Washington are Teslas? - teslas/all vehicles*100

total_vehicles <- nrow(clean_df)
tesla_vehicles <- clean_df |> filter(Tesla == "TESLA") |> nrow()

market_share <- tesla_vehicles/total_vehicles *100
tesla_market_share <- round(tesla_vehicles/total_vehicles * 100, 1)

market_df <- data.frame(
  Group = c("TESLA", "OTHER"),
  Market_Share = c(tesla_market_share, 100 - tesla_market_share)
)

market_df <- market_df |>
mutate(
  Label = paste0(Market_Share, "%"),
  ypos = cumsum(Market_Share) - 0.5 * Market_Share
)


ggplot(market_df, aes(x = "", y = Market_Share, fill = Group)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(y = ypos, label = Label)) +
  labs(title = "Tesla Market Share in Washington (%)") +
  theme_void()


# 2. Top Tesla Models Selling in the area
# - How much are we making from these models?

make_model_sold <- clean_df |>
  filter(Tesla == "TESLA") |>
  group_by(Make,Model) |>
  summarize(Count_sold = n(), .groups = "drop") |>
  arrange(desc(Count_sold))

ggplot(make_model_sold, aes(x = reorder(Model,Count_sold), y = Count_sold)) +
  geom_col(fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Tesla Models Sold - All Time") +
  theme_minimal()



msrp_by_year <- clean_df |>
  filter(Base_MSRP > 0) |>
  filter(Tesla == "TESLA") |>
  group_by(Model_Year, Make, Model) |>
  summarize(min(Base_MSRP), .groups = "drop") |>
  arrange(Model_Year, Make, Model)

#There weren't good base msrp in this dataset - supplementing with online data

df_tesla_prices <- read.csv("C:\\Users\\avund\\OneDrive\\Documents\\R Studio\\Project 1\\Tesla_Current_Base_Prices.csv")

df_tesla_prices <- df_tesla_prices |>
 mutate(
   Model = toupper(Model)
 )

Top_Tesla_Models_Priced <- make_model_sold |>
  left_join(df_tesla_prices, by = "Model") |>
  mutate(
   Estimated_Revenue = as.numeric(Count_sold) * as.numeric(Base_Price_USD)
  )

# Final Estimates for lifetime sales

Top_Tesla_Models_Priced <- Top_Tesla_Models_Priced |>
  filter(!is.na(Estimated_Revenue)) |>
  group_by(Model) |>
  slice_min(Base_Price_USD) |>
  ungroup()

#Next Visualization
ggplot(Top_Tesla_Models_Priced, aes(x = reorder(Model, Estimated_Revenue), y = Estimated_Revenue)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Estimated Revenue Per Model") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal()

#ADD IN

tesla_sold_by_year <- clean_df |>
  filter(Tesla == "TESLA") |>
  group_by(Model_Year) |>
  summarise(Count = n()) |>
  arrange(Model_Year)

ggplot(tesla_sold_by_year, aes( x = Model_Year, y = Count))+
  geom_line(color = "dodgerblue", size = 1.1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Tesla Vehicles Sold by Year") +
  theme_minimal()



# 3 Tesla vs Competitors - Range and MSRP
# - AVG and Median

#AVG Range
AVG_Range_For_EVs <- clean_df |>
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) |>
  group_by(Tesla) |>
  summarize(AVG_Range = mean(Electric_Range),
            AVG_MSRP = mean(Base_MSRP))

#Median Range
Median_Range_For_EVs <- clean_df |>
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) |>
  group_by(Tesla) |>
  summarize(Median_Range = median(Electric_Range),
                                  Median_MSRP = median(Base_MSRP))

#MSRP data is fully populated and may skew results


#Visualizations for MSRP and Range AVG

#Range
ggplot(AVG_Range_For_EVs, aes(x = Tesla, y = AVG_Range, fill = Tesla)) +
  geom_col() +
  labs(title = "AVG Electric Range (BEV)") +
  theme_minimal()

#MSRP
ggplot(AVG_Range_For_EVs, aes(x = Tesla, y = AVG_MSRP, fill = Tesla)) +
  geom_col() +
  labs(title = "AVG MSRP (BEV)") +
  theme_minimal()





#4 PHEV vs BEV Trends


PHEV_vs_BEV <- clean_df |>
  group_by(Model_Year, Electric_Vehicle_Type) |>
  summarize(Count = n(), .groups = "drop")


#5 Top Electric Utilities in Washington for Tesla


Utility_Count_for_Teslas  <- clean_df |>
  filter(Tesla == "TESLA") |>
  group_by(Electric_Utility) |>
  summarize(Count = n(), .groups = "drop") |>
  arrange(desc(Count)) |>
  head(5)



















