library(dbplyr)
library(dplyr)
library(ggplot2)
library(readxl)
install.packages("maps")
library(maps)
world_map <- map_data("world")

df <- read_excel("C:/Users/Dell/Projects/DataAnalyst/GPM-2-R/BT nhóm/ICT.xlsx")
View(df)

# Tìm các giá trị NA trong dataframe
na_values <- is.na(df)
# Tổng số giá trị NA trong mỗi cột
na_count <- colSums(na_values)
# Hiển thị số lượng giá trị NA trong mỗi cột
print(na_count)
#Xác định dòng có giá trị NA
df$has_na <- ifelse(rowSums(is.na(df)) > 0, 1, 0)
str(df)
df <- df %>%
  mutate_at(vars(where(is.character)), ~if_else(is.na(.), "Unknown", .))


# Tính giá trị trung bình của biến "Value" cho mỗi quốc gia
df_avg <- df %>%
  group_by(Country) %>%
  summarize(avg_value = mean(Value, na.rm = TRUE))
# Vẽ biểu đồ cột so sánh giá trị trung bình của biến "Value" cho các quốc gia
ggplot(df_avg, aes(x = Country, y = avg_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Biểu đồ cột: Giá trị trung bình của biến Value theo quốc gia",
       x = "Quốc gia",
       y = "Giá trị trung bình") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lọc dữ liệu cho quốc gia New Zealand
df_nz <- df %>%
  filter(Country == "New Zealand")
# Vẽ biểu đồ cột cho quốc gia New Zealand với màu pastel blue
ggplot(df_nz, aes(x = TIME, y = Value)) +
  geom_bar(stat = "identity", fill = "#B8E6B8") +  
  labs(title = "Biểu đồ cột: Chỉ số ICT của New Zealand",
       x = "Năm",
       y = "Giá trị") +
  theme_minimal()

# Lọc dữ liệu cho quốc gia United States
df_us <- df %>%
  filter(Country == "United States")
# Vẽ biểu đồ điểm cho quốc gia United States
ggplot(df_us, aes(x = TIME, y = Value)) +
  geom_point(color = "#D7A9E3") +
  labs(title = "Biểu đồ điểm: Chỉ số ICT của United States",
       x = "Năm",
       y = "Giá trị") +
  theme_minimal()

# Lọc dữ liệu cho các quốc gia và các giai đoạn khác nhau
df_selected <- df %>%
  filter(Country %in% c("Australia", "Brazil", "Colombia", "Finland", "New Zealand"),
         TIME < 2019 | (TIME >= 2019 & TIME <= 2022) | TIME > 2022)
# Tính giá trị trung bình cho mỗi quốc gia và mỗi giai đoạn
avg_values <- df_selected %>%
  group_by(Country, GiaiDoan = case_when(
    TIME < 2019 ~ "Trước năm 2019",
    TIME >= 2019 & TIME <= 2022 ~ "Trong giai đoạn Covid-19",
    TIME > 2022 ~ "Sau năm 2022"
  )) %>%
  summarize(avg_value = mean(Value, na.rm = TRUE))
# Vẽ biểu đồ cột so sánh giá trị của các quốc gia trong 3 giai đoạn
ggplot(avg_values, aes(x = Country, y = avg_value, fill = GiaiDoan)) +
  geom_col(position = "dodge", color = "black") +
  labs(title = "So sánh chỉ số ICT của các quốc gia trong 3 giai đoạn",
       x = "Quốc gia",
       y = "Giá trị trung bình",
       fill = "Giai đoạn") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Viewer
# Lọc dữ liệu cho các quốc gia và các giai đoạn khác nhau
df_selected <- df %>%
  filter(Country %in% c("Australia", "Brazil", "Colombia", "Finland", "New Zealand"),
         TIME < 2019 | (TIME >= 2019 & TIME <= 2022) | TIME > 2022)

# Tính giá trị trung bình cho mỗi quốc gia và mỗi giai đoạn
avg_values <- df_selected %>%
  group_by(Country, GiaiDoan = case_when(
    TIME < 2019 ~ "Trước năm 2019",
    TIME >= 2019 & TIME <= 2022 ~ "Trong giai đoạn Covid-19",
    TIME > 2022 ~ "Sau năm 2022"
  )) %>%
  summarize(avg_value = mean(Value, na.rm = TRUE))
# Vẽ biểu đồ cột so sánh giá trị của các quốc gia trong 3 giai đoạn
p <- ggplot(avg_values, aes(x = Country, y = avg_value, fill = GiaiDoan)) +
  geom_col(position = "dodge", color = "black") +
  labs(title = "So sánh chỉ số ICT của các quốc gia trong 3 giai đoạn",
       x = "Quốc gia",
       y = "Giá trị trung bình",
       fill = "Giai đoạn") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p, tooltip = c("x", "y"))

library(maps)
library(ggplot2)
library(plotly)
# Lấy dữ liệu về biên giới các quốc gia
world_map <- map_data("world")
# Tạo biểu đồ bản đồ thế giới
world_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void()
# Chuyển đổi biểu đồ sang dạng tương tác bằng plotly và hiển thị tên quốc gia
world_plot <- ggplotly(world_plot, tooltip = c("text")) %>%
  style(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
# Hiển thị biểu đồ
print(world_plot)


# Tính giá trị trung bình của chỉ số ICT cho mỗi quốc gia
avg_ict <- df %>%
  group_by(Country) %>%
  summarize(avg_ict_value = mean(Value, na.rm = TRUE))
# Sắp xếp lại dữ liệu theo giá trị trung bình giảm dần
avg_ict <- avg_ict %>%
  arrange(desc(avg_ict_value))
# Vẽ biểu đồ cột thể hiện giá trị trung bình của chỉ số ICT cho các quốc gia
ggplot(avg_ict, aes(x = reorder(Country, avg_ict_value), y = avg_ict_value)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Tầm quan trọng của ICT ở mỗi quốc gia",
       x = "Quốc gia",
       y = "Giá trị trung bình") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


