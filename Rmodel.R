###name:RVirusBroadcast 
###author:hxj7(hxj5hxj5@126.com)  
###version:202002010  
###note:本程序是"VirusBroadcast (in Java)"的R版本  
###      VirusBroadcast (in Java) 项目链接：
###      https://github.com/KikiLetGo/VirusBroadcast/tree/master/src  

library(tibble)  
library(dplyr) 

########## 模拟参数 ########## 
ORIGINAL_COUNT <- 50     # 初始感染数量 
BROAD_RATE <- 0.8        # 传播率 
SHADOW_TIME <- 140       # 潜伏时间，14天为140 
HOSPITAL_RECEIVE_TIME <- 10   # 医院收治响应时间 
BED_COUNT <- 1000        # 医院床位 

MOVE_WISH_MU <- -0.99   # 流动意向平均值，建议调整范围：[-0.99,0.99]; 
#   -0.99 人群流动最慢速率，甚至完全控制疫情传播; 
#   0.99为人群流动最快速率, 可导致全城感染 

CITY_PERSON_SIZE <- 5000    # 城市总人口数量 

FATALITY_RATE <- 0.02       # 病死率，根据2月6日数据估算（病死数/确诊数）为0.02 
SHADOW_TIME_SIGMA <- 25     # 潜伏时间方差 
CURED_TIME <- 50            # 治愈时间均值，从入院开始计时 
CURED_SIGMA <- 10           # 治愈时间标准差 
DIE_TIME <- 300             # 死亡时间均值，30天，从发病（确诊）时开始计时 
DIE_SIGMA <- 50             # 死亡时间标准差 

CITY_WIDTH <- 700           # 城市大小即窗口边界，限制不允许出城 
CITY_HEIGHT <- 800 

MAX_TRY <- 300             # 最大模拟次数，300代表30天 

########## 生成人群点，用不同颜色代表不同健康状态。 ########## 
# 用正态分布刻画人群点的分布 
CITY_CENTERX <- 400         # x轴的mu值 
CITY_CENTERY <- 400 
PERSON_DIST_X_SIGMA <- 100  # x轴的sigma值 
PERSON_DIST_Y_SIGMA <- 100 

# 市民状态应该需要细分，虽然有的状态暂未纳入模拟，但是细分状态应该保留 
STATE_NORMAL <- 0            # 正常人，未感染的健康人 
STATE_SUSPECTED <- STATE_NORMAL + 1   # 有暴露感染风险 
STATE_SHADOW <- STATE_SUSPECTED + 1   # 潜伏期 
STATE_CONFIRMED <- STATE_SHADOW + 1   # 发病且已确诊为感染病人 
STATE_FREEZE <- STATE_CONFIRMED + 1   # 隔离治疗，禁止位移 
STATE_DEATH <- STATE_FREEZE + 1    # 病死者 
STATE_CURED <- STATE_DEATH + 1   # 治愈数量用于计算治愈出院后归还床位数量，该状态是否存续待定 

worldtime <- 0 
NTRY_PER_DAY <- 10   # 一天模拟几次 
getday <- function(t) (t - 1) %/% NTRY_PER_DAY + 1 

# 生成人群数据 
format_coord <- function(coord, boundary) { 
  if (coord < 0) return(runif(1, 0, 10)) 
  else if  (coord > boundary) return(runif(1, boundary - 10, boundary)) 
  else return(coord) 
} 
set.seed(123) 
people <- tibble( 
  id = 1:CITY_PERSON_SIZE, 
  x = sapply(rnorm(CITY_PERSON_SIZE, CITY_CENTERX, PERSON_DIST_X_SIGMA),  
             format_coord, boundary = CITY_WIDTH),    # (x, y) 为人群点坐标 
  y = sapply(rnorm(CITY_PERSON_SIZE, CITY_CENTERY, PERSON_DIST_Y_SIGMA),  
             format_coord, boundary = CITY_HEIGHT), 
  state = STATE_NORMAL,    # 健康状态 
  infected_time = 0,     # 感染时刻 
  confirmed_time = 0,    # 确诊时刻 
  freeze_time = 0,       # 隔离时刻 
  cured_moment = 0,      # 痊愈时刻，为0代表不确定 
  die_moment = 0         # 死亡时刻，为0代表未确定，-1代表不会病死 
) %>% 
  mutate(tx = rnorm(CITY_PERSON_SIZE, x, PERSON_DIST_X_SIGMA),  # target x 
         ty = rnorm(CITY_PERSON_SIZE, y, PERSON_DIST_Y_SIGMA), 
         has_target = T, is_arrived = F) 

# 随机选择初始感染者 
peop_id <- sample(people$id, ORIGINAL_COUNT) 
people$state[peop_id] <- STATE_SHADOW 
people$infected_time[peop_id] <- worldtime 
people$confirmed_time[peop_id] <- worldtime +  
  max(rnorm(length(peop_id), SHADOW_TIME / 2, SHADOW_TIME_SIGMA), 0) 

########## 生成床位点 ########## 
HOSPITAL_X <- 720   # 第一张床位的x坐标 
HOSPITAL_Y <- 80    # 第一张床位的y坐标 
NBED_PER_COLUMN <- 100   # 医院每一列有多少张床位 
BED_ROW_SPACE <- 6       # 一行中床位的间距 
BED_COLUMN_SPACE <- 6    # 一列中床位的间距 

bed_ncolumn <- ceiling(BED_COUNT / NBED_PER_COLUMN) 
hosp_beds <- tibble(id = 1, x = 0, y = 0, is_empty = T, state = STATE_NORMAL) %>%  
  slice(-1) 
if (BED_COUNT > 0) { 
  hosp_beds <- tibble( 
    id = 1:BED_COUNT, 
    x = HOSPITAL_X + rep(((1:bed_ncolumn) - 1) * BED_ROW_SPACE, 
                         each = NBED_PER_COLUMN)[1:BED_COUNT],
    y = HOSPITAL_Y + 10 - BED_COLUMN_SPACE + 
      rep((1:NBED_PER_COLUMN) * BED_COLUMN_SPACE, bed_ncolumn)[1:BED_COUNT],
    is_empty = T,
    person_id = 0       # 占用床位的患者的序号，床位为空时为0
  )
}

########## 准备画图的数据 ##########
npeople_total <- CITY_PERSON_SIZE
npeople_shadow <- ORIGINAL_COUNT
npeople_confirmed <- npeople_freeze <- npeople_cured <- npeople_death <- 0
nbed_need <- 0

########## 画出初始数据 ##########
# 设置画图参数
person_color <- data.frame(   # 不同健康状态的颜色不同
  label = c("健康", "潜伏", "确诊", "隔离", "治愈", "死亡"),
  state = c(STATE_NORMAL, STATE_SHADOW, STATE_CONFIRMED, STATE_FREEZE, 
            STATE_CURED, STATE_DEATH),
  color = c(
    "lightgreen",   # 健康
    "#EEEE00",      # 潜伏期
    "red",          # 确诊
    "#FFC0CB",      # 隔离
    "green",        # 治愈
    "black"         # 死亡
  ), stringsAsFactors = F
)
bed_color <- data.frame(  
  is_empty = c(T, F), color = c("#F8F8FF", "#FFC0CB"), stringsAsFactors = F  
) 
x11(width = 5, height = 7, xpos = 0, ypos = 0, title = "人群变化模拟")
window_hist <- dev.cur()
x11(width = 7, height = 7, xpos = 460, ypos = 0, title = "疫情传播模拟")
window_scatter <- dev.cur()
max_plot_x <- ifelse(BED_COUNT > 0, max(hosp_beds$x), CITY_WIDTH) + 10

# 疫情传播模拟散点图
dev.set(window_scatter)
plot(x = people$x, y = people$y, cex = .8, pch = 20, xlab = NA, ylab = NA,
     xlim = c(5, max_plot_x), xaxt = "n", yaxt = "n", bty = "n", main = "疫情传播模拟", 
     sub = paste0("世界时间第 ", getday(worldtime), " 天"),
     col = (people %>% left_join(person_color, by = "state") %>%
              select(color))$color)
points(x = hosp_beds$x, y = hosp_beds$y, cex = .8, pch = 20,
       col = (hosp_beds %>% left_join(bed_color, by = "is_empty") %>%
                select(color))$color)
rect(HOSPITAL_X - BED_ROW_SPACE / 2, HOSPITAL_Y + 10 - BED_COLUMN_SPACE, 
     max(hosp_beds$x) + BED_ROW_SPACE / 2, max(hosp_beds$y + BED_COLUMN_SPACE))
legend(x = 150, y = -30, legend = person_color$label, col = person_color$color,
       pch = 20, horiz = T, bty = "n", xpd = T)

# 人群变化模拟条形图
dev.set(window_hist)
bp_data <- c(npeople_death, npeople_cured, nbed_need, npeople_freeze, 
             npeople_confirmed, npeople_shadow)
bp_color <- c("black", "green", "#FFE4E1", "#FFC0CB", "red", "#EEEE00")
bp_labels <- c("死亡", "治愈", "不足\n床位", "隔离", "累计\n确诊", "潜伏")
bp <- barplot(bp_data, horiz = T, border = NA, col = bp_color, 
              xlim = c(0, CITY_PERSON_SIZE * 1), main = "人群变化模拟", 
              sub = paste0("世界时间第 ", getday(worldtime), " 天"))
abline(v = BED_COUNT, col = "gray", lty = 3)
abline(v = CITY_PERSON_SIZE, col = "gray", lty = 1)
text(x = -350, y = bp, labels = bp_labels, xpd = T)
text(x = bp_data + CITY_PERSON_SIZE / 15, y = bp, xpd = T,
     labels = ifelse(bp_data > 0, bp_data, ""))
legend(x = 300, y = -.6, legend = c("总床位数", "城市总人口"), col = "gray",
       lty = c(3, 1), bty = "n", horiz = T, xpd = T)

Sys.sleep(5)  # 手动调整窗口大小

########## 更新人群数据 ##########
# 市民流动意愿以及移动位置参数174  MOVE_WISH_SIGMA <- 1
MOVE_DIST_SIGMA <- 50
SAFE_DIST <- 2   # 安全距离

worldtime <- worldtime + 1
get_min_dist <- function(person, peop) {  # 一个人和一群人之间的最小距离
  min(sqrt((person["x"] - peop$x) ^ 2 + (person["y"] - peop$y) ^ 2))
}
for (i in 1:MAX_TRY) {
  # 如果已经隔离或者死亡了，就不需要处理了
  #
  # 处理已经确诊的感染者（即患者）
  peop_id <- people$id[people$state == STATE_CONFIRMED & 
                         people$die_moment == 0]
  if ((npeop <- length(peop_id)) > 0) {
    people$die_moment[peop_id] <- ifelse(
      runif(npeop, 0, 1) < FATALITY_RATE,     # 用均匀分布模拟确诊患者是否会死亡
      people$confirmed_time + max(rnorm(npeop, DIE_TIME, DIE_SIGMA), 0),  # 发病后确定死亡时刻
      -1                                      # 逃过了死神的魔爪
    )
  }
  # 如果患者已经确诊，且（世界时刻-确诊时刻）大于医院响应时间，
  # 即医院准备好病床了，可以抬走了
  peop_id <- people$id[people$state == STATE_CONFIRMED & 
                         worldtime - people$confirmed_time >= HOSPITAL_RECEIVE_TIME]
  if ((npeop <- length(peop_id)) > 0) {
    if ((nbed_empty <- sum(hosp_beds$is_empty)) > 0) {  # 有空余床位
      nbed_use <- min(npeop, nbed_empty)
      bed_id <- hosp_beds$id[hosp_beds$is_empty][1:nbed_use]
      # 更新患者信息
      peop_id2 <- sample(peop_id, nbed_use)   # 这里是随机选择，理论上应该按症状轻重
      people$x[peop_id2] <- hosp_beds$x[bed_id]
      people$y[peop_id2] <- hosp_beds$y[bed_id]
      people$state[peop_id2] <- STATE_FREEZE
      people$freeze_time[peop_id2] <- worldtime
      # 更新床位信息
      hosp_beds$is_empty[bed_id] <- F
      hosp_beds$person_id[bed_id] <- peop_id2
    } 
  }
  # TODO 需要确定一个变量用于治愈时长。
  # 为了说明问题，暂时用一个正态分布模拟治愈时长并且假定治愈的人不会再被感染
  peop_id <- people$id[people$state == STATE_FREEZE & 
                         people$cured_moment == 0]
  if ((npeop <- length(peop_id)) > 0) { # 正态分布模拟治愈时间
    people$cured_moment[peop_id] <- people$freeze_time[peop_id] + 
      max(rnorm(npeop, CURED_TIME, CURED_SIGMA), 0)
  }
  peop_id <- people$id[people$state == STATE_FREEZE & people$cured_moment > 0 &
                         worldtime >= people$cured_moment]
  if ((npeop <- length(peop_id)) > 0) {  # 归还床位
    people$state[peop_id] <- STATE_CURED
    hosp_beds$is_empty[! hosp_beds$is_empty & hosp_beds$person_id %in% peop_id] <- T
    people$x[peop_id] <- sapply(rnorm(npeop, CITY_CENTERX, PERSON_DIST_X_SIGMA), 
                                format_coord, boundary = CITY_WIDTH)    # (x, y) 为人群点坐标
    people$y[peop_id] <- sapply(rnorm(npeop, CITY_CENTERY, PERSON_DIST_Y_SIGMA), 
                                format_coord, boundary = CITY_HEIGHT)
    people$tx[peop_id] <- rnorm(npeop, people$x[peop_id], PERSON_DIST_X_SIGMA)
    people$ty[peop_id] <- rnorm(npeop, people$y[peop_id], PERSON_DIST_Y_SIGMA)
    people$has_target[peop_id] <- T
    people$is_arrived[peop_id] <- F
  }
  # 处理病死者
  peop_id <- people$id[people$state %in% c(STATE_CONFIRMED, STATE_FREEZE) & 
                         worldtime >= people$die_moment & people$die_moment > 0]
  if (length(peop_id) > 0) {  # 归还床位
    people$state[peop_id] <- STATE_DEATH
    hosp_beds$is_empty[! hosp_beds$is_empty & hosp_beds$person_id %in% peop_id] <- T
  }
  # 处理发病的潜伏期感染者
  peop_id <- people$id[people$state == STATE_SHADOW &
                         worldtime >= people$confirmed_time]
  if ((npeop <- length(peop_id)) > 0) {
    people$state[peop_id] <- STATE_CONFIRMED   # 潜伏者发病
  }
  # 处理未隔离者的移动问题
  peop_id <- people$id[
    ! people$state %in% c(STATE_FREEZE, STATE_DEATH) & 
      rnorm(CITY_PERSON_SIZE, MOVE_WISH_MU, MOVE_WISH_SIGMA) > 0] # 流动意愿
  if ((npeop <- length(peop_id)) > 0) {  # 正态分布模拟要移动到的目标点
    pp_id <- peop_id[! people$has_target[peop_id] | people$is_arrived[peop_id]]
    if ((npp <- length(pp_id)) > 0) {
      people$tx[pp_id] <- rnorm(npp, people$tx[pp_id], PERSON_DIST_X_SIGMA)
      people$ty[pp_id] <- rnorm(npp, people$ty[pp_id], PERSON_DIST_Y_SIGMA)
      people$has_target[pp_id] <- T
      people$is_arrived[pp_id] <- F
    }
    # 计算运动位移262      dx <- people$tx[peop_id] - people$x[peop_id]
    dy <- people$ty[peop_id] - people$y[peop_id]
    move_dist <- sqrt(dx ^ 2 + dy ^ 2)
    people$is_arrived[peop_id][move_dist < 1] <- T  # 判断是否到达目标点266    pp_id <- peop_id[move_dist >= 1]
    if ((npp <- length(pp_id)) > 0) {
      udx <- sign(dx[move_dist >= 1])  # x轴运动方向269        udy <- sign(dy[move_dist >= 1])
      # 是否到了边界
      pid_x <- (1:npp)[people$x[pp_id] + udx < 0 | people$x[pp_id] + udx > CITY_WIDTH]
      pid_y <- (1:npp)[people$y[pp_id] + udy < 0 | people$y[pp_id] + udy > CITY_HEIGHT]
      # 更新到了边界的点的信息
      people$x[pp_id[pid_x]] <- people$x[pp_id[pid_x]] - udx[pid_x]
      people$y[pp_id[pid_y]] <- people$y[pp_id[pid_y]] - udy[pid_y]
      people$has_target[unique(c(pp_id[pid_x], pp_id[pid_y]))] <- F
      # 更新没有到边界的点的信息278        people$x[pp_id[! pp_id %in% pid_x]] <- people$x[pp_id[! pp_id %in% pid_x]] + 
      udx[! pp_id %in% pid_x]
      people$y[pp_id[! pp_id %in% pid_y]] <- people$y[pp_id[! pp_id %in% pid_y]] + 
        udy[! pp_id %in% pid_y]
    }
  }
  # 处理健康人被感染的问题
  # 通过一个随机幸运值和安全距离决定感染其他人286    normal_peop_id <- people$id[people$state == STATE_NORMAL]
  other_peop_id <- people$id[! people$state %in% c(STATE_NORMAL, STATE_CURED)]
  if (length(normal_peop_id) > 0) {
    normal_other_dist <- apply(people[normal_peop_id, ], 1, get_min_dist,
                               peop = people[other_peop_id, ])
    normal2other_id <- normal_peop_id[normal_other_dist < SAFE_DIST &
                                        runif(length(normal_peop_id), 0, 1) < BROAD_RATE]
    if ((n2other <- length(normal2other_id)) > 0) {
      people$state[normal2other_id] <- STATE_SHADOW
      people$infected_time[normal2other_id] <- worldtime
      people$confirmed_time[normal2other_id] <- worldtime + 
        max(rnorm(n2other, SHADOW_TIME / 2, SHADOW_TIME_SIGMA), 0)
    }
  }
  
  # 画出更新后的数据
  npeople_confirmed <- sum(people$state >= STATE_CONFIRMED)
  npeople_death <- sum(people$state == STATE_DEATH)
  npeople_freeze <- sum(people$state == STATE_FREEZE)
  npeople_shadow <- sum(people$state == STATE_SHADOW)
  npeople_cured <- sum(people$state == STATE_CURED)
  nbed_need <- npeople_confirmed - npeople_cured - npeople_death - BED_COUNT
  nbed_need <- ifelse(nbed_need > 0, nbed_need, 0)  # 不足病床数
  # 疫情传播模拟散点图
  dev.set(window_scatter)
  plot(x = people$x, y = people$y, cex = .8, pch = 20, xlab = NA, ylab = NA,
       xlim = c(5, max_plot_x), xaxt = "n", yaxt = "n", bty = "n", main = "疫情传播模拟", 
       sub = paste0("世界时间第 ", getday(worldtime), " 天"),
       col = (people %>% left_join(person_color, by = "state") %>%
                select(color))$color)
  points(x = hosp_beds$x, y = hosp_beds$y, cex = .8, pch = 20,
         col = (hosp_beds %>% left_join(bed_color, by = "is_empty") %>%
                  select(color))$color)
  rect(HOSPITAL_X - BED_ROW_SPACE / 2, HOSPITAL_Y + 10 - BED_COLUMN_SPACE, 
       max(hosp_beds$x) + BED_ROW_SPACE / 2, max(hosp_beds$y + BED_COLUMN_SPACE))
  legend(x = 150, y = -30, legend = person_color$label, col = person_color$color,
         pch = 20, horiz = T, bty = "n", xpd = T)
  # 人群变化模拟条形图
  dev.set(window_hist)
  bp_data <- c(npeople_death, npeople_cured, nbed_need, npeople_freeze, 
               npeople_confirmed, npeople_shadow)
  bp <- barplot(bp_data, horiz = T, border = NA, col = bp_color, 
                xlim = c(0, CITY_PERSON_SIZE * 1), main = "人群变化模拟", 
                sub = paste0("世界时间第 ", getday(worldtime), " 天"))
  abline(v = BED_COUNT, col = "gray", lty = 3)
  abline(v = CITY_PERSON_SIZE, col = "gray", lty = 1)
  text(x = -350, y = bp, labels = bp_labels, xpd = T)
  text(x = bp_data + CITY_PERSON_SIZE / 15, y = bp, xpd = T,
       labels = ifelse(bp_data > 0, bp_data, ""))
  legend(x = 300, y = -.6, legend = c("总床位数", "城市总人口"), col = "gray",
         lty = c(3, 1), bty = "n", horiz = T, xpd = T)
  
  
  # 更新世界时间
  worldtime <- worldtime + 1
}
————————————————
版权声明：本文为CSDN博主「生信了（公众号同名）」的原创文章，遵循 CC 4.0 BY-SA 版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/biocity/article/details/104330501