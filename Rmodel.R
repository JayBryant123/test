###name:RVirusBroadcast 
###author:hxj7(hxj5hxj5@126.com)  
###version:202002010  
###note:��������"VirusBroadcast (in Java)"��R�汾  
###      VirusBroadcast (in Java) ��Ŀ���ӣ�
###      https://github.com/KikiLetGo/VirusBroadcast/tree/master/src  

library(tibble)  
library(dplyr) 

########## ģ����� ########## 
ORIGINAL_COUNT <- 50     # ��ʼ��Ⱦ���� 
BROAD_RATE <- 0.8        # ������ 
SHADOW_TIME <- 140       # Ǳ��ʱ�䣬14��Ϊ140 
HOSPITAL_RECEIVE_TIME <- 10   # ҽԺ������Ӧʱ�� 
BED_COUNT <- 1000        # ҽԺ��λ 

MOVE_WISH_MU <- -0.99   # ��������ƽ��ֵ�����������Χ��[-0.99,0.99]; 
#   -0.99 ��Ⱥ�����������ʣ�������ȫ�������鴫��; 
#   0.99Ϊ��Ⱥ�����������, �ɵ���ȫ�Ǹ�Ⱦ 

CITY_PERSON_SIZE <- 5000    # �������˿����� 

FATALITY_RATE <- 0.02       # �����ʣ�����2��6�����ݹ��㣨������/ȷ������Ϊ0.02 
SHADOW_TIME_SIGMA <- 25     # Ǳ��ʱ�䷽�� 
CURED_TIME <- 50            # ����ʱ���ֵ������Ժ��ʼ��ʱ 
CURED_SIGMA <- 10           # ����ʱ���׼�� 
DIE_TIME <- 300             # ����ʱ���ֵ��30�죬�ӷ�����ȷ�ʱ��ʼ��ʱ 
DIE_SIGMA <- 50             # ����ʱ���׼�� 

CITY_WIDTH <- 700           # ���д�С�����ڱ߽磬���Ʋ��������� 
CITY_HEIGHT <- 800 

MAX_TRY <- 300             # ���ģ�������300����30�� 

########## ������Ⱥ�㣬�ò�ͬ��ɫ������ͬ����״̬�� ########## 
# ����̬�ֲ��̻���Ⱥ��ķֲ� 
CITY_CENTERX <- 400         # x���muֵ 
CITY_CENTERY <- 400 
PERSON_DIST_X_SIGMA <- 100  # x���sigmaֵ 
PERSON_DIST_Y_SIGMA <- 100 

# ����״̬Ӧ����Ҫϸ�֣���Ȼ�е�״̬��δ����ģ�⣬����ϸ��״̬Ӧ�ñ��� 
STATE_NORMAL <- 0            # �����ˣ�δ��Ⱦ�Ľ����� 
STATE_SUSPECTED <- STATE_NORMAL + 1   # �б�¶��Ⱦ���� 
STATE_SHADOW <- STATE_SUSPECTED + 1   # Ǳ���� 
STATE_CONFIRMED <- STATE_SHADOW + 1   # ��������ȷ��Ϊ��Ⱦ���� 
STATE_FREEZE <- STATE_CONFIRMED + 1   # �������ƣ���ֹλ�� 
STATE_DEATH <- STATE_FREEZE + 1    # ������ 
STATE_CURED <- STATE_DEATH + 1   # �����������ڼ���������Ժ��黹��λ��������״̬�Ƿ�������� 

worldtime <- 0 
NTRY_PER_DAY <- 10   # һ��ģ�⼸�� 
getday <- function(t) (t - 1) %/% NTRY_PER_DAY + 1 

# ������Ⱥ���� 
format_coord <- function(coord, boundary) { 
  if (coord < 0) return(runif(1, 0, 10)) 
  else if  (coord > boundary) return(runif(1, boundary - 10, boundary)) 
  else return(coord) 
} 
set.seed(123) 
people <- tibble( 
  id = 1:CITY_PERSON_SIZE, 
  x = sapply(rnorm(CITY_PERSON_SIZE, CITY_CENTERX, PERSON_DIST_X_SIGMA),  
             format_coord, boundary = CITY_WIDTH),    # (x, y) Ϊ��Ⱥ������ 
  y = sapply(rnorm(CITY_PERSON_SIZE, CITY_CENTERY, PERSON_DIST_Y_SIGMA),  
             format_coord, boundary = CITY_HEIGHT), 
  state = STATE_NORMAL,    # ����״̬ 
  infected_time = 0,     # ��Ⱦʱ�� 
  confirmed_time = 0,    # ȷ��ʱ�� 
  freeze_time = 0,       # ����ʱ�� 
  cured_moment = 0,      # Ȭ��ʱ�̣�Ϊ0������ȷ�� 
  die_moment = 0         # ����ʱ�̣�Ϊ0����δȷ����-1�������Ს�� 
) %>% 
  mutate(tx = rnorm(CITY_PERSON_SIZE, x, PERSON_DIST_X_SIGMA),  # target x 
         ty = rnorm(CITY_PERSON_SIZE, y, PERSON_DIST_Y_SIGMA), 
         has_target = T, is_arrived = F) 

# ���ѡ���ʼ��Ⱦ�� 
peop_id <- sample(people$id, ORIGINAL_COUNT) 
people$state[peop_id] <- STATE_SHADOW 
people$infected_time[peop_id] <- worldtime 
people$confirmed_time[peop_id] <- worldtime +  
  max(rnorm(length(peop_id), SHADOW_TIME / 2, SHADOW_TIME_SIGMA), 0) 

########## ���ɴ�λ�� ########## 
HOSPITAL_X <- 720   # ��һ�Ŵ�λ��x���� 
HOSPITAL_Y <- 80    # ��һ�Ŵ�λ��y���� 
NBED_PER_COLUMN <- 100   # ҽԺÿһ���ж����Ŵ�λ 
BED_ROW_SPACE <- 6       # һ���д�λ�ļ�� 
BED_COLUMN_SPACE <- 6    # һ���д�λ�ļ�� 

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
    person_id = 0       # ռ�ô�λ�Ļ��ߵ���ţ���λΪ��ʱΪ0
  )
}

########## ׼����ͼ������ ##########
npeople_total <- CITY_PERSON_SIZE
npeople_shadow <- ORIGINAL_COUNT
npeople_confirmed <- npeople_freeze <- npeople_cured <- npeople_death <- 0
nbed_need <- 0

########## ������ʼ���� ##########
# ���û�ͼ����
person_color <- data.frame(   # ��ͬ����״̬����ɫ��ͬ
  label = c("����", "Ǳ��", "ȷ��", "����", "����", "����"),
  state = c(STATE_NORMAL, STATE_SHADOW, STATE_CONFIRMED, STATE_FREEZE, 
            STATE_CURED, STATE_DEATH),
  color = c(
    "lightgreen",   # ����
    "#EEEE00",      # Ǳ����
    "red",          # ȷ��
    "#FFC0CB",      # ����
    "green",        # ����
    "black"         # ����
  ), stringsAsFactors = F
)
bed_color <- data.frame(  
  is_empty = c(T, F), color = c("#F8F8FF", "#FFC0CB"), stringsAsFactors = F  
) 
x11(width = 5, height = 7, xpos = 0, ypos = 0, title = "��Ⱥ�仯ģ��")
window_hist <- dev.cur()
x11(width = 7, height = 7, xpos = 460, ypos = 0, title = "���鴫��ģ��")
window_scatter <- dev.cur()
max_plot_x <- ifelse(BED_COUNT > 0, max(hosp_beds$x), CITY_WIDTH) + 10

# ���鴫��ģ��ɢ��ͼ
dev.set(window_scatter)
plot(x = people$x, y = people$y, cex = .8, pch = 20, xlab = NA, ylab = NA,
     xlim = c(5, max_plot_x), xaxt = "n", yaxt = "n", bty = "n", main = "���鴫��ģ��", 
     sub = paste0("����ʱ��� ", getday(worldtime), " ��"),
     col = (people %>% left_join(person_color, by = "state") %>%
              select(color))$color)
points(x = hosp_beds$x, y = hosp_beds$y, cex = .8, pch = 20,
       col = (hosp_beds %>% left_join(bed_color, by = "is_empty") %>%
                select(color))$color)
rect(HOSPITAL_X - BED_ROW_SPACE / 2, HOSPITAL_Y + 10 - BED_COLUMN_SPACE, 
     max(hosp_beds$x) + BED_ROW_SPACE / 2, max(hosp_beds$y + BED_COLUMN_SPACE))
legend(x = 150, y = -30, legend = person_color$label, col = person_color$color,
       pch = 20, horiz = T, bty = "n", xpd = T)

# ��Ⱥ�仯ģ������ͼ
dev.set(window_hist)
bp_data <- c(npeople_death, npeople_cured, nbed_need, npeople_freeze, 
             npeople_confirmed, npeople_shadow)
bp_color <- c("black", "green", "#FFE4E1", "#FFC0CB", "red", "#EEEE00")
bp_labels <- c("����", "����", "����\n��λ", "����", "�ۼ�\nȷ��", "Ǳ��")
bp <- barplot(bp_data, horiz = T, border = NA, col = bp_color, 
              xlim = c(0, CITY_PERSON_SIZE * 1), main = "��Ⱥ�仯ģ��", 
              sub = paste0("����ʱ��� ", getday(worldtime), " ��"))
abline(v = BED_COUNT, col = "gray", lty = 3)
abline(v = CITY_PERSON_SIZE, col = "gray", lty = 1)
text(x = -350, y = bp, labels = bp_labels, xpd = T)
text(x = bp_data + CITY_PERSON_SIZE / 15, y = bp, xpd = T,
     labels = ifelse(bp_data > 0, bp_data, ""))
legend(x = 300, y = -.6, legend = c("�ܴ�λ��", "�������˿�"), col = "gray",
       lty = c(3, 1), bty = "n", horiz = T, xpd = T)

Sys.sleep(5)  # �ֶ��������ڴ�С

########## ������Ⱥ���� ##########
# ����������Ը�Լ��ƶ�λ�ò���174  MOVE_WISH_SIGMA <- 1
MOVE_DIST_SIGMA <- 50
SAFE_DIST <- 2   # ��ȫ����

worldtime <- worldtime + 1
get_min_dist <- function(person, peop) {  # һ���˺�һȺ��֮�����С����
  min(sqrt((person["x"] - peop$x) ^ 2 + (person["y"] - peop$y) ^ 2))
}
for (i in 1:MAX_TRY) {
  # ����Ѿ�������������ˣ��Ͳ���Ҫ������
  #
  # �����Ѿ�ȷ��ĸ�Ⱦ�ߣ������ߣ�
  peop_id <- people$id[people$state == STATE_CONFIRMED & 
                         people$die_moment == 0]
  if ((npeop <- length(peop_id)) > 0) {
    people$die_moment[peop_id] <- ifelse(
      runif(npeop, 0, 1) < FATALITY_RATE,     # �þ��ȷֲ�ģ��ȷ�ﻼ���Ƿ������
      people$confirmed_time + max(rnorm(npeop, DIE_TIME, DIE_SIGMA), 0),  # ������ȷ������ʱ��
      -1                                      # �ӹ��������ħצ
    )
  }
  # ��������Ѿ�ȷ��ң�����ʱ��-ȷ��ʱ�̣�����ҽԺ��Ӧʱ�䣬
  # ��ҽԺ׼���ò����ˣ�����̧����
  peop_id <- people$id[people$state == STATE_CONFIRMED & 
                         worldtime - people$confirmed_time >= HOSPITAL_RECEIVE_TIME]
  if ((npeop <- length(peop_id)) > 0) {
    if ((nbed_empty <- sum(hosp_beds$is_empty)) > 0) {  # �п��ലλ
      nbed_use <- min(npeop, nbed_empty)
      bed_id <- hosp_beds$id[hosp_beds$is_empty][1:nbed_use]
      # ���»�����Ϣ
      peop_id2 <- sample(peop_id, nbed_use)   # ���������ѡ��������Ӧ�ð�֢״����
      people$x[peop_id2] <- hosp_beds$x[bed_id]
      people$y[peop_id2] <- hosp_beds$y[bed_id]
      people$state[peop_id2] <- STATE_FREEZE
      people$freeze_time[peop_id2] <- worldtime
      # ���´�λ��Ϣ
      hosp_beds$is_empty[bed_id] <- F
      hosp_beds$person_id[bed_id] <- peop_id2
    } 
  }
  # TODO ��Ҫȷ��һ��������������ʱ����
  # Ϊ��˵�����⣬��ʱ��һ����̬�ֲ�ģ������ʱ�����Ҽٶ��������˲����ٱ���Ⱦ
  peop_id <- people$id[people$state == STATE_FREEZE & 
                         people$cured_moment == 0]
  if ((npeop <- length(peop_id)) > 0) { # ��̬�ֲ�ģ������ʱ��
    people$cured_moment[peop_id] <- people$freeze_time[peop_id] + 
      max(rnorm(npeop, CURED_TIME, CURED_SIGMA), 0)
  }
  peop_id <- people$id[people$state == STATE_FREEZE & people$cured_moment > 0 &
                         worldtime >= people$cured_moment]
  if ((npeop <- length(peop_id)) > 0) {  # �黹��λ
    people$state[peop_id] <- STATE_CURED
    hosp_beds$is_empty[! hosp_beds$is_empty & hosp_beds$person_id %in% peop_id] <- T
    people$x[peop_id] <- sapply(rnorm(npeop, CITY_CENTERX, PERSON_DIST_X_SIGMA), 
                                format_coord, boundary = CITY_WIDTH)    # (x, y) Ϊ��Ⱥ������
    people$y[peop_id] <- sapply(rnorm(npeop, CITY_CENTERY, PERSON_DIST_Y_SIGMA), 
                                format_coord, boundary = CITY_HEIGHT)
    people$tx[peop_id] <- rnorm(npeop, people$x[peop_id], PERSON_DIST_X_SIGMA)
    people$ty[peop_id] <- rnorm(npeop, people$y[peop_id], PERSON_DIST_Y_SIGMA)
    people$has_target[peop_id] <- T
    people$is_arrived[peop_id] <- F
  }
  # ����������
  peop_id <- people$id[people$state %in% c(STATE_CONFIRMED, STATE_FREEZE) & 
                         worldtime >= people$die_moment & people$die_moment > 0]
  if (length(peop_id) > 0) {  # �黹��λ
    people$state[peop_id] <- STATE_DEATH
    hosp_beds$is_empty[! hosp_beds$is_empty & hosp_beds$person_id %in% peop_id] <- T
  }
  # ����������Ǳ���ڸ�Ⱦ��
  peop_id <- people$id[people$state == STATE_SHADOW &
                         worldtime >= people$confirmed_time]
  if ((npeop <- length(peop_id)) > 0) {
    people$state[peop_id] <- STATE_CONFIRMED   # Ǳ���߷���
  }
  # ����δ�����ߵ��ƶ�����
  peop_id <- people$id[
    ! people$state %in% c(STATE_FREEZE, STATE_DEATH) & 
      rnorm(CITY_PERSON_SIZE, MOVE_WISH_MU, MOVE_WISH_SIGMA) > 0] # ������Ը
  if ((npeop <- length(peop_id)) > 0) {  # ��̬�ֲ�ģ��Ҫ�ƶ�����Ŀ���
    pp_id <- peop_id[! people$has_target[peop_id] | people$is_arrived[peop_id]]
    if ((npp <- length(pp_id)) > 0) {
      people$tx[pp_id] <- rnorm(npp, people$tx[pp_id], PERSON_DIST_X_SIGMA)
      people$ty[pp_id] <- rnorm(npp, people$ty[pp_id], PERSON_DIST_Y_SIGMA)
      people$has_target[pp_id] <- T
      people$is_arrived[pp_id] <- F
    }
    # �����˶�λ��262      dx <- people$tx[peop_id] - people$x[peop_id]
    dy <- people$ty[peop_id] - people$y[peop_id]
    move_dist <- sqrt(dx ^ 2 + dy ^ 2)
    people$is_arrived[peop_id][move_dist < 1] <- T  # �ж��Ƿ񵽴�Ŀ���266    pp_id <- peop_id[move_dist >= 1]
    if ((npp <- length(pp_id)) > 0) {
      udx <- sign(dx[move_dist >= 1])  # x���˶�����269        udy <- sign(dy[move_dist >= 1])
      # �Ƿ��˱߽�
      pid_x <- (1:npp)[people$x[pp_id] + udx < 0 | people$x[pp_id] + udx > CITY_WIDTH]
      pid_y <- (1:npp)[people$y[pp_id] + udy < 0 | people$y[pp_id] + udy > CITY_HEIGHT]
      # ���µ��˱߽�ĵ����Ϣ
      people$x[pp_id[pid_x]] <- people$x[pp_id[pid_x]] - udx[pid_x]
      people$y[pp_id[pid_y]] <- people$y[pp_id[pid_y]] - udy[pid_y]
      people$has_target[unique(c(pp_id[pid_x], pp_id[pid_y]))] <- F
      # ����û�е��߽�ĵ����Ϣ278        people$x[pp_id[! pp_id %in% pid_x]] <- people$x[pp_id[! pp_id %in% pid_x]] + 
      udx[! pp_id %in% pid_x]
      people$y[pp_id[! pp_id %in% pid_y]] <- people$y[pp_id[! pp_id %in% pid_y]] + 
        udy[! pp_id %in% pid_y]
    }
  }
  # ���������˱���Ⱦ������
  # ͨ��һ���������ֵ�Ͱ�ȫ���������Ⱦ������286    normal_peop_id <- people$id[people$state == STATE_NORMAL]
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
  
  # �������º������
  npeople_confirmed <- sum(people$state >= STATE_CONFIRMED)
  npeople_death <- sum(people$state == STATE_DEATH)
  npeople_freeze <- sum(people$state == STATE_FREEZE)
  npeople_shadow <- sum(people$state == STATE_SHADOW)
  npeople_cured <- sum(people$state == STATE_CURED)
  nbed_need <- npeople_confirmed - npeople_cured - npeople_death - BED_COUNT
  nbed_need <- ifelse(nbed_need > 0, nbed_need, 0)  # ���㲡����
  # ���鴫��ģ��ɢ��ͼ
  dev.set(window_scatter)
  plot(x = people$x, y = people$y, cex = .8, pch = 20, xlab = NA, ylab = NA,
       xlim = c(5, max_plot_x), xaxt = "n", yaxt = "n", bty = "n", main = "���鴫��ģ��", 
       sub = paste0("����ʱ��� ", getday(worldtime), " ��"),
       col = (people %>% left_join(person_color, by = "state") %>%
                select(color))$color)
  points(x = hosp_beds$x, y = hosp_beds$y, cex = .8, pch = 20,
         col = (hosp_beds %>% left_join(bed_color, by = "is_empty") %>%
                  select(color))$color)
  rect(HOSPITAL_X - BED_ROW_SPACE / 2, HOSPITAL_Y + 10 - BED_COLUMN_SPACE, 
       max(hosp_beds$x) + BED_ROW_SPACE / 2, max(hosp_beds$y + BED_COLUMN_SPACE))
  legend(x = 150, y = -30, legend = person_color$label, col = person_color$color,
         pch = 20, horiz = T, bty = "n", xpd = T)
  # ��Ⱥ�仯ģ������ͼ
  dev.set(window_hist)
  bp_data <- c(npeople_death, npeople_cured, nbed_need, npeople_freeze, 
               npeople_confirmed, npeople_shadow)
  bp <- barplot(bp_data, horiz = T, border = NA, col = bp_color, 
                xlim = c(0, CITY_PERSON_SIZE * 1), main = "��Ⱥ�仯ģ��", 
                sub = paste0("����ʱ��� ", getday(worldtime), " ��"))
  abline(v = BED_COUNT, col = "gray", lty = 3)
  abline(v = CITY_PERSON_SIZE, col = "gray", lty = 1)
  text(x = -350, y = bp, labels = bp_labels, xpd = T)
  text(x = bp_data + CITY_PERSON_SIZE / 15, y = bp, xpd = T,
       labels = ifelse(bp_data > 0, bp_data, ""))
  legend(x = 300, y = -.6, legend = c("�ܴ�λ��", "�������˿�"), col = "gray",
         lty = c(3, 1), bty = "n", horiz = T, xpd = T)
  
  
  # ��������ʱ��
  worldtime <- worldtime + 1
}
��������������������������������
��Ȩ����������ΪCSDN�����������ˣ����ں�ͬ��������ԭ�����£���ѭ CC 4.0 BY-SA ��ȨЭ�飬ת���븽��ԭ�ĳ������Ӽ���������
ԭ�����ӣ�https://blog.csdn.net/biocity/article/details/104330501