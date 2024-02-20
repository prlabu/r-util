
## PRELUDE ----
library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(jsonlite)
library(RColorBrewer)
library(lme4)
library(ggpubr)


PATH_ANALYSIS <- "/Volumes/Nexus4/DBS/groupanalyses/task-lombard/20210922-beh-lombard-effect-PLB"

# p <- r"(Y:\DBS\participants.tsv)"
# p <- gsub("\\\\", "/", p)

setwd(PATH_ANALYSIS)
PATH_DATA <- '/Volumes/Nexus4/DBS/derivatives'
TASK <- 'lombard'
SESSION <- 'intraop'

PATH_FIG = glue('{PATH_ANALYSIS}/fig')


subject_ids = c(1001:1020)


## HELPER FUNCTIONS ----
read_tsv_by_subject <- function(path_annot, subject_id) {
  df_new = read_tsv(path_annot)
  df_new$subject_id = subject_id
  df_new
}


SUBJECTS_META = read_tsv("/Volumes/Nexus4/DBS/participants.tsv") %>%
  mutate(target = str_remove(dbs_target, "_.*")) %>%
  relocate(target, .after=dbs_target)


# install.packages("ggpubr")


# COLORS <- fromJSON("../20230602-metadata-and-groupdata-PLB/lombard_colors.json") %>%
#   pivot_wider(names_from = name, values_from = hex)
# color_map_conditions = 
# c(COLORS$control, COLORS$lombard, COLORS$missing)
# color_map_targets = c(COLORS$STN, COLORS$GPi, COLORS$VIM)

COLORS <- read_tsv("../20230602-metadata-and-groupdata-PLB/lombard_colors.tsv")
color_map_conditions <- COLORS %>% filter(category == 'condition') 
color_map_targets <- COLORS %>% filter(category == 'target')
color_map_disease <- COLORS %>% filter(category == 'disease') 
# scale_color_manual(values = color_map_targets$hex, breaks=color_map_targets$name) +
  


theme_set(theme_grey(base_size = 18, axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))) 


subject_ids = c(1001:1020)


# A couple of helper functions
summarize_all_stats <- function(.data) {
  dplyr::summarise(.data, across(where(is.numeric), .fns = 
                                   list(mean = mean,
                                        stdev = sd,
                                        median = median,
                                        q25 = ~quantile(., 0.25),
                                        q75 = ~quantile(., 0.75),
                                        max = max,
                                        min = min,
                                        n = length)))
}

read_tsv_by_subject <- function(path_annot, subject_id) {
  df_new = read_tsv(path_annot)
  df_new$subject_id = subject_id
  df_new
}


## Load a bunch of tables ---- 
annot_name = 'electrodes'
electrodes <-
  tibble(subject_id_tmp=paste0('DM',subject_ids)) %>%
  mutate(path_annot=glue("{PATH_DER}/sub-{subject_id_tmp}/annot/sub-{subject_id_tmp}_electrodes.tsv")) %>%
  mutate(exists_annot=file.exists(path_annot)) %>%
  filter(exists_annot) %>%
  # separate_wider_delim(path_annot, ".", names = c("path_annot_base", "path_annot_ext")) %>%
  mutate(path_base = path_dir(path_annot), 
         path_fname = path_ext_remove(path_file(path_annot))) %>%
  mutate(t = map(pull(., path_annot), read_tsv)) %>%
  
  unnest(t) %>% # unnest the tibbles on each row
  # select(-subject_id, subject_id=subject_id_tmp) %>% # rename variables to avoid conflict in rowbinding
  rename(subject_id=subject_id_tmp) %>% 
  group_by(subject_id) %>%
  # filter() %>%
  group_walk(~ write_tsv(.x, paste0("~/Desktop/sub-", .y$subject_id, "_electrodes.tsv"))) 


# # load a bunch of tables--old
# annot_name = 'produced-sentences-acoustics'
# sentences <-
#   tibble(subject_id=paste0('DM',subject_ids)) %>%
#   mutate(path_annot=glue("{PATH_DATA}/sub-{subject_id}/annot/sub-{subject_id}_ses-{SESSION}_task-{TASK}_annot-{annot_name}.tsv")) %>%
#   mutate(exists_annot=file.exists(path_annot)) %>%
#   filter(exists_annot) 
#   # pull(path_annot) %>% 
# sentences <- map2(sentences$path_annot, sentences$path_annot, read_tsv_by_subject) %>%
#   list_rbind()
#   rename(any_of(c("subject_id" = "sub"))) %>%
#   mutate(sentence_on = onset, 
#          sentence_off = onset + duration, 
#          noise_type = as.factor(noise_type)) %>%
#   group_by(subject_id, run_id, trial_id, noise_type)
# sentences$noise_type <- recode(sentences$noise_type, '0' = "QUIET",  '1' = "LMBRD")



aa <-
  sentences %>%
  group_by(subject_id, run_id, noise_type) %>%
  arrange(intensity_praat, .by_group=TRUE) %>%
  mutate(intensity_praat_rank = row_number()) %>% 
  add_count() %>%
  mutate(intensity_praat_prct = intensity_praat_rank/n) %>%
  relocate(subject_id, run_id, trial_id, noise_type, intensity_praat, intensity_praat_prct) %>%
  
## Save a table
fname = glue('produced-sentences-acoustics-all-subj-DM1001-DM1020')
# ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), dpi=300, width=700*2.5, height=600*2.5, units='px')
write_tsv(sentences, glue('{fname}.tsv'))
                               
          

# Save a figure ---------
fname = glue('events-by-{rlang::as_string(color_by)}_plot-type-pointwise_tlock-{tlock_curr}')
# ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), dpi=300, width=700*2.5, height=600*2.5, units='px')
p <- ggplot(); dev.new(); ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), dpi=300, plot=p)
write_tsv(dftmp, glue('{PATH_FIG}/{fname}.tsv'))
# ggplotly(p)
                               
        
# Save a distribution horizontally ---------
ggplot(tmp_plot) +
  aes(x=time, y=!!color_by, color=!!color_by) +
  #geom_density(color = NA, alpha = 0.4, adjust=1.5) + 
  geom_jitter(size = 0.7) + 
  geom_boxplot(width=0.5) + 
  geom_vline(xintercept = 0) + 
  stat_compare_means() + 
  facet_grid(factor(event, voi) ~ ., scales="free") + 
  scale_color_manual(values = color_map_targets$hex, breaks=color_map_targets$name) +
  xlim(c(-1, 5)) +
  theme_bw() + 
  xlab(glue('Time [s] wrt {tlock_curr}'))                       
                               



voi <- c('IY', 'UW', 'AA', 'AE')
# fcr = (mean(u.F2) + mean(a.F2) + mean(i.F1) + mean(u.F1)) / (mean(i.F2) + mean(a.F1));

# % create FCR measure for every sentence
FCR <- vowels %>%
  mutate(phoneme = str_replace_all(phoneme, "[:digit:]", "")) %>%
  group_by(subject_id, run_id, trial_id, phoneme, noise_type) %>%
  summarise(F1 = mean(F1), F2 = mean(F2)) %>%
  filter(phoneme %in% voi) %>%
  pivot_wider(names_from = phoneme, values_from = c("F1", "F2")) %>%
  mutate(FCR = (F2_UW + F2_AA + F1_IY + F1_UW)/(F2_IY + F1_AA)) %>%
  group_by(subject_id, run_id, trial_id, noise_type)

sentences <- vowels %>%
  group_by(subject_id, run_id, trial_id, noise_type) %>%
  summarise_at(vars(intensity_rms:intensity_praat, duration), ~ mean(.x, na.rm = TRUE))

sentences <- left_join(sentences, FCR)
sentences <- left_join(sentences, trials %>% select(subject_id, run_id, trial_id, block_id))




# plot distributions for each individual 
color_by <- sym("intensity_rms") # F0, FCR, intensity_rms, intensity_praat, duration
ggplot(sentences, aes(x=noise_type, y=!!color_by, fill=noise_type, colours())) + 
  geom_jitter() + 
  geom_boxplot(width=0.5) + scale_fill_manual(values=color_map) + 
  stat_compare_means() +
  facet_wrap(~subject_id, scale="free_y", ncol = 5) + 
  ggtitle(glue('{rlang::as_string(color_by)}-sentences-per-subject'))

fname = glue('{rlang::as_string(color_by)}-sentences-per-subject')
# ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), width=700, height=600, units='px', dpi=300)
# ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), dpi=300)
dev.print(pdf, glue('{PATH_FIG}/{fname}.pdf'))


# single plot with summary stats for each patient in 2-d scatter
sentences %>%
  group_by(subject_id, noise_type) %>%
  summarize(m = mean(!!color_by, na.rm=TRUE),
            std = sd(!!color_by, na.rm=TRUE)) %>%
  pivot_wider(names_from = noise_type, values_from = c("m", "std")) %>%
  ggplot() + 
  theme(aspect.ratio=1) + 
  geom_abline() + 
  geom_errorbar( mapping = aes(x = m_QUIET, y = m_LMBRD,
                               ymin = m_LMBRD - 1.96*std_LMBRD, 
                               ymax = m_LMBRD + 1.96*std_LMBRD), 
                 width = 0) +
  geom_errorbarh( mapping = aes(x = m_QUIET, y = m_LMBRD,
                                xmin = m_QUIET - 1.96*std_QUIET, 
                                xmax = m_QUIET + 1.96*std_QUIET), 
                  height = 0) + 
  geom_point(aes(x = m_QUIET, 
                 y = m_LMBRD,
                 fill = subject_id),
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = TRUE) + 
  ggtitle(glue('{rlang::as_string(color_by)}-sentences-scatter')) + 
  xlab('QUIET') + 
  ylab('LOMBARD')


stats_by_subject <- compare_means(c(intensity_praat, FCR, F0, duration) ~ noise_type, sentences, group.by = c("subject_id")) %>%
  mutate(sig = p < 0.05) %>%
  rename(metric=.y.)

# Heatmap 
ggplot(stats_by_subject, aes(metric, subject_id, fill=sig)) + 
  geom_tile() + 
  theme(aspect.ratio=1) + 
  scale_fill_manual(values=color_map) 

fname = glue('all-metrics-heatmap-wilcoxon')
# ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), width=6.45, height=5.75, dpi=300)
dev.print(pdf, glue('{PATH_FIG}/{fname}.pdf'))



# scale out even further
stats_by_subject_summ <- stats_by_subject %>%
  group_by(metric, sig) %>%
  summarise(n = n()) %>%
  arrange(desc(sig), desc(n))
  
ggplot(stats_by_subject_summ, aes(fill=sig, x=metric, y=n)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=color_map) + 
  ylab('# participants') + 
  xlab("") +
  ggtitle('Summary of Lombard vs Quiet within-participant')

fname = glue('all-metrics-barplot-wilcoxon')
# ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), width=6.45, height=5.75, dpi=300)
dev.print(pdf, glue('{PATH_FIG}/{fname}.pdf'))






color_by <- sym("FCR")
ggplot(vowels_grouped, aes(x=noise_type, y=!!color_by, fill=noise_type, colours())) + 
  geom_jitter() + 
  geom_boxplot(width=0.5) + scale_fill_manual(values=color_map) + 
  stat_compare_means() +
  facet_wrap(~subject_id, scale="free_y", ncol = 5)

fname = glue('{rlang::as_string(annot_name)}-by-condition-{rlang::as_string(color_by)}')
ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), dpi=300)


# compare_means(FCR ~ noise_type, data = vowels_grouped, group.by = as.vector(groups(vowels_grouped), 'character'))




### plot by 
df_summ <-
  sentences %>% 
  group_by(subject_id, run_id, block_id) %>% 
  mutate(within_block_trial_id = row_number()) %>% 
  ungroup() %>%
  select(subject_id, within_block_trial_id, noise_type, intensity_praat) %>%
  group_by(subject_id, within_block_trial_id, noise_type) %>%
  drop_na() %>%
  summarize_all_stats()

m = sym("intensity_praat_mean")
s = sym("intensity_praat_stdev")
ggplot(df_summ, aes(x=within_block_trial_id, y=!!m, group=noise_type, color=noise_type)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks=c(1:10)) + 
  geom_errorbar(aes(ymin=!!m-!!s, ymax=!!m+!!s), width=.2,
                position=position_dodge(0.05)) + 
  facet_wrap(vars(subject_id), ncol=5) 
fname = glue('within-block-trials--by-subject-lombard-vs-quiet-{rlang::as_string(m)}')
ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), dpi=300)
write_tsv(df_summ, glue('{PATH_FIG}/{fname}.tsv'))







# # combine trials and sentences 
# trials <- trials %>% 
#   left_join(sentences, by=c('subject_id', 'run_id', 'trial_id')) %>%
#   left_join(SUBJECTS_META, by=c('subject_id'))


voi = c('audio_on', 'audio_off', 'go_time', 'sentence_on', 'sentence_off')


# tlock to audio_on
tmp = trials
tmp[voi] = tmp[voi] - tmp$audio_on
trials_tlock_audio_on = pivot_longer(tmp, cols=voi, names_to='event', values_to='time') %>%
  filter(event!='audio_on') %>% 
  mutate(tlock='audio_on')


tmp = trials
tmp[voi] = tmp[voi] - tmp$audio_off
trials_tlock_audio_off = pivot_longer(tmp, cols=voi, names_to='event', values_to='time') %>%
  filter(event!='audio_off') %>% 
  mutate(tlock='audio_off')

tmp = trials
tmp[voi] = tmp[voi] - tmp$go_time
trials_tlock_go_time = pivot_longer(tmp, cols=voi, names_to='event', values_to='time') %>%
  filter(event!='go_time') %>% 
  mutate(tlock='go_time')


tmp = trials
tmp[voi] = tmp[voi] - tmp$sentence_on
trials_tlock_sentence_on = pivot_longer(tmp, cols=voi, names_to='event', values_to='time') %>%
  filter(event!='sentence_on') %>% 
  mutate(tlock='sentence_on')


all = bind_rows(trials_tlock_audio_on, trials_tlock_audio_off, trials_tlock_go_time, trials_tlock_sentence_on) 
all = mutate(all, target=case_when(str_detect(dbs_target, 'STN') ~ 'STN', 
                                   str_detect(dbs_target, 'GPi') ~ 'GPi', 
                                   str_detect(dbs_target, 'VIM') ~ 'VIM'))



# Plot type: points 
tlock_curr = 'sentence_on'
color_by <- sym("target")

all %>% 
  filter(tlock==tlock_curr) %>%
  filter(event!='audio_on') %>%
  ggplot() +
  aes(x=time, y=!!color_by, color=!!color_by) +
  #geom_density(color = NA, alpha = 0.4, adjust=1.5) + 
  geom_jitter() + 
  geom_boxplot() + 
  geom_vline(xintercept = 0) + 
  facet_grid(factor(event, voi) ~ ., scales="free") + 
  xlim(c(-2, 7)) +
  theme_bw() + 
  xlab(glue('Time [s] wrt {tlock_curr}'))


fname = glue('events-by-{rlang::as_string(color_by)}_plot-type-pointwise_tlock-{tlock_curr}')
ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), width=10, height=6, dpi=300)



# Plot type: density 
tlock_curr = 'sentence_on'
color_by <- sym("target")

all %>% 
  filter(tlock==tlock_curr) %>%
  filter(event!='audio_on') %>%
  
  ggplot() +
  aes(x=time, color=!!color_by, fill=!!color_by) +
  geom_density(color = NA, alpha = 0.4, adjust=1.5) + 
  geom_vline(xintercept = 0) + 
  facet_grid(factor(event, voi) ~ ., scales="free") + 
  xlim(c(-2, 7)) +
  theme_bw() + 
  xlab(glue('Time [s] wrt {tlock_curr}'))


fname = glue('events-by-{rlang::as_string(color_by)}_plot-type-density_tlock-{tlock_curr}')
ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), width=10, height=6, dpi=300)





# Plot type: per patient 
tlock_curr = 'audio_off'
color_by <- sym("target")


all %>% arrange(target) %>% 
  mutate(subject_id=factor(subject_id)) %>%
  filter(tlock==tlock_curr) %>%
  filter(event!='audio_on') %>% 
  
  ggplot() +
  aes(x=time, y=(subject_id), color=!!color_by) +
  geom_jitter() + 
  geom_boxplot() + 
  geom_vline(xintercept = 0) + 
  facet_grid(factor(event, voi) ~ ., scales="fixed") + 
  xlim(c(-2, 7)) +
  theme_bw() + 
  xlab(glue('Time [s] wrt {tlock_curr}'))


fname = glue('events-by-{rlang::as_string(color_by)}_plot-type-pointwise-per-patient_tlock-{tlock_curr}')
ggsave(file=glue('{PATH_FIG}/{fname}.pdf'), width=10, height=6, dpi=300)









