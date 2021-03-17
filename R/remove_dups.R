df <- readRDS('R/worldcities.rds')

# identify duplicates in list column
samenames <- df %>%
  group_by(list) %>%
  mutate(dupe = n()>1) %>%
  filter(dupe == TRUE)

# Append list with admin_name (US = state)
samenames <- samenames %>%
  mutate(list = paste0(list, ' (', admin_name, ')'))

# Check out duplicates STILL
dups <- samenames %>%
  group_by(list) %>%
  filter(n()>1)

# Initialise
dfdups <- anti_join(samenames, dups)

# Keep dups with population
dups <- dups %>%
  filter(!is.na(population))

# Exclude dups from samenames
dfdups <- dfdups %>%
  rbind(dups) %>%
  select(-dupe)

# Join
df <- anti_join(df, samenames, by = names(df[1:10])) %>%
  bind_rows(dfdups)

# Save new rds
saveRDS(df, 'R/worldcities.rds')

# Clean up
rm(df, dfdups, dups, samenames)