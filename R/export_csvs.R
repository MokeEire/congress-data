# Libraries
library(here)
source(here("R", "parsing_functions.R"))


# Load data ---------------------------------------------------------------

# Load latest version of clean data
clean_data = fs::dir_info(here::here("data", "cleaned"), regexp = "\\.Rds")
latest_file = clean_data[which.max(clean_data$change_time), "path", drop = T]

all_bills = readRDS(latest_file)

# Action code mapping table from GovInfo
# https://github.com/usgpo/bill-status/blob/main/BILLSTATUS-XML_User_User-Guide.md#3-action-code-element-possible-values
action_codes = read_csv(here("data", "action_codes.csv"), col_types = "cc")


# Unnest different datasets -----------------------------------------------

# Bill characteristics
bills_clean = all_bills |> 
  # Search each bill's actions for a "BecameLaw" action type
  mutate(bill_passed = map_lgl(actions, \(bill_actions) "BecameLaw" %in% bill_actions$type),
         policy_areas = map_chr(policy_areas, str_flatten_comma),
         legislative_subjects = map_chr(legislative_subjects, str_flatten_comma)) |> 
  select(-where(is_list), -where(\(x)sum(!is.na(x)) == 0)) |> 
  arrange(bill_id)

# Actions
# Unnest & arrange
all_actions = select(all_bills, bill_id, actions) |> 
  unnest(actions, keep_empty = T) |> # Keep bills without any actions, these shouldn't exist though
  arrange(bill_id, action_date, action_time)

actions_clean = all_actions |> 
  select(-where(is_list), -where(\(x)sum(!is.na(x)) == 0)) |> 
  arrange(bill_id, action_date, action_time)
# Votes
all_votes = all_actions |> 
  unnest(committees, keep_empty = T) |> 
  unnest(vote_record) |> 
  unnest(vote, names_sep = "_") |> 
  select(-vote_action_date, -vote_action_time)

# Split votes into Senate and House because they use different columns
# TODO: Do this cleaning in the data collection

# Senate
all_votes_senate = all_votes |> 
  filter(chamber == "Senate") |> 
  # Remove columns with no values
  select(-where(\(x) sum(!is.na(x)) == 0)) |> 
  # Unnest legislator data
  unnest(vote_legislator_votes, keep_empty = T)

# House
all_votes_house = all_votes |> 
  filter(chamber == "House") |> 
  # Remove columns with no values
  select(-where(\(x) sum(!is.na(x)) == 0)) |> 
  # Unnest legislator data
  unnest(vote_legislator_votes, keep_empty = T) |> #glimpse()
  # Align col names with senate
  rename(vote_number = vote_rollcall_num, 
         party = legislator_party, state = legislator_state, last_name = legislator_sort_field, 
         vote_cast = vote, vote_document_text = vote_desc, lis_member_id = legislator_name_id) |> 
  # Remove aggregates
  select(-vote_party_votes) |> 
  mutate(date = ymd_hms(date))


votes_combined = bind_rows(all_votes_house, all_votes_senate)

# Committees
all_committees = select(all_bills, bill_id, committees) |> 
  unnest(committees, keep_empty = T) |> 
  unnest(activities, names_sep = "_", keep_empty = T) |> 
  unnest(subcommittees, keep_empty = T, names_sep = "_") |> 
  unnest(subcommittees_activities, names_sep = "_", keep_empty = T) |> 
  arrange(bill_id, activities_date, subcommittees_activities_date)

# Sponsors
all_sponsors = select(all_bills, bill_id, sponsors) |> 
  unnest(sponsors, keep_empty = T) |> 
  mutate(type = "sponsor", district = as.character(district))
# Cosponsors
all_cosponsors = select(all_bills, bill_id, cosponsors) |> 
  unnest(cosponsors, keep_empty = T) |> 
  mutate(type = "cosponsor", district = as.character(district))

sponsors_combined = bind_rows(all_sponsors, all_cosponsors)

# Create a list of distinct sponsors
sponsors_distinct = sponsors_combined |> 
  distinct(bioguide_id, first_name, last_name, full_name, party, state, district)

# Save objects ------------------------------------------------------------

# CSVs
write_csv(bills_clean, here("data", "cleaned", "bills_117.csv"), na = "")
write_csv(actions_clean, here("data", "cleaned", "actions_117.csv"), na = "")
write_csv(all_committees, here("data", "cleaned", "committees_117.csv"), na = "")
write_csv(votes_combined, here("data", "cleaned", "votes_117.csv"), na = "")
write_csv(sponsors_combined, here("data", "cleaned", "sponsors_117.csv"), na = "")
write_csv(sponsors_distinct, here("data", "cleaned", "sponsors_distinct_117.csv"), na = "")
