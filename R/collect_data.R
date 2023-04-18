library(tictoc)
library(furrr)
library(here)
plan(multisession)
source("R/parsing_functions.R")

# List files ----
# Files collected from:
# https://www.govinfo.gov/bulkdata/BILLSTATUS/117
# Congress committee membership:
# https://github.com/unitedstates/congress-legislators/commit/5e4d9a0656458646e96c2b378fba640c0e22f8b1?diff=split
# Also here:
# https://clerk.house.gov/xml/lists/MemberData.xml
bill_types = list.files(here("data", "BILLSTATUS","117"))

bill_folders = here("data", "BILLSTATUS", "117", bill_types) |> 
    str_remove_all("\\/OneDrive")

bill_files = map(bill_folders, list.files, full.names = T) |> 
  set_names(bill_types)

all_files = flatten_chr(bill_files)


# Get files from sitemap --------------------------------------------------
# bill_status_sitemaps = read_xml("https://www.govinfo.gov/sitemap/bulkdata/BILLSTATUS/sitemapindex.xml") %>% 
#   # Strip namespace for coherent xpaths
#   xml_ns_strip() %>% 
#   # Find all loc elements which contain urls
#   xml_find_all("/sitemapindex/sitemap/child::loc") %>% 
#   # Convert them to text
#   map_chr(xml_text)

# Select sitemaps for given congress
# sitemaps_117 = str_subset(bill_status_sitemaps, "117")
# 
# (xml_nodes_117 = sitemaps_117 %>% 
#     map(read_xml) %>% 
#     map(xml_ns_strip) %>% 
#     map(xml_find_all, "/urlset/url/child::loc"))
# 
# xml_files_117 = xml_nodes_117 %>% 
#   map(xml_text) %>% 
#   set_names(str_extract(sitemaps_117, "(?<=BILLSTATUS\\/117)[a-z]{1,7}"))
# 
# xml_files_117_sample = map(xml_files_117, sample, size = 10)


# Extract bills from files ------------------------------------------------

# Testing
sample_files = sample(all_files, 500)

tic()
sample_df = future_map(sample_files, extract_bill_status, 
                       log_types = "console", .progress=T) |> 
  list_rbind()
toc()


tic(str_c("Extract ", length(all_files), " bills"))
all_bills = future_map(all_files, extract_bill_status, 
                       log_types = NULL, .progress = T) |> 
  list_rbind() |> 
  select(congress, origin_chamber, bill_id, type, number, title, 
         introduced_date, update_date, 
         latest_action_date = latest_action_action_date, latest_action_text, latest_action_action_time = latest_action_action_time,
         policy_areas, legislative_subjects, constitutional_authority_statement_text, titles, summaries, text_versions,
         sponsors, cosponsors, actions, committees,
         everything())
toc()


# Check data --------------------------------------------------------------

skimr::skim(all_bills)
# Note: length on list variables counts tibble columns
# Compare single bill to the XML
test_bill = sample_n(all_bills, 1)
test_file = str_subset(all_files, str_c(tolower(str_remove_all(test_bill$bill_id, "\\-")), "\\."))
test_xml = read_xml(test_file) %>% xml_child("bill")

test_xml
# Actions
View(unnest(test_bill$actions[[1]], committees, names_sep = "_"), "Test Actions")
test_xml_actions = xml_find_all(test_xml, "./actions/item")
test_xml_actions %>% map(xml_children)

# Committees
View(unnest(unnest(unnest(test_bill$committees[[1]], activities, names_sep = "_"), subcommittees, names_sep = "_"), subcommittees_activities, names_sep = "_"), "Test Committees")
test_xml_committees = xml_find_all(test_xml, "./committees/item")
test_xml_committees %>% map(xml_children)



# Unnest different datasets -----------------------------------------------

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
         vote_cast = vote, vote_document_text = vote_desc) |> 
  # Remove aggregates
  select(-vote_party_votes)


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


# Save objects ------------------------------------------------------------

saveRDS(all_bills, here("data", "cleaned", paste0("BILLSTATUS_117_", lubridate::today(), ".Rds")))
# saveRDS(actions_unnested, here("data", "cleaned", "BILLSTATUS_117_Actions.Rds"))

write_csv(actions_clean, here("data", "cleaned", "actions_117.csv"), na = "")
write_csv(all_committees, here("data", "cleaned", "committees_117.csv"), na = "")
write_csv(votes_combined, here("data", "cleaned", "votes_117.csv"), na = "")
write_csv(sponsors_combined, here("data", "cleaned", "sponsors_117.csv"), na = "")

