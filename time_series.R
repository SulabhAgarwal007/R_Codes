library(lubridate)

# has all combinations of functions for m,d,y to convert date into standard dates

mdy('05-31-2019')
ymd('2019/05/31')
myd('05, 2019 31')
dmy('31 may 2019')

# for hour min sec, use _hms

mdy_hms('05-31-2019 13:54:26')

# to define timezone use tz

t = mdy_hms('05-31-2019 00:23:45', tz='Pacific/Auckland')

# to search for a time zone name use OlsonNames()

# to extract or set any value use function like second()

# to convert date in diff time zone use with_tz

with_tz(t, "America/Chicago")

# format()
# parse_date_time()

