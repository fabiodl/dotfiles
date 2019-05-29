import vobject
import sys
import re

v = vobject.readOne(sys.stdin)
summary = v.contents['vevent'][0].contents['summary'][0].value
startTime = v.contents['vevent'][0].contents['dtstart'][0].value
endTime = v.contents['vevent'][0].contents['dtend'][0].value
print("Summary:", summary)
print("Location:", v.contents['vevent'][0].contents['location'][0].value)
print("Time:", startTime.strftime("%c"), "~", endTime.strftime("%X"))
localCalendar = open("/home/fabio/remote-diary", "r").read()
searchFor = "{}-{} {}".format(startTime.strftime("%-m/%-d/%Y %H:%M"),
                              endTime.strftime("%H:%M"), summary)
if re.search(searchFor, localCalendar):
    print("OK, in calendar")
else:
    print("NOT in calendar, run update-schedule")
