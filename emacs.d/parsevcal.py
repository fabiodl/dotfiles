import vobject
import sys

v = vobject.readOne(sys.stdin)
print("Summary:", v.contents['vevent'][0].contents['summary'][0].value)
print("Location:", v.contents['vevent'][0].contents['location'][0].value)
print("Time:", v.contents['vevent'][0].contents['dtstart'][0].value.strftime(
    "%c"), "~", v.contents['vevent'][0].contents['dtend'][0].value.strftime("%X"))
