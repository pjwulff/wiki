#!/usr/bin/env python
import json
import threading
import time
import urllib.parse, urllib.request

codes = ["en", "de"]
random_template = "https://%s.wikipedia.org/wiki/Special:Random"
types = ["articleinfo", "prose"]
url_template = "https://xtools.wmflabs.org/api/page/%s/%s.wikipedia.org/%s"

obs = 1000
num_threads = 100
per_thread = obs // num_threads
total = len(codes) * obs
so_far = 0

def get_stats():
    global so_far
    for n in range(per_thread):
        for code in codes:
            random = random_template % code
            while True:
                try:
                    data = {}
                    with urllib.request.urlopen(random) as url:
                        name = url.geturl().split("wiki/")[-1]
                        for infotype in types:
                            url_addr = url_template % (infotype, code, name)
                            with urllib.request.urlopen(url_addr) as url:
                                data[infotype] = \
                                        json.loads(url.read().decode())
                            time.sleep(num_threads / 500.)
                    ##f.write(json.dumps(data) + "\n")
                    name = urllib.parse.unquote(name)
                    csv = "%s,\"%s\",%u,%u\n" % \
                        (data["articleinfo"]["project"],
                         name,
                         data["articleinfo"]["revisions"],
                         data["prose"]["words"])
                    f.write(csv)
                    if len(name) < 64:
                        name += " " * (64 - len(name))
                    else:
                        name = name[:64]
                    so_far += 1
                    percent = (100 * so_far) // total
                    print("%d%%\t%s" % (percent, name), end="\r")
                    break
                except urllib.request.URLError as err:
                    continue
                    print("couldn't get %s" % url_addr)
                    print(err)

with open("data.csv", "a") as f:
    f.write("Project,Name,Revisions,Length\n")
    threads = [threading.Thread(target=get_stats) for n in range(num_threads)]
    [x.start() for x in threads]
    [x.join() for x in threads]

