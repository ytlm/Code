#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

import sys
import re
import json
import time

domain_forbid = {}
app_forbid = {}
stream_forbid = {}

def to_str(*val):
    ans = ""
    split = ""
    for v in val:
        ans = ans + split + str(v)
        split = ":"

    return str(ans)

def to_number(val):
    return int(val)

def forbid_filter(dicts, key, endtime):

    if key in dicts:
        if endtime == 0:
            dicts.pop(key)
        elif dicts[key] < endtime:
            dicts[key] = endtime
    else:
        cur_time = int(time.time())
        if endtime != 0 and endtime > cur_time:
            dicts[key] = endtime


def write_forbid_file(name, data):

    with open(name, 'w') as fp:
        json.dump(data, fp, indent=4)


if __name__ == '__main__':

    for line in open(sys.argv[1], 'r'):
        if re.findall(r'/stream_forbid/domain', line):
            domain = re.findall(r'domain=([^&]*)', line)
            end_time = re.findall(r'end_time=(\d*)', line)
            if end_time and domain:
                dm = domain[0]
                et = end_time[0]
                forbid_filter(domain_forbid, to_str(dm), to_number(et))
        elif re.findall(r'/stream_forbid/app', line):
            domain = re.findall(r'domain=([^&]*)', line)
            app = re.findall(r'app=([^&]*)', line)
            end_time = re.findall(r'end_time=(\d*)', line)
            if end_time and domain and app:
                dm = domain[0]
                ap = app[0]
                et = end_time[0]
                forbid_filter(app_forbid, to_str(dm, ap), to_number(et))
        elif re.findall(r'/stream_forbid/stream', line):
            domain = re.findall(r'domain=([^&]*)', line)
            app = re.findall(r'app=([^&]*)', line)
            stream = re.findall(r'stream=([^&]*)', line)
            end_time = re.findall(r'end_time=(\d*)', line)
            if end_time and domain and app and stream:
                dm = domain[0]
                ap = app[0]
                sm = stream[0]
                et = end_time[0]
                forbid_filter(stream_forbid, to_str(dm, ap, sm), to_number(et))
        else:
            print(line)


    write_forbid_file("domain_forbid.json", domain_forbid)
    write_forbid_file("app_forbid.json", app_forbid)
    write_forbid_file("stream_forbid.json", stream_forbid)

