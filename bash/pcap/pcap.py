#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

# pip install scapy scapy_http

from scapy.all import *
import scapy_http.http as http
import requests
import json

def analyse(File, Host):
    pkts = rdpcap(File)
    res = open('result', 'w+')
    for pkt in pkts:
        if TCP in pkt and pkt.haslayer(http.HTTPRequest):
            http_header = pkt[http.HTTPRequest].fields
            if 'Host' in http_header and http_header['Host'] == Host :
                req_url = '-X ' + http_header['Method'] + ' "http://' + http_header['Host'] + http_header['Path'] + '"'
                for key in http_header:
                    if not key == 'Path' and not key == 'Host' and not key == 'Method' and not key == 'Http-Version' and not key == 'Additional-Headers' and not key == 'Headers':
                        req_url = req_url + ' -H "' + key + ': ' + http_header[key] + '"'

                res.write(req_url + '\n')

if __name__ == "__main__":
    analyse(sys.argv[1], sys.argv[2])

# python pcap.py [filename.pcap] [host to filter]
# the result will save file of result
