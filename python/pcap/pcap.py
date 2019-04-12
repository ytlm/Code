#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

# pip install scapy scapy_http

from scapy.all import *
import scapy_http.http as http # pip install scapy scapy_http
import requests
import json

def analyse(File):
    pkts = rdpcap(File)
    res = open('result', 'w+')
    for pkt in pkts:
        if TCP in pkt:
            if pkt.haslayer(http.HTTPRequest):
                req_header = pkt[http.HTTPRequest].fields
                # print("request: ", pkt[http.HTTPRequest].show())
                # print("req header: ", req_header)
                if 'Host' in req_header:
                    req_url = '-X ' + req_header['Method'] + ' "http://' + req_header['Host'] + req_header['Path'] + '"'
                    for key in req_header:
                        if not key == 'Path' and not key == 'Host' and not key == 'Method' and \
                                not key == 'Http-Version' and not key == 'Additional-Headers' and \
                                not key == 'Headers':
                            req_url = req_url + ' -H "' + key + ': ' + req_header[key] + '"'

                    res.write(req_url + '\n')
            elif pkt.haslayer(http.HTTPResponse):
                resp_header = pkt[http.HTTPResponse].fields
                # print("response: ", pkt[http.HTTPResponse].show())
                # print("resp header: ", resp_header)
    res.close()


if __name__ == "__main__":
    analyse(sys.argv[1])

# python pcap.py [filename.pcap]
# the result will save file of result
