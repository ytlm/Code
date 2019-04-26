#!/usr/bin/env python3.6
# _*_ encoding: utf-8 _*_

import requests                     # pip install requests
from bs4 import BeautifulSoup       # pip install beautifulsoup4
import re
import json

def _get_page():

    headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.103 Safari/537.36',
            }

    url = "https://dytt8.net/html/tv/hytv/20190411/58477.html"

    res = requests.get(url, headers=headers)

    return res.content

def _parse_html(html):

    """
    <div id="Zoom">
        <span>
            <table>
                <tbody>
                    <tr>
                        <td>
                            <a zijpeafj="thunder://....">ftp://...</a>
                        </td>
                    </tr>
                </tbody>
            </table>
        </span>
    </div>
    """

    # soup = BeautifulSoup(open("58477.html"), "html.parser")
    soup = BeautifulSoup(html, "html.parser")

    all_a = soup.find('div', id="Zoom").find_all('a')

    targets = []

    for item in all_a:
        if item:
            targets.append(item.text.encode('utf-8'))

    return targets

def _filter_targets(targets):

    if not targets or len(targets) <= 0:
        return True

    try:
        f = open("targets", "r")
        alwready_targets = [line.strip() for line in f]
    except IOError:
        alwready_targets = []

    main_target = []

    for t in targets:
        if not (t.decode('utf-8') in alwready_targets):
            main_target.append(t.decode('utf-8'))

    if len(main_target) > 0:
        with open("targets", "w") as f:
            for item in targets:
                f.write("%s\n" % item.decode('utf-8'))

    return main_target

def _start_download(tar):

    print(tar)

    if type(tar) is not list or len(tar) <= 0:
        return True

    patten = re.compile(r'-(\d{2}\S*$)')
    for it in tar:
        out = patten.findall(it)
        jreq = json.dumps({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "aria2.addUri",
                "params": [
                    it,
                    {
                    "out": out[0],
                    "split": "5",
                    "max-connection-per-server": "16",
                    "seed-ratio": "0"
                    }
                ]
            })

        c = requests.post('http://127.0.0.1:6800/jsonrpc', jreq)

        print(c)


if __name__ == '__main__':
    html = _get_page()
    targets = _parse_html(html)
    main_target = _filter_targets(targets)
    _start_download(main_target)
