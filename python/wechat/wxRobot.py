#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

# pip install itchat

import itchat
import requests

KEY = 'e795ccea777548fe8c0cf49c427a1526'

def getResponse(msg):
    apiUrl = 'http://www.tuling123.com/openapi/api'
    data = {
        'key' : KEY,
        'info' : msg,
        'userid' : 'wechat-robot'
    }

    try:
        r = requests.post(apiUrl, data = data).json()
        return r.get('text')
    except:
        return

@itchat.msg_register(itchat.content.TEXT)
def tulingReplay(msg):
    defaultReplay = 'I receive : ' + msg['Text']

    reply = getResponse(msg['Text'])

    return reply or defaultReplay


if __name__ == '__main__':
    itchat.auto_login(hotReload=True, enableCmdQR=2)
    itchat.run()
