#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

import itchat
from itchat.content import *

import sys
import time
import re
import os

reload(sys)
sys.setdefaultencoding('utf8')

msg_information = {}
face_bug = None

@itchat.msg_register([TEXT, PICTURE, FRIENDS, CARD, MAP, SHARING, RECORDING, ATTACHMENT, VIDEO],isFriendChat = True, isGroupChat = True)
def handle_receive_msg(msg):
    global face_bug
    msg_time_rec = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    msg_from = itchat.search_friends(userName=msg['FromUserName'])['NickName']
    msg_time = msg['CreateTime']
    msg_id = msg['MsgId']

    msg_content = None
    msg_share_url = None

    if msg['Type'] in ['Text', 'Friends']:
        msg_content = msg['Text']

    elif msg['Type'] in ['Attachment', 'Video', 'Picture', 'Recording']:
        msg_content = msg['FileName']
        msg['Text'](str(msg_content))

    elif msg['Type'] == 'Card':
        msg_content = msg['RecommendInfo']['NickName'] + ' 的名片'
        if msg['RecommendInfo']['Sex'] == 1:
            msg_content += '性别为男'
        else:
            msg_content += '性别为女'

    elif msg['Type'] == 'Map':
        x, y, location = re.search(
            "<location x=\"(.*?)\" y=\"(.*?)\".*label=\"(.*?)\".*", msg['OriContent']).group(1, 2, 3)
        if location is None:
            msg_content = r"纬度->" + x.__str__() + " 经度->" + y.__str__()
        else:
            msg_content = r"" + location

    elif msg['Type'] == 'Sharing':
        msg_content = msg['Text']
        msg_share_url = msg['Url']

    face_bug = msg_content

    msg_information.update(
        {
            msg_id: {
                "msg_from": msg_from, "msg_time": msg_time, "msg_time_rec": msg_time_rec,
                "msg_type": msg['Type'],
                "msg_content": msg_content, "msg_share_url": msg_share_url
            }
        }
    )


@itchat.msg_register(NOTE, isFriendChat = True, isGroupChat = True)
def information(msg):
    if '撤回了一条消息' in msg['Content']:
        old_msg_id = re.search("\<msgid\>(.*?)\<\/msgid\>", msg['Content']).group(1)
        old_msg = msg_information.get(old_msg_id)
        if len(old_msg_id) < 11:
            itchat.send_file(face_bug, toUserName = 'filehelper')
        else:
            msg_body = "撤回消息提醒 : \n" \
                       + "好友 : " + old_msg.get('msg_from') + "\n" \
                       + "时间 : " + old_msg.get('msg_time_rec') + "\n" \
                       + "类型 : " + old_msg.get("msg_type") + "\n" \
                       + "内容 : " + r"" + old_msg.get('msg_content') + "\n"
            if old_msg['msg_type'] == "Sharing":
                msg_body += "链接 : " + old_msg.get('msg_share_url')

            itchat.send_msg(msg_body, toUserName = 'filehelper')

            if old_msg['msg_type'] in ['Picture', 'Recording', 'Video', 'Attachment']:
                file = '@fil@%s' % (old_msg['msg_content'])
                itchat.send(msg = file, toUserName = 'filehelper')
                os.remove(old_msg['msg_content'])

            msg_information.pop(old_msg_id)

if __name__ == '__main__':
    itchat.auto_login(hotReload = True, enableCmdQR = 2)
    itchat.run()
