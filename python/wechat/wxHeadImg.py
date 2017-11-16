#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

# pip install itchat pillow

import itchat
import math
import os

import PIL.Image as Image

headDir = './head'

def getHead():

    if os.path.exists(headDir):
        os.system('rm -rf ' + headDir)
        os.system('mkdir ' + headDir)

    friends = itchat.get_friends()

    num = 0
    for i in friends:
        headImg = itchat.get_head_img(userName = i["UserName"])
        fileImage = open(headDir + '/' + str(num) + '.jpg', 'wb')
        fileImage.write(headImg)
        fileImage.close()
        num += 1

def spliceHead():

    if os.path.isfile(headDir + '/all.jpg'):
        os.remove(headDir + '/all.jpg')

    ls = os.listdir(headDir)

    lines = int(math.ceil(math.sqrt(len(ls))))
    eachSize = int(math.floor(640 / lines))

    image = Image.new('RGBA', (640, 640))

    x = 0
    y = 0

    for i in ls:
        img = Image.open(headDir + '/' + str(i))
        img = img.resize((eachSize, eachSize), Image.ANTIALIAS)

        image.paste(img, (x * eachSize, y * eachSize))
        x = x + 1
        if x == lines:
            x = 0
            y = y + 1

    image.save(headDir + '/all.jpg')


if __name__ == '__main__':
    itchat.auto_login(hotReload = True, enableCmdQR = 2)
    getHead()
    spliceHead()
    itchat.send_image(headDir + '/all.jpg', toUserName = 'filehelper')
