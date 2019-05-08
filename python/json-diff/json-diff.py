#!/usr/bin/env python
# _*_ encoding: utf-8 _*_

import json

def toStr(val):
    if isinstance(val, int):
        return str(val).encode('utf-8')

    return val.encode('utf-8')

# return
#       flag       True / False
#       new_info
#       old_info
def json_cmp(new_data, old_data):

    if isinstance(new_data, dict) and isinstance(old_data, dict):

        for nk in new_data.keys():
            if nk in old_data:
                flag, nd, od = json_cmp(new_data[nk], old_data[nk])
                if flag:
                    new_data.pop(nk)
                    old_data.pop(nk)

        if new_data or old_data:
            return False, new_data, old_data

        return True, None, None

    elif isinstance(new_data, list) and isinstance(old_data, list):

        for nk in list(new_data):
            if nk in old_data or str(nk) in old_data:
                new_data.remove(nk)
                if nk in old_data:
                    old_data.remove(nk)
                else:
                    old_data.remove(str(nk))

        if new_data or old_data:
            return False, new_data, old_data

        return True, None, None

    else:

        return not cmp(toStr(new_data), toStr(old_data)), None, None

if __name__ == '__main__':

    new_data = "{\"domain\":{\"stop\":{\"timeout\":10,\"response\":{\"httpcode\":200},\"api\":\"127.0.0.1:7200\\/v1\\/\"}}}"
    old_data = "{\"domain\":{\"stop\":{\"timeout\":10,\"api\":\"127.0.0.1:7200\\/v1\\/\",\"response\":{\"httpcode\":200}}}}"

    flag, new_info, old_info = json_cmp(json.loads(new_data), json.loads(old_data))

    print("flag: ", flag)
    print("new_info: ", new_info)
    print("old_info: ", old_info)

    print(not cmp(new_data, old_data))


