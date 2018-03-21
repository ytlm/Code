### if
```bash
if [ condition1 ]; then
    expression1
elif [ condition2 ]; then
    expression2
else
    expression3
fi
```
* **-eq** 等于
* **-ne** 不等于
* **-gt** 大于
* **-ge** 大于等于
* **-lt** 小于
* **-le** 小于等于
* **[ -a FILE ]** 文件存在
* **[ -d FILE ]** 文件存在且是一个目录
* **[ -f FILE ]** 文件存在且是一个普通文件

### timezone
```bash
timedatectl set-timezone Asia/Shanghai
rm -rf /etc/localtime && ln -sv /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
```
