/*自己设计并实现一些有关string方面的一些函数，及处理一些问题
 *
 * 自定义类型为myString 详情见mystring.c
 * 
 */

#ifndef _MYSTRING_H
#define _MYSTRING_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define INIT_SIZE 5 

typedef struct mystring * myString;


void print(myString );//输出测试用


myString str_new(void);
//该函数返回一个myString  类型的指针，为初始化内存

void str_delete(myString);
//释放原来申请的内存

int str_init(myString s,const char * ch);
//将字符串ch当成s的初始字符串

size_t str_getsize(const myString s);
//获取字符串的实际长度

int str_compare(const myString s1,const myString s2);
/*比较两个字符串的大小，返回值为0表示两个字符串相等，
 * 正数表示s1大于s2
 * -1表示s1小于s2
 */

int str_ncopy(myString s1,myString s2,size_t n);
//把s2的从头开始的连续n个字符复制到s1中

int str_begin_cat(myString s1,const myString s2);
//将s2添加到s1之前

int str_end_cat(myString s1,const myString s2);
//将s2添加到s1之后

int str_sToint(const myString);
//将对应的字符转换成int整数并返回，加出错处理

double str_sTodouble(const myString);
//将字符串转换成实数并返回,加出错处理

int find_first_str(const myString s,const char * ch);
//返回在myString中第一个满足ch字符串的下标，没有满足的则返回-1

int replace_str(myString s,const char * from,const char * to);
//在myString中替换所有出现的from字符串的地方为to字符串，若替换成功自返回1，出错则返回-1

char * str_division(const myString s,size_t beg,size_t n);
//将字符串从beg开始的之后n个字符截断并返回，原来的字符串不变

void str_erase(myString s,size_t beg,size_t n);
//将字符串从beg开始的n个字符删除

int wrtie_str_to_file(const char * pathname,const myString s);
//将字符串s全部写入到文件pathname中

int read_str_from_file(const char * pathname,myString s,size_t n);
//从文件中读入n个字符放入到字符串s中

char ** str_split(myString s,const char * ch, size_t * n);
//将字符串s，用字符ch分割，返回分割后的数组,n为数组的长度



#endif
