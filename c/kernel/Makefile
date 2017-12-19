
PWD            := $(shell pwd)

KVERSION       := $(shell uname -r)

KERNEL_DIR      = /lib/modules/$(KVERSION)/build/

MODULE_NAME    := example

obj-m          := $(MODULE_NAME).o

all:
	make -C $(KERNEL_DIR) M=$(PWD) modules

clean:
	make -C $(KERNEL_DIR) M=$(PWD) clean

test:
	sudo dmesg -C
	sudo insmod example.ko
	sudo rmmod example.ko
	dmesg
