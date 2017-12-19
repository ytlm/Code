#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Tinglong Yang");
MODULE_DESCRIPTION("A example for linux kernel module.");
MODULE_VERSION("1.0.0");

static int __init example_init(void)
{
    printk(KERN_INFO "Hello World!\n");
    return 0;
}

static void __exit example_exit(void)
{
    printk(KERN_INFO "Goodbye!\n");
}

module_init(example_init);
module_exit(example_exit);
