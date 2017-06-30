/*
 * 简单的抓取ip，arp，rarp的数据包
 *
*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <pcap.h>
#include <time.h>
#include <arpa/inet.h>

#define COUNT_DEVS 100
#define ETH_IP   0x0800
#define ETH_ARP  0x0806
#define ETH_RARP 0x8035

void showProtocol(char * pro,const u_char *packet,bpf_u_int32 n)
{
	int i;
	fprintf(stdout,"Des MAC : ");
	for(i = 0; i < 6; i++)
		fprintf(stdout," %02x",packet[i]);
	
	fprintf(stdout,"\n");

	fprintf(stdout,"Sou MAC : ");
	for(i = 6; i < 12; i++)
		fprintf(stdout," %02x",packet[i]);

	fprintf(stdout,"\n");
	fprintf(stdout,"Protocol type is : %s\n\n",pro);

	for( i = i+2; i < n; i++)
	{
		fprintf(stdout," %02x",packet[i]);
		if(((i-13)%16 == 0) || i ==n-1)
			fprintf(stdout,"\n");
	}
}

void callback(u_char * arg, const struct pcap_pkthdr * pkthdr, const u_char * packet)
{
	int * id = (int*)arg;
	fprintf(stdout,"\nId : %d\n",++(*id));
	fprintf(stdout,"Packet  length : %d\n",pkthdr->len);
	fprintf(stdout,"Capture length : %d\n",pkthdr->caplen);
	fprintf(stdout,"Received time  : %s\n",ctime((const time_t *)&pkthdr->ts.tv_sec));

	u_int16_t *pcol;
	pcol = (u_int16_t*)(packet+12);
	*pcol = ntohs(*pcol);
	if((*pcol) == ETH_IP)
		showProtocol("IP",packet,pkthdr->caplen);
	if((*pcol) == ETH_ARP)
		showProtocol("ARP",packet,pkthdr->caplen);
	if((*pcol) == ETH_RARP)
		showProtocol("RARP",packet,pkthdr->caplen);
}
int main(int argc, char * argv[])
{
	char      errbuf[PCAP_ERRBUF_SIZE];
	char      *devname = NULL;
	pcap_t    *device = NULL;

	memset(errbuf,0,sizeof(errbuf));
	
	devname = pcap_lookupdev(errbuf);
	if(devname == NULL)
	{
		fprintf(stderr,"pcap_lookupdev() error:%s\n",errbuf);
		exit(1);
	}

	device = pcap_open_live(devname,65535,1,-1,errbuf);

	if(device == NULL)
	{
		fprintf(stderr,"pcap_open_live() error:%s\n",errbuf);
		exit(1);
	}
	
	const char * str = "ip and udp and port 53";
	struct bpf_program fp;
	bpf_u_int32 netmask,netp;
	
	if(pcap_lookupnet(devname,&netp,&netmask,errbuf) == -1)
	{
		fprintf(stderr,"lookupnet() error:%s\n",errbuf);
		exit(1);
	}

	if(pcap_compile(device,&fp,str,1,netmask) == -1)
	{
		pcap_perror(device,pcap_geterr(device));
		exit(1);
	}
	
	if(pcap_setfilter(device,&fp) == -1)
	{
		pcap_perror(device,pcap_geterr(device));
		exit(1);
	}
	
	int i = 0;

	if(pcap_loop(device,-1,callback,(u_char*)&i) != 0)
	{
		fprintf(stderr,"an error occurs\n");
		exit(1);
	}
	
	pcap_close(device);

	return 0;
}
