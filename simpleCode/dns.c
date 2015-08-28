/*
 * 简单的DNS解析查询
 *
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <pcap.h>
#include <time.h>
#include <arpa/inet.h>
#include <stdint.h>

#define SIZE_IP  4
#define SIZE_MAC 6

typedef struct
{
	unsigned char des_mac[SIZE_MAC];
	unsigned char sou_mac[SIZE_MAC];
	uint16_t pcol;
}ETHdata;

typedef struct
{
	uint8_t  ver_headl;//version(4) headerLength(4);
	uint8_t  pcolServ;
	uint16_t totaLength;
	uint16_t identifi;
	uint16_t flag_offset;//flag(3) offset(13)
	uint8_t  liveTime;
	uint8_t  protocol;
	uint16_t headCheakSum;
	unsigned char souAddr[SIZE_IP];
	unsigned char desAddr[SIZE_IP];
}IPdata;

typedef struct
{
	uint16_t souPort;
	uint16_t desPort;
	uint16_t length;
	uint16_t checkSum;
}UDPdata;

typedef struct
{
	uint16_t ID;
	uint16_t FLAGS;//QR(1),opcode(4),AA(1),TC(1),RD(1),RA(1),zero(3),rcode(4)
	uint16_t queCount;
	uint16_t ansRss;
	uint16_t authority;
	uint16_t additional;
}DNSdata;

typedef struct
{
	ETHdata eth;
	IPdata  ip;
	UDPdata udp;
	DNSdata dnsHeader;
}DNS;

void printANS(uint8_t n, const u_char * pac)
{
	uint8_t * count = NULL;
	uint8_t * temp  = NULL;
	uint8_t i,j;
	i = n+1;
	while(1)
	{
		count = (uint8_t *)(pac+i);
		if(*count == 0)
        	return ;
		if(*count == 0xc0)
		{
			temp = (uint8_t *)(pac+i+1);
			printANS(((*temp) - 13),pac);
			return ;
		}
		for(i = i+1,j = 0; j < *count; i++, j++)
		{
			//printf("%d\n",i);
			fprintf(stdout,"%c",pac[i]);
		}

		fprintf(stdout,".");
	}
}


void dnsAnswer(const u_char * pac,DNS * dnsType)
{
	int i = 0;
	int j;
	uint8_t *count = NULL;
	uint8_t *temp  = NULL;
	uint16_t queSum,ansSum;

	queSum = ntohs(dnsType->dnsHeader.queCount);
	ansSum = ntohs(dnsType->dnsHeader.ansRss);
	
	//printf("queSum = %d\n ansSum = %d\n",queSum,ansSum);	
	uint16_t *dnsAnsType  = NULL;
	uint16_t *dnsAnsClass = NULL;

	while(queSum --)//output all quesion
	{
		while(1)
		{
			count = (uint8_t*)(pac+i);
			if(*count == 0)
				break;
			//*count = ntohs(*count);
			for(i = i+1,j = 0; j < *count; i++,j++)
			{

			//	printf("%d\n",i);
				fprintf(stdout,"%c",pac[i]);
			}
			fprintf(stdout,".");
		}
		fprintf(stdout,"\n");
		i = i + 5;
	}

	while(ansSum --)//output all answer
	{
		fprintf(stdout, "\t");
		
		int I_temp = 0;

		temp = (uint8_t*)(pac+i);
		i = i+2;
		dnsAnsType = (uint16_t*)(pac+i);
		*dnsAnsType = ntohs(*dnsAnsType);
		i = i+2;
		dnsAnsClass = (uint16_t*)(pac+i);
		*dnsAnsClass = ntohs(*dnsAnsClass);
		i = i+8;
		if(*dnsAnsType == 5)
		{
			fprintf(stdout,"CNAME:");
			while(1)
			{
				count = (uint8_t*)(pac+i);
				
				if(*count == 0)
					break;

				if(*count == 0xc0)
				{
					temp = (uint8_t *)(pac+i+1);
					printANS(((*temp) - 13),pac);
					i = i+1;
					break;
				}
				for(i = i+1,j = 0; j < *count; i++,j++)
				{
			//		printf("%d\n",i);
					fprintf(stdout,"%c",pac[i]);
				}
				if(I_temp != 0)
					i = I_temp;
				fprintf(stdout,".");
			}
			i++;
			fprintf(stdout,"\n");
		}
		else
		{
			for(i = i, j = 0; j < SIZE_IP; i++, j++)
			{
			//	printf("%d\n",i);
				fprintf(stdout, "%d",pac[i]);
				if(j != SIZE_IP - 1)
					fprintf(stdout,".");
			}
			fprintf(stdout,"\n");
		}
	}
	fprintf(stdout,"\n");
}


void callback(u_char * arg, const struct pcap_pkthdr * pkthdr, const u_char * packet)
{
	int * id = NULL;
	id = (int*)arg;
	
	DNS *dnsType = NULL;
	dnsType = (DNS*)(packet);

	uint16_t Flags;
	Flags = ntohs(dnsType->dnsHeader.FLAGS); 
	if((Flags >> 15) == 1)
	{
		fprintf(stdout,"\nId : %d\n",++(*id));
		dnsAnswer(packet+sizeof(DNS),dnsType);
	}
}

int main(int argc, char * argv[])
{
	char   errbuf[PCAP_ERRBUF_SIZE];
	char   *devname = NULL;
	pcap_t *device  = NULL;

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

	if(pcap_loop(device,2,callback,(u_char*)&i) != 0)
	{
		fprintf(stderr,"an error occurs\n");
		exit(1);
	}
	
	pcap_close(device);

	return 0;
}
