#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void merge(int a[],int low,int mid,int height)
{
	int c[100];
	int i = low;
	int j = mid+1;
	int p = 0;
	while(i <= mid && j <= height)
		c[p++] = a[i] < a[j] ? a[i++] : a[j++];
	while(i <= mid)
		c[p++] = a[i++];
	while(j <= height)
		c[p++] = a[j++];
	for(i = low,j = 0; i <= height; i++,j++)
	   a[i] = c[j];	
}

void mergeSort(int a[],int low,int height)
{
	int mid;
	if(low >= height)
		return;
	mid = (low+height) >> 1;
	mergeSort(a,low,mid);
	mergeSort(a,mid+1,height);
	merge(a,low,mid,height);
}

int main(int argc,char * argv[])
{
	int arr[100] = {46,1,89,4,9,10};
	int i;
	int n = 6;
	for(i = 0; i < n; i++)
		printf("%d%c", arr[i],i == n-1 ? '\n' : ' ');
	mergeSort(arr,0,n-1);
	for(i = 0; i < n; i++)
		printf("%d%c", arr[i],i == n-1 ? '\n' : ' ');
	return 0;
}
