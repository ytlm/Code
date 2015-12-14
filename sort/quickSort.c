#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int par(int a[],int low,int heigh)
{
	int i = low;
	int j = heigh;
	int temp = a[i];
	while(i < j)
	{
		while(j > i && a[j] >= temp)
			j--;
		a[i] = a[j];
		while(i < j && a[i] < temp)
			i++;
		a[j] = a[i];
	}
	a[i] = temp;
	return i;
}

void quickSort(int a[],int low,int heigh)
{
	if(low >= heigh)
		return;
	int mid;
	mid = par(a,low,heigh);

	quickSort(a,low,mid-1);
	quickSort(a,mid+1,heigh);
}
int main(int argc, char* argv[])
{
	int arr[100] = {8,9,3,5,12,78,1,2,905,1,7,8,9,3,2,1,4,5,6,7,0};
	int i;
	int n = 20;
	quickSort(arr,0,n-1);
	for(i =0; i < n; i++)
		printf("%d%c",arr[i],i == n-1 ? '\n' : ' ');
	return 0;
}
