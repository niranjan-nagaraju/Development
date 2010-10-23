#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <vector>
#include <algorithm>
using namespace std;

bool isJolly(vector<int> inList)
{
	vector<int> diffList;
	for(int i=0; i<inList.size()-1; i++) {
		int currdiff = abs(inList[i] - inList[i+1]);
		diffList.push_back(currdiff);
	}
	sort(diffList.begin(), diffList.end());

	for(int i=0; i<diffList.size(); i++) 
		if (diffList[i] != (i+1))
			return false;
	return true;
}

int main()
{
	vector<string> results;
	while(cin) {
		int length;
		vector<int> inList ;
		
		cin>>length;
		for (int i=0; i<length; i++) {
			int currval;
			cin>>currval;
			inList.push_back(currval);
		}
		if (isJolly(inList))
			results.push_back("Jolly");
		else
			results.push_back("Not Jolly");
	}

	for (int i =0; i<results.size()-1; i++)
		cout<<results[i]<<endl;
  
	return 0;
}
