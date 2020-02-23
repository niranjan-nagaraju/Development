#include <iostream>
#include <list>
#include <string>
using namespace std;

struct s {
	string lb;
	string ub;

	s(string a, string b) {lb = a; ub = b;}

	static bool compare( s &a, s &b )
	{ 
		if (a.lb != b.lb)
			return a.lb < b.lb;

		return a.ub < b.ub;
	}
	bool operator<(s &b)
	{
		if (this->lb != b.lb)
			return this->lb < b.lb;

		return this->ub < b.ub;

	}
	bool operator!=(s &b) {
		return !( (this->lb == b.lb) && (this->ub == b.ub));
	}
};

int main(void)
{
	list<s> strlist, strlist2;

	s tmp1("172.30.0.110", "172.30.0.115"), tmp2("172.30.1.1", "172.30.1.5"), tmp3("172.1.1.1", "172.2.1.1"), tmp4("172.30.0.145", ""), tmp5("172.30.0.146", "");

	strlist.push_back(tmp1);
	strlist.push_back(tmp2);
	strlist.push_back(tmp3);
	strlist.push_back(tmp4);

	strlist2.push_back(tmp3);
	strlist2.push_back(tmp2);
	strlist2.push_back(tmp5);
	strlist2.push_back(tmp1);

	list<s>::iterator iter, iter1;

	cout<<"Before Sort "<<endl;
	for(iter = strlist.begin(); iter != strlist.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	for(iter = strlist2.begin(); iter != strlist2.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	strlist.sort();
	strlist2.sort();

	cout<<"After Sort "<<endl;
	for(iter = strlist.begin(); iter != strlist.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	for(iter = strlist2.begin(); iter != strlist2.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	cout<<endl<<"Comparing "<<endl;

	for(iter = strlist.begin(), iter1 = strlist2.begin(); iter != strlist.end(); iter++, iter1++) {
		cout<<endl<<"Comparing ";
		cout<<"("<<iter->lb<<","<<iter->ub<<")  with ";
		cout<<"("<<iter1->lb<<","<<iter1->ub<<")" << endl;

		if (*iter != *iter1) {
			cout<<"Not equal.... Breaking out"<<endl;
			break;
		}

	}
	return 0;
}
