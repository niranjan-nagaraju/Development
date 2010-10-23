#include <iostream>
#include <list>
#include <string>
using namespace std;

class s {
	public:
	string lb;
	string ub;

	s(string a, string b) {lb = a; ub = b;}

	bool operator()( s &a, s &b )
	{ 
		return (a.lb.compare(b.lb) < 0) && (a.ub.compare(b.ub)); 
	}
};
/**
struct Functor 
{
    bool operator()( s &a, s &b )
	{
		return (a.lb.compare(b.lb) < 0) && (a.ub.compare(b.ub));
	}
};
*/
int main(void)
{
	list<s> strlist, strlist2;
	//Functor f;

	s tmp1("172.30.0.110", "172.30.0.115"), tmp2("172.30.1.1", "172.30.1.5"), tmp3("172.1.1.1", "172.2.1.1");

	strlist.push_back(tmp1);
	strlist.push_back(tmp2);
	strlist.push_back(tmp3);

	strlist2.push_back(tmp3);
	strlist2.push_back(tmp2);
	strlist2.push_back(tmp1);

	list<s>::iterator iter;
	cout<<"Before Sort "<<endl;
	for(iter = strlist.begin(); iter != strlist.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	for(iter = strlist2.begin(); iter != strlist2.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	strlist.sort(strlist.front());
	strlist2.sort(tmp1);


	cout<<"After Sort "<<endl;
	for(iter = strlist.begin(); iter != strlist.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	for(iter = strlist2.begin(); iter != strlist2.end(); iter++)
		cout<<"("<<iter->lb<<","<<iter->ub<<")  ";
	cout<<endl;

	return 0;
}
