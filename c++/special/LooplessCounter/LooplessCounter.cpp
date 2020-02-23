/* A Loopless Counter */

#include<stdio.h>
using namespace std;

class LooplessCounter
{
     public:
       
       
       LooplessCounter()
       {
                        static int count;
                        
                        printf("%d\n", count+1);
                        count++;
       }
};


int main()
{
    LooplessCounter lc[5];
    getchar();
    return 0;
}
