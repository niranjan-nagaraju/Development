#include <stdio.h>
#include <string.h>
#include <common.h>
#include <ctype.h>

void reverse(char *array, int min, int max)
{
    int i, j;

    for(i=min, j=max; i<j; i++, j--)
    {
        swapChar(array+i, array+j);
    }
}

void reverse_words(char *array, int len)
{
    int i, ll, ul;

    reverse(array, 0, len-1);

    ll = ul = 0;

	i = 0;
	while (ul <= len)
    {
        if(! isspace(array[i]) && array[i] != 0)
        {
            ul++;
        }
        else
        {
            reverse(array, ll, ul-1);
            ll = ul = i+1;
        }

		i++;

    }
}

int main()
{
    char word[100];

	scanf("%[^\n]s", word);
	reverse_words(word, strlen(word));

	printf("%s\n", word);
    return 0;

}
