
#include <stdio.h>
int main()
{
    int i,j;
    int myNumbers[3][3];

    for(i=0;i<=3;i++){

        for(j=0;j<=3;j++){
            scanf("%d",&myNumbers[i][j]);
        }
    }

      for(i=0;i<=3;i++){
        for(j=0;j<=3;j++){

        printf("%d  ",myNumbers[i][j]);

        }
    }
    return 0;
}
