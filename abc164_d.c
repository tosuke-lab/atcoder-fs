#include<stdio.h>
#include<string.h>

#define ll long long

char S[200000];
ll dp[2019][200000];

int main() {
  scanf("%s", S);

  dp[S[0]-'0'][0]++;

  size_t N=strlen(S);
  for(int j=1; j<N; j++) {
    int m=S[j]-'0';
    dp[m][j]++;
    for(int n=0; n<2019; n++) {
      int k=(10*n+m)%2019;
      dp[k][j] += dp[n][j-1];
    }
  }

  ll ans=0;
  for(int i=0; i<N; i++) {
    ans+=dp[0][i];
  }

  printf("%d\n", ans);
}