#include <bits/stdc++.h>
using namespace std;

#define int long long
#define rep(i, a, b) for (int i = a; i < (b); ++i)
#define all(x) begin(x), end(x)
#define sz(x) (int)(x).size()
typedef long long ll;
typedef pair<int, int> pii;
typedef vector<int> vi;

int solve(map<string, vector<string>> &adj, string source, string target)
{
    map<string, int> dp;
    function<int(string)> dfs = [&](string u)
    {
        if (dp.count(u))
            return dp[u];
        if (u == target)
            return 1LL;
        int res = 0;
        for (auto &v : adj[u])
        {
            res += dfs(v);
        }
        return dp[u] = res;
    };
    return dfs(source);
}

signed main()
{
    ifstream inputFile("day11.in");
    string line;
    int n = 0;
    int m = 0;
    map<string, vector<string>> adj;
    while (getline(inputFile, line))
    {
        istringstream iss(line);
        string source;
        iss >> source;
        source.resize(source.size() - 1);
        string dest;
        while (iss >> dest)
        {
            adj[source].push_back(dest);
            m++;
        }
        n++;
    }

    {
        int c1 = solve(adj, "you", "out");
        cout << "Part 1: " << c1 << endl;
    }
    {
        int c2_1 = solve(adj, "svr", "dac");
        int c2_2 = solve(adj, "dac", "fft");
        int c2_3 = solve(adj, "fft", "out");
        int c2_4 = solve(adj, "svr", "fft");
        int c2_5 = solve(adj, "fft", "dac");
        int c2_6 = solve(adj, "dac", "out");
        int c2 = c2_1 * c2_2 * c2_3 + c2_4 * c2_5 * c2_6;
        cout << "Part 2: " << c2 << endl;
    }
}