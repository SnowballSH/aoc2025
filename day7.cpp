#include <bits/stdc++.h>
using namespace std;

#define rep(i, a, b) for (int i = a; i < (b); ++i)
#define all(x) begin(x), end(x)
#define sz(x) (int)(x).size()
typedef long long ll;
typedef pair<int, int> pii;
typedef vector<int> vi;

// This problem is much easier in an imperative language

int main()
{
    ifstream inputFile("day7.in");
    string line;
    int n = 0, m = 0;
    vector<vector<char>> grid;
    int sr = 0, sc = 0;
    while (getline(inputFile, line))
    {
        m = line.length();
        grid.push_back(vector<char>(all(line)));
        auto it = find(all(line), 'S');
        if (it != line.end())
        {
            sr = n;
            sc = it - line.begin();
        }
        n++;
    }

    // O(nm)
    vector<ll> dp(m);
    vector<ll> dp2(m);
    dp[sc] = 1ll;
    int cur_row = sr;

    ll ans1 = 0ll;

    queue<pii> q;
    q.emplace(sr, sc);
    while (!q.empty())
    {
        auto [i, j] = q.front();
        q.pop();
        if (i + 1 >= n)
            continue;
        if (i > cur_row)
            cur_row = i, dp.swap(dp2), fill(all(dp2), 0ll);
        dp2[j] += dp[j];
        if (grid[i + 1][j] == '|')
            continue;
        if (grid[i + 1][j] == '^')
        {
            ans1++;
            if (j > 0)
            {
                dp2[j - 1] += dp[j];
                if (grid[i + 1][j - 1] != '|')
                    q.emplace(i + 1, j - 1), grid[i + 1][j - 1] = '|';
            }
            if (j < m - 1)
            {
                dp2[j + 1] += dp[j];
                if (grid[i + 1][j + 1] != '|')
                    q.emplace(i + 1, j + 1), grid[i + 1][j + 1] = '|';
            }
            continue;
        }
        q.emplace(i + 1, j);
        grid[i + 1][j] = '|';
    }

    ll ans2 = accumulate(all(dp2), 0ll);

    cout << "Part 1: " << ans1 << endl;
    cout << "Part 2: " << ans2 << endl;
}