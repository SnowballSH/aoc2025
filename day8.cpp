#include <bits/stdc++.h>
using namespace std;

#define int long long
#define rep(i, a, b) for (int i = a; i < (b); ++i)
#define all(x) begin(x), end(x)
#define sz(x) (int)(x).size()
typedef long long ll;
typedef pair<int, int> pii;
typedef vector<int> vi;

// This problem is much easier in an imperative language

struct UF
{
    vi e;
    int k;
    UF(int n) : e(n, -1), k(n) {}
    bool sameSet(int a, int b) { return find(a) == find(b); }
    int size(int x) { return -e[find(x)]; }
    int find(int x) { return e[x] < 0 ? x : e[x] = find(e[x]); }
    bool join(int a, int b)
    {
        a = find(a), b = find(b);
        if (a == b)
            return false;
        k--;
        if (e[a] > e[b])
            swap(a, b);
        e[a] += e[b];
        e[b] = a;
        return true;
    }
};

struct Point
{
    int x, y, z;
    Point(int a, int b, int c) : x(a), y(b), z(c) {}
    Point() {}
    int dist(Point other)
    {
        return (x - other.x) * (x - other.x) + (y - other.y) * (y - other.y) + (z - other.z) * (z - other.z);
    }
};

signed main()
{
    ifstream inputFile("day8.in");
    string line;
    int n = 0;
    vector<Point> points;
    int sr = 0, sc = 0;
    while (getline(inputFile, line))
    {
        istringstream iss(line);
        int x, y, z;
        char c;
        iss >> x >> c >> y >> c >> z;
        points.emplace_back(x, y, z);
        n++;
    }

    // Preprocess: O(n^2 log n)
    vector<pair<int, pii>> q;
    q.reserve(n * n);
    rep(i, 0, n) rep(j, i + 1, n)
        q.emplace_back(points[i].dist(points[j]), make_pair(i, j));
    sort(all(q));

    // O(n log n)
    {
        UF dsu(n);
        const int topk = 1000;
        rep(k, 0, topk)
        {
            auto [d, p] = q[k];
            auto [i, j] = p;
            dsu.join(i, j);
        }
        vi sizes;
        rep(i, 0, n)
        {
            int j = dsu.find(i);
            if (i == j)
            {
                sizes.push_back(dsu.size(j));
            }
        }
        assert(sizes.size() >= 3);
        sort(all(sizes));
        reverse(all(sizes));
        int ans1 = sizes[0] * sizes[1] * sizes[2];

        cout << "Part 1: " << ans1 << endl;
    }

    // O(n^2 log n)
    {
        UF dsu(n);
        int i, j;
        for (int k = 0; dsu.k > 1; k++)
        {
            auto [d, p] = q[k];
            tie(i, j) = p;
            dsu.join(i, j);
        }
        int ans2 = points[i].x * points[j].x;
        cout << "Part 2: " << ans2 << endl;
    }
}