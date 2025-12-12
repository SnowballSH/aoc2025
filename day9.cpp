#include <bits/stdc++.h>
using namespace std;

#define int long long
#define rep(i, a, b) for (int i = a; i < (b); ++i)
#define all(x) begin(x), end(x)
#define sz(x) (int)(x).size()
typedef long long ll;
typedef pair<int, int> pii;
typedef vector<int> vi;

template <class T>
int sgn(T x) { return (x > 0) - (x < 0); }
template <class T>
struct Point
{
    typedef Point P;
    T x, y;
    explicit Point(T x = 0, T y = 0) : x(x), y(y) {}
    bool operator<(P p) const { return tie(x, y) < tie(p.x, p.y); }
    bool operator==(P p) const { return tie(x, y) == tie(p.x, p.y); }
    P operator+(P p) const { return P(x + p.x, y + p.y); }
    P operator-(P p) const { return P(x - p.x, y - p.y); }
    P operator*(T d) const { return P(x * d, y * d); }
    P operator/(T d) const { return P(x / d, y / d); }
    T dot(P p) const { return x * p.x + y * p.y; }
    T cross(P p) const { return x * p.y - y * p.x; }
    T cross(P a, P b) const { return (a - *this).cross(b - *this); }
    T dist2() const { return x * x + y * y; }
    double dist() const { return sqrt((double)dist2()); }
    // angle to x-axis in interval [-pi, pi]
    double angle() const { return atan2(y, x); }
    P unit() const { return *this / dist(); } // makes dist()=1
    P perp() const { return P(-y, x); }       // rotates +90 degrees
    P normal() const { return perp().unit(); }
    // returns point rotated 'a' radians ccw around the origin
    P rotate(double a) const
    {
        return P(x * cos(a) - y * sin(a), x * sin(a) + y * cos(a));
    }
    friend ostream &operator<<(ostream &os, P p)
    {
        return os << "(" << p.x << "," << p.y << ")";
    }
};

template <class P>
int sideOf(P s, P e, P p) { return sgn(s.cross(e, p)); }

template <class P>
int sideOf(const P &s, const P &e, const P &p, double eps)
{
    auto a = (e - s).cross(p - s);
    double l = (e - s).dist() * eps;
    return (a > l) - (a < -l);
}

template <class P>
bool onSegment(P s, P e, P p)
{
    return p.cross(s, e) == 0 && (s - p).dot(e - p) <= 0;
}

template <class P>
bool inPolygon(vector<P> &p, P a, bool strict = true)
{
    int cnt = 0, n = sz(p);
    rep(i, 0, n)
    {
        P q = p[(i + 1) % n];
        if (onSegment(p[i], q, a))
            return !strict;
        // or : i f ( segDist (p [ i ] , q , a) <= eps ) return ! s t r i c t ;
        cnt ^= ((a.y < p[i].y) - (a.y < q.y)) * a.cross(p[i], q) > 0;
    }
    return cnt;
}

typedef Point<int> P;

bool strictCrossing(P u, P v, P a, P b)
{
    bool u_vert = (u.x == v.x);
    bool a_vert = (a.x == b.x);

    if (u_vert == a_vert)
        return false;

    P v1 = u, v2 = v, h1 = a, h2 = b;
    if (!u_vert)
    {
        swap(v1, h1);
        swap(v2, h2);
    }

    int vx = v1.x;
    int vy_min = min(v1.y, v2.y);
    int vy_max = max(v1.y, v2.y);

    int hy = h1.y;
    int hx_min = min(h1.x, h2.x);
    int hx_max = max(h1.x, h2.x);

    return (vx > hx_min && vx < hx_max) && (hy > vy_min && hy < vy_max);
}

bool intersects(const vector<P> &poly, P u, P v)
{
    int n = sz(poly);
    for (int i = 0; i < n; ++i)
    {
        if (strictCrossing(u, v, poly[i], poly[(i + 1) % n]))
            return true;
    }
    return false;
}

signed main()
{
    ifstream inputFile("day9.in");
    string line;
    int n = 0;
    vector<P> points;
    while (getline(inputFile, line))
    {
        istringstream iss(line);
        int x, y;
        char c;
        iss >> x >> c >> y;
        points.emplace_back(x, y);
        n++;
    }

    int ans = 0;
    rep(i, 0, n) rep(j, i + 1, n)
    {
        P p1 = points[i];
        P p2 = points[j];
        P p3(p1.x, p2.y);
        P p4(p2.x, p1.y);
        if (!inPolygon(points, p3, false) || !inPolygon(points, p4, false))
        {
            continue;
        }
        if (!intersects(points, p1, p3) && !intersects(points, p3, p2) &&
            !intersects(points, p2, p4) && !intersects(points, p4, p1))
        {
            ans = max(ans, (abs(p1.x - p2.x) + 1) * (abs(p1.y - p2.y) + 1));
        }
    }
    cout << "Part 2: " << ans << endl;
}