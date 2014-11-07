[-0.5, 0.5] x;
[0, 5] u;
[1, 2] a;

loop(state: x; control: u)
{
    u = -a*x;
    x = x^3 + u;
}
@L: b*x^2
