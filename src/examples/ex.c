#include <stdio.h>
#include <stdlib.h>

unsigned long fib_it(unsigned long n){
    if(n < 2)
        return n;
    unsigned long prev = 1;
    unsigned long prevprev = 0;
    for(unsigned long i = 2; i <= n; i++)
    {
        unsigned long tmp = prevprev;
        prevprev = prev;
        prev += tmp;
    }
    return prev;
}

unsigned long fib_rec(unsigned long n){
    if(n == 0)
        return 0;
    if(n == 1)
        return 1;
    return fib_rec(n - 1) + fib_rec(n - 2);
}

int main(){
    return 0;
}
